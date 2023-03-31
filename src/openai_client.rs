use std::{
    cell::{Cell, RefCell},
    collections::BTreeMap,
    fs::{self, File},
};

use async_openai::{types::*, Client};
use tokio::runtime::{Builder, Runtime};

type CacheKey = (Vec<(String, String)>, Option<String>);

struct Cache {
    cache_file: Option<String>,
    map: RefCell<BTreeMap<CacheKey, String>>,
    updated: Cell<bool>,
}

impl Cache {
    fn new(cache_file: Option<String>) -> Self {
        let v: Vec<(CacheKey, String)> = if let Some(cache_file) = &cache_file {
            if let Ok(cache_file) = File::open(cache_file) {
                serde_json::from_reader(cache_file).unwrap()
            } else {
                vec![]
            }
        } else {
            vec![]
        };
        let map = v.into_iter().collect();
        Self {
            cache_file,
            map: RefCell::new(map),
            updated: Cell::new(false),
        }
    }

    fn get(&self, key: &CacheKey) -> Option<String> {
        self.map.borrow().get(key).cloned()
    }

    fn insert(&self, key: CacheKey, value: String) {
        if self.cache_file.is_some() {
            self.updated.set(true);
            self.map.borrow_mut().insert(key, value);
        }
    }

    fn save(&self) {
        if let Some(cache_file) = &self.cache_file {
            if self.updated.get() {
                let mut file = File::create(cache_file).unwrap();
                let v: Vec<_> = self
                    .map
                    .borrow()
                    .iter()
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect();
                serde_json::to_writer(&mut file, &v).unwrap();
                self.updated.set(false);
            }
        }
    }
}

pub struct OpenAIClient {
    inner: Client,
    runtime: Runtime,
    cache: Cache,
}

const MODEL: &str = "gpt-3.5-turbo-0301";

impl OpenAIClient {
    pub fn new(api_key_file: &str, cache_file: Option<String>) -> Self {
        let api_key = fs::read_to_string(api_key_file).unwrap().trim().to_string();
        let inner = Client::new().with_api_key(api_key);
        let runtime = Builder::new_current_thread().enable_all().build().unwrap();
        let cache = Cache::new(cache_file);
        Self {
            inner,
            runtime,
            cache,
        }
    }

    pub fn translate_global_variable(&self, code: &str) -> String {
        let m1 = system("You are a helpful assistant that translates C to Rust.");
        let prompt = format!(
            "Translate the following C global variable declaration to Rust without any explanation:
```
{}
```",
            code
        );
        let m2 = user(&prompt);
        let msgs = vec![m1, m2];
        let result = self.send_request(msgs, None);
        extract_code(result)
    }

    pub fn rename(&self, name: &str) -> String {
        let m1 = system("You are a helpful assistant.");
        let prompt = format!("The name of a C function is `{}`. What would be its name if it was written in Rust? Give only the name without any explanation.", name);
        let m2 = user(&prompt);
        let msgs = vec![m1, m2];
        let result = self.send_request(msgs, None);
        result
            .replace(['`', '.'], "")
            .split(' ')
            .next()
            .unwrap()
            .to_string()
    }

    pub fn translate_signature(&self, code: &str, new_name: &str, n: usize) -> Vec<String> {
        assert!((1..=10).contains(&n));
        let m1 = system("You are a helpful assistant.");
        let sigs: String = (1..=n).map(|i| format!("{}. `signature`\n", i)).collect();
        let prompt = format!(
            "Consider the following C function:
```
{}
```
If this function was written in Rust with Rust idioms, what would be its signature? Give {} candidate signature{} without any explanation.
Your answer looks like
{}where each signature looks like `fn {4}(...);` or `fn {4}(...) -> ...;`.",
            code,
            n,
            if n == 1 { "" } else { "s" },
            sigs,
            new_name
        );
        let m2 = user(&prompt);
        let msgs = vec![m1, m2];
        let result = self.send_request(msgs, None);
        let sigs: Vec<_> = result
            .split('\n')
            .filter_map(|s| {
                if !s.starts_with(['1', '2', '3', '4', '5', '6', '7', '8', '9']) {
                    return None;
                }
                let i = s.find('`')?;
                let s = &s[i + 1..];
                let i = s.find('`')?;
                let s = s[..i].trim();
                let s = if let Some(s) = s.strip_suffix(';') {
                    s
                } else {
                    s
                };
                Some(s.to_string() + " {}")
            })
            .collect();
        assert_eq!(sigs.len(), n, "{}", result);
        sigs
    }

    pub fn translate_function(
        &self,
        code: &str,
        signature: &str,
        globs: &Vec<&str>,
        callees: &Vec<&str>,
    ) -> String {
        let m1 = system("You are a helpful assistant that translates C to Rust.");
        let globs = if globs.is_empty() {
            "".to_string()
        } else {
            format!(
                "The following global variables exist:
```
{}
```
",
                globs.join("\n")
            )
        };
        let callees = if callees.is_empty() {
            "".to_string()
        } else {
            format!(
                "The following functions exist:
```
{}
```
",
                callees.join("\n")
            )
        };
        let prompt = format!(
            "{}{}Translate the following C function to Rust using Rust idioms without any explanation:
```
{}
```
Your answer must start with:
```
{} {{
```
Try to avoid unsafe code.",
            globs, callees, code, signature.strip_suffix(" {}").unwrap()
        );
        let m2 = user(&prompt);
        let msgs = vec![m1, m2];
        let result = self.send_request(msgs, Some("\n}"));
        let pat1 = "```rust\n";
        let pat2 = "```\n";
        let result = if let Some(i) = result.find(pat1) {
            &result[i + pat1.len()..]
        } else {
            let i = result.find(pat2).unwrap();
            &result[i + pat2.len()..]
        };
        result.to_string() + "\n}"
    }

    pub fn fix(&self, code: &str, error: &str) -> String {
        let m1 = system("You are a helpful assistant.");
        let prompt = format!(
            "The following Rust code has a compilation error:
```
{}
```
The error message is:
```
{}
```
Fix the code without any explanation.",
            code, error
        );
        let m2 = user(&prompt);
        let msgs = vec![m1, m2];
        let result = self.send_request(msgs, None);
        extract_code(result)
    }

    pub fn compare(&self, code1: &str, code2: &str) -> std::cmp::Ordering {
        let m1 = system("You are a helpful assistant.");
        let prompt = format!(
            "Which of the following two Rust functions is more Rust-idiomatic?
Implementation 1:
```
{}
```
Implementation 2:
```
{}
```
Your answer must follow the following format:
Implementation [n]
[reason]",
            code1, code2
        );
        let m2 = user(&prompt);
        let msgs = vec![m1, m2];
        let result = self.send_request(msgs, Some("\n"));
        let c = result.chars().find(|&c| c == '1' || c == '2').unwrap();
        match c {
            '1' => std::cmp::Ordering::Greater,
            '2' => std::cmp::Ordering::Less,
            _ => unreachable!(),
        }
    }

    fn send_request(&self, msgs: Vec<ChatCompletionRequestMessage>, stop: Option<&str>) -> String {
        tracing::info!("send_request");
        for msg in &msgs {
            tracing::info!("{}\n{}", msg.role, msg.content);
        }

        let key = (
            msgs.iter()
                .map(|ChatCompletionRequestMessage { role, content, .. }| {
                    (role_to_str(role).to_string(), content.clone())
                })
                .collect::<Vec<_>>(),
            stop.map(|s| s.to_string()),
        );
        if let Some(result) = self.cache.get(&key) {
            tracing::info!("cache hit");
            tracing::info!("{}", result);
            return result;
        }

        let tokens = num_tokens(&msgs);
        let mut request = CreateChatCompletionRequestArgs::default();
        request
            .model(MODEL)
            .messages(msgs)
            .max_tokens(4096 - tokens)
            .temperature(0f32);
        if let Some(stop) = stop {
            request.stop(stop);
        }
        let request = request.build().unwrap();
        let response = self
            .runtime
            .block_on(self.inner.chat().create(request))
            .unwrap();
        assert_eq!(tokens as u32, response.usage.unwrap().prompt_tokens);

        let result = response.choices[0].message.content.clone();
        tracing::info!("{}", result);
        self.cache.insert(key, result.clone());
        self.cache.save();
        result
    }
}

fn extract_code(result: String) -> String {
    let pat1 = "```rust\n";
    let pat2 = "```\n";
    let result = if let Some(i) = result.find(pat1) {
        &result[i + pat1.len()..]
    } else {
        let i = result.find(pat2).unwrap();
        &result[i + pat2.len()..]
    };
    let pat = "\n```";
    if let Some(i) = result.find(pat) {
        &result[..i]
    } else {
        result
    }
    .to_string()
}

fn role_to_str(role: &Role) -> &'static str {
    match role {
        Role::System => "system",
        Role::User => "user",
        Role::Assistant => "assistant",
    }
}

fn num_tokens(msgs: &[ChatCompletionRequestMessage]) -> u16 {
    let bpe = tiktoken_rs::cl100k_base().unwrap();
    let count = |s: &str| bpe.encode_with_special_tokens(s).len() as u16;
    let mut num_tokens = 3;
    for msg in msgs {
        let role = role_to_str(&msg.role);
        num_tokens += 4 + count(role) + count(&msg.content);
    }
    num_tokens
}

fn system(s: &str) -> ChatCompletionRequestMessage {
    ChatCompletionRequestMessage {
        role: Role::System,
        content: s.to_string(),
        name: None,
    }
}

fn user(s: &str) -> ChatCompletionRequestMessage {
    ChatCompletionRequestMessage {
        role: Role::User,
        content: s.to_string(),
        name: None,
    }
}
