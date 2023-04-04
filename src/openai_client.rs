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

    pub fn rename_type(&self, name: &str) -> String {
        if name.chars().next().unwrap().is_uppercase() && !name.contains('_') {
            return name.to_string();
        }
        let m1 = system("You are a helpful assistant. Answer as concisely as possible.");
        let m2 = user("Convert `foo` to `CamelCase`.");
        let m3 = assistant("`Foo`");
        let m4 = user("Convert `Bar` to `CamelCase`.");
        let m5 = assistant("`Bar`");
        let m6 = user("Convert `foo_bar` to `CamelCase`.");
        let m7 = assistant("`FooBar`");
        let m8 = user("Convert `barBaz` to `CamelCase`.");
        let m9 = assistant("`BarBaz`");
        let prompt = format!("Convert `{}` to `CamelCase`.", name);
        let m10 = user(&prompt);
        let msgs = vec![m1, m2, m3, m4, m5, m6, m7, m8, m9, m10];
        let result = self.send_request(msgs, None);
        extract_name(result)
    }

    pub fn translate_type(&self, code: &str, sort: &str, deps: &[&str]) -> String {
        let m1 = system("You are a helpful assistant that translates C to Rust.");
        let deps = if deps.is_empty() {
            "".to_string()
        } else {
            format!(
                "The following type{} been translated from C to Rust already:
```
{}
```
",
                if deps.len() == 1 { " has" } else { "s have" },
                deps.join("\n")
            )
        };
        let prompt = format!(
            "{}Translate the following C {} definition to Rust using Rust idioms without any explanation:
```
{}
```
Try to avoid unsafe code.",
            deps, sort, code
        );
        let m2 = user(&prompt);
        let msgs = vec![m1, m2];
        let result = self.send_request(msgs, None);
        extract_code(result)
    }

    pub fn rename_variable(&self, name: &str) -> String {
        if !name.contains(|c: char| c.is_lowercase()) {
            return name.to_string();
        }
        let m1 = system("You are a helpful assistant. Answer as concisely as possible.");
        let m2 = user("Convert `Foo` to `SCREAMING_SNAKE_CASE`.");
        let m3 = assistant("`FOO`");
        let m4 = user("Convert `BAR` to `SCREAMING_SNAKE_CASE`.");
        let m5 = assistant("`BAR`");
        let m6 = user("Convert `foo_bar` to `SCREAMING_SNAKE_CASE`.");
        let m7 = assistant("`FOO_BAR`");
        let m8 = user("Convert `barBaz` to `SCREAMING_SNAKE_CASE`.");
        let m9 = assistant("`BAR_BAZ`");
        let prompt = format!("Convert `{}` to `SCREAMING_SNAKE_CASE`.", name);
        let m10 = user(&prompt);
        let msgs = vec![m1, m2, m3, m4, m5, m6, m7, m8, m9, m10];
        let result = self.send_request(msgs, None);
        extract_name(result)
    }

    pub fn translate_variable(&self, code: &str, deps: &[&str]) -> String {
        let m1 = system("You are a helpful assistant that translates C to Rust.");
        let deps = if deps.is_empty() {
            "".to_string()
        } else {
            format!(
                "The following definition{} been translated from C to Rust already:
```
{}
```
",
                if deps.len() == 1 { " has" } else { "s have" },
                deps.join("\n")
            )
        };
        let prompt = format!(
            "{}Translate the following C global variable declaration to a Rust global variable declaration without any explanation:
```
{}
```
Try to avoid unsafe code.",
            deps, code
        );
        let m2 = user(&prompt);
        let msgs = vec![m1, m2];
        let result = self.send_request(msgs, None);
        extract_code(result)
    }

    pub fn rename_function(&self, name: &str) -> String {
        if !name.contains(|c: char| c.is_uppercase()) {
            return name.to_string();
        }
        let m1 = system("You are a helpful assistant. Answer as concisely as possible.");
        let m2 = user("Convert `Foo` to `snake_case`.");
        let m3 = assistant("`foo`");
        let m4 = user("Convert `BAR` to `snake_case`.");
        let m5 = assistant("`bar`");
        let m6 = user("Convert `foo_bar` to `snake_case`.");
        let m7 = assistant("`foo_bar`");
        let m8 = user("Convert `barBaz` to `snake_case`.");
        let m9 = assistant("`bar_baz`");
        let prompt = format!("Convert `{}` to `snake_case`.", name);
        let m10 = user(&prompt);
        let msgs = vec![m1, m2, m3, m4, m5, m6, m7, m8, m9, m10];
        let result = self.send_request(msgs, None);
        extract_name(result)
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
        globs: &[&str],
        callees: &[&str],
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
        if let Some(reason) = &response.choices[0].finish_reason {
            tracing::info!("{}", reason);
        }
        self.cache.insert(key, result.clone());
        self.cache.save();
        result
    }
}

fn extract_name(result: String) -> String {
    let i = result.find('`').unwrap();
    let result = &result[i + 1..];
    let i = result.find('`').unwrap();
    result[..i].to_string()
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

fn assistant(s: &str) -> ChatCompletionRequestMessage {
    ChatCompletionRequestMessage {
        role: Role::Assistant,
        content: s.to_string(),
        name: None,
    }
}
