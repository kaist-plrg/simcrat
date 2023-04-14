use std::{
    collections::BTreeMap,
    fs::{self, File},
    sync::{
        atomic::{AtomicBool, Ordering},
        RwLock,
    },
};

use async_openai::{types::*, Client};
use etrace::some_or;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
struct CacheKey {
    messages: Vec<(String, String)>,
    stop: Option<String>,
}

impl CacheKey {
    fn new<S: AsRef<str>>(messages: &[ChatCompletionRequestMessage], stop: &Option<S>) -> Self {
        let messages = messages
            .iter()
            .map(|ChatCompletionRequestMessage { role, content, .. }| {
                (role_to_str(role).to_string(), content.clone())
            })
            .collect();
        let stop = stop.as_ref().map(|s| s.as_ref().to_string());
        Self { messages, stop }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct CacheVal {
    content: String,
    reason: Option<String>,
}

impl CacheVal {
    fn new(content: String, reason: Option<String>) -> Self {
        Self { content, reason }
    }

    fn is_too_long(&self) -> bool {
        self.reason.as_ref().map(|s| s == "length").unwrap_or(false)
    }
}

struct Cache {
    cache_file: Option<String>,
    map: RwLock<BTreeMap<CacheKey, CacheVal>>,
    updated: AtomicBool,
}

impl Cache {
    fn new(cache_file: Option<String>) -> Self {
        let v: Vec<_> = if let Some(cache_file) = &cache_file {
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
            map: RwLock::new(map),
            updated: AtomicBool::new(false),
        }
    }

    fn get(&self, key: &CacheKey) -> Option<CacheVal> {
        self.map.read().unwrap().get(key).cloned()
    }

    fn insert(&self, key: CacheKey, value: CacheVal) {
        if self.cache_file.is_some() {
            self.updated.store(true, Ordering::Release);
            self.map.write().unwrap().insert(key, value);
        }
    }

    fn save(&self) {
        if let Some(cache_file) = &self.cache_file {
            if self.updated.load(Ordering::Acquire) {
                let mut file = File::create(cache_file).unwrap();
                let v: Vec<_> = self
                    .map
                    .read()
                    .unwrap()
                    .iter()
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect();
                serde_json::to_writer(&mut file, &v).unwrap();
                self.updated.store(false, Ordering::Release);
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum OpenAIError {
    TooLong,
    NoAnswer,
}

pub struct OpenAIClient {
    inner: Client,
    cache: Cache,
}

const MODEL: &str = "gpt-3.5-turbo-0301";

impl OpenAIClient {
    pub fn new(api_key_file: &str, cache_file: Option<String>) -> Self {
        let api_key = fs::read_to_string(api_key_file).unwrap().trim().to_string();
        let inner = Client::new().with_api_key(api_key);
        let cache = Cache::new(cache_file);
        Self { inner, cache }
    }

    pub async fn rename_type(&self, name: &str) -> String {
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
        extract_name(result.await.unwrap())
    }

    pub async fn translate_type(&self, code: &str, sort: &str, deps: &[String]) -> String {
        let m1 = system("You are a helpful assistant that translates C to Rust.");
        let deps = make_deps(deps);
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
        extract_code(result.await.unwrap()).unwrap()
    }

    pub async fn rename_variable(&self, name: &str) -> String {
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
        extract_name(result.await.unwrap())
    }

    pub async fn translate_variable(
        &self,
        code: &str,
        deps: &[String],
    ) -> Result<String, OpenAIError> {
        let m1 = system("You are a helpful assistant that translates C to Rust.");
        let deps = make_deps(deps);
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
        let result = result.await.ok_or(OpenAIError::TooLong)?;
        extract_code(result).ok_or(OpenAIError::NoAnswer)
    }

    pub async fn rename_function(&self, name: &str) -> String {
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
        extract_name(result.await.unwrap())
    }

    pub async fn translate_signature(
        &self,
        code: &str,
        new_name: &str,
        deps: &[String],
        n: usize,
    ) -> Vec<String> {
        assert!((1..=9).contains(&n));
        let m1 = system("You are a helpful assistant.");
        let m2 = user(&signature_prompt(
            "int hello() {
    if (NAME == NULL) {
        return 1;
    }
    printf(\"Hello %s!\\n\", NAME);
    return 0;
}",
            "hello",
            &["const NAME: &str;".to_string()],
            5,
        ));
        let m3 = assistant(
            "Explanation:
The function checks if the global constant `NAME` is `NULL` and returns `1` if it is. \
Otherwise, it prints a greeting message and returns `0`.
Signatures:
1. `fn hello();`
2. `fn hello() -> i32;`
3. `fn hello() -> isize;`
4. `fn hello() -> Option<()>;`
5. `fn hello() -> Result<(), ()>;`",
        );
        let m4 = user(&signature_prompt(
            "int divide(int n, int d, int *q, int *r) {
    if (d == 0) {
        return DIV_BY_ZERO;
    }
    *q = n / d;
    *r = n % d;
    return 0;
}",
            "divide",
            &["const DIV_BY_ZERO: i32;".to_string()],
            5,
        ));
        let m5 = assistant(
            "Explanation:
The function takes in two integers and two pointers to integers. \
It checks if the second integer is zero, and if so, returns an error code. \
Otherwise, it calculates the quotient and remainder of the division of the first integer by the second integer \
and stores them in the memory locations pointed to by the two pointers. \
Finally, it returns zero to indicate success.
Signatures:
1. `fn divide(n: i32, d: i32, q: &mut i32, r: &mut i32) -> i32;`
2. `fn divide(n: i32, d: i32, q: &mut i32, r: &mut i32) -> Result<(), ()>;`
4. `fn divide(n: i32, d: i32) -> Option<(i32, i32)>;`
3. `fn divide(n: i32, d: i32) -> Result<(i32, i32), ()>;`
5. `fn divide(n: i32, d: i32) -> (i32, i32, i32);`"
        );
        let m6 = user(&signature_prompt(code, new_name, deps, n));
        let msgs = vec![m1, m2, m3, m4, m5, m6];
        let result = self.send_request(msgs, None).await.unwrap();
        let sigs: Vec<_> = result
            .split('\n')
            .filter_map(|s| {
                let mut chars = s.chars();
                let c1 = chars.next()?;
                if !('1'..='9').contains(&c1) {
                    return None;
                }
                let c2 = chars.next()?;
                if c2 != '.' {
                    return None;
                }
                let i = s.find('`')?;
                let s = &s[i + 1..];
                let i = s.find('`')?;
                let s = s[..i].trim();
                let s = s.strip_prefix("unsafe ").unwrap_or(s).trim();
                let s = s.strip_suffix(';').unwrap_or(s).trim();
                Some(s.to_string())
            })
            .collect();
        sigs
    }

    pub async fn translate_function(&self, code: &str, signature: &str, deps: &[String]) -> String {
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
            "{}Translate the following C function to Rust using Rust idioms without any explanation:
```
{}
```
Your answer must start with:
```
{} {{
```
Try to avoid unsafe code. Do not add `use` statements. Use full paths instead.",
            deps, code, signature
        );
        let m2 = user(&prompt);
        let msgs = vec![m1, m2];
        let result = self.send_request(msgs, Some("\n}")).await.unwrap();
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

    pub async fn fix(&self, code: &str, error: &str) -> Option<String> {
        let m1 = system("You are a helpful assistant.");
        let instruction = if error.contains("error[E0133]: ") {
            "Write the fixed code by inserting an unsafe block at a proper location."
        } else if error.contains("error[E0425]: ") || error.contains("error[E0433]: ") {
            "Write the fixed code by using a full path to the name. Don't use `use` statements."
        } else {
            "Explain the error first and then write the fixed code."
        };
        let prompt = format!(
            "The following Rust code has a compilation error:
```
{}
```
The error message is:
```
{}
```
{}
",
            code, error, instruction
        );
        let m2 = user(&prompt);
        let msgs = vec![m1, m2];
        let result = self.send_request(msgs, None);
        extract_code(result.await?)
    }

    pub async fn compare(&self, code1: &str, code2: &str) -> std::cmp::Ordering {
        let m1 = system("You are a helpful assistant.");
        let m2 = user(
            "Consider two following Rust functions:
Implementation 1
```
fn div(n: u32, d: u32) -> i32 {
    if d == 0 {
        return -1;
    }
    (n / d) as i32
}
```
Implementation 2
```
fn div(n: u32, d: u32) -> Option<u32> {
    if d == 0 {
        return None;
    }
    Some(n / d)
}
```
Which one is more Rust-idiomatic? Compare them and choose one.
Your answer format is:

Comparison:
[comparison]
Choice: Implementation [n]",
        );
        let m3 = assistant("Comparison:
Both handle the case where the denominator is zero, but they do it differently. Implementation 1 returns -1, which is not a valid result for the division operation, while implementation 2 returns an Option type, which is a more idiomatic way of handling errors in Rust. Additionally, implementation 2 returns an unsigned integer instead of a signed integer, which is more appropriate for the result of a division operation.
Choice: Implementation 2");
        let prompt = format!(
            "Consider two following Rust functions:
Implementation 1
```
{}
```
Implementation 2
```
{}
```
Which one is more Rust-idiomatic? Compare them and choose one.
Your answer format is:

Comparison:
[comparison]
Choice: Implementation [n]",
            code1, code2
        );
        let m4 = user(&prompt);
        let msgs = vec![m1, m2, m3, m4];
        let result = self.send_request(msgs, None).await.unwrap();
        let s = "Choice: Implementation ";
        let i = some_or!(result.find(s), return std::cmp::Ordering::Equal);
        let c = some_or!(
            result[i + s.len()..]
                .chars()
                .find(|&c| c == '1' || c == '2'),
            return std::cmp::Ordering::Equal
        );
        match c {
            '1' => std::cmp::Ordering::Greater,
            '2' => std::cmp::Ordering::Less,
            _ => std::cmp::Ordering::Equal,
        }
    }

    async fn send_request(
        &self,
        msgs: Vec<ChatCompletionRequestMessage>,
        stop: Option<&str>,
    ) -> Option<String> {
        tracing::info!("send_request");
        for msg in &msgs {
            tracing::info!("{}\n{}", msg.role, msg.content);
        }

        let key = CacheKey::new(&msgs, &stop);
        if let Some(result) = self.cache.get(&key) {
            tracing::info!("cache hit");
            tracing::info!("{}", result.content);
            if result.is_too_long() {
                return None;
            } else {
                return Some(result.content);
            }
        }

        let tokens = num_tokens(&msgs);
        let mut request = CreateChatCompletionRequestArgs::default();
        request
            .model(MODEL)
            .messages(msgs)
            .max_tokens(4095 - tokens)
            .temperature(0f32);
        if let Some(stop) = stop {
            request.stop(stop);
        }
        let request = request.build().unwrap();
        let mut response = self.inner.chat().create(request).await.unwrap();
        assert_eq!(tokens as u32, response.usage.unwrap().prompt_tokens);
        assert_eq!(response.choices.len(), 1);

        let choice = response.choices.pop().unwrap();
        let content = choice.message.content;
        let reason = choice.finish_reason;
        let val = CacheVal::new(content.clone(), reason);
        let too_long = val.is_too_long();
        tracing::info!("{}", content);

        self.cache.insert(key, val);
        self.cache.save();

        if too_long {
            None
        } else {
            Some(content)
        }
    }
}

fn make_deps(deps: &[String]) -> String {
    if deps.is_empty() {
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
    }
}

fn signature_prompt(code: &str, new_name: &str, deps: &[String], n: usize) -> String {
    let sigs: String = (1..=n).map(|i| format!("{}. `signature`\n", i)).collect();
    format!(
        "{}Consider the following C function:
```
{}
```
If this function was written in Rust with Rust idioms, what would be its signature?
First, explain the function. Then, give {} Rust-idiomatic candidate signature{}.
Do not add additional parameters to the signatures.
The answer format is:

Explanation:
[explanation]
Signatures:
{}
Each signature must look like `fn {5}(...);` or `fn {5}(...) -> ...;`.",
        make_deps(deps),
        code,
        n,
        if n == 1 { "" } else { "s" },
        sigs,
        new_name
    )
}

fn extract_name(result: String) -> String {
    let i = result.find('`').unwrap();
    let result = &result[i + 1..];
    let i = result.find('`').unwrap();
    result[..i].to_string()
}

fn extract_code(result: String) -> Option<String> {
    let pat1 = "```rust\n";
    let pat2 = "```\n";
    let result = if let Some(i) = result.find(pat1) {
        &result[i + pat1.len()..]
    } else {
        let i = result.find(pat2)?;
        &result[i + pat2.len()..]
    };
    let pat = "\n```";
    let i = result.find(pat)?;
    Some(result[..i].to_string())
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
