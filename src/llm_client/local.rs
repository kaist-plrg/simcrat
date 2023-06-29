#![allow(unused)]

use std::{
    sync::{atomic::AtomicUsize, Mutex},
    time::Instant,
};

use async_trait::async_trait;
use etrace::some_or;
use reqwest::Client;
use serde::{Deserialize, Serialize};

use super::{
    cache::{Cache, DbConfig, HasElapsed},
    tokens_in_str, LanguageModel,
};

#[derive(Debug, Clone, Serialize, Deserialize)]
struct CacheKey {
    prompt: String,
    new_tokens_once: usize,
    stop: Option<String>,
}

impl CacheKey {
    fn new<S: AsRef<str>>(prompt: &str, new_tokens_once: usize, stop: &Option<S>) -> Self {
        let prompt = prompt.to_string();
        let stop = stop.as_ref().map(|s| s.as_ref().to_string());
        Self {
            prompt,
            new_tokens_once,
            stop,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct CacheVal {
    content: String,
    response_tokens: usize,
    request_tokens: usize,
    elapsed: f32,
}

impl CacheVal {
    fn new(content: String, response_tokens: usize, request_tokens: usize, elapsed: f32) -> Self {
        Self {
            content,
            response_tokens,
            request_tokens,
            elapsed,
        }
    }
}

impl HasElapsed for CacheVal {
    fn elapsed(&self) -> f32 {
        self.elapsed
    }
}

#[derive(Serialize, Deserialize)]
struct GenerationResult {
    result: Option<String>,
}

pub struct LocalClient {
    inner: Client,
    url: String,
    cache: Cache<CacheKey, CacheVal>,

    total_request_tokens: AtomicUsize,
    total_response_tokens: AtomicUsize,
    total_response_time: Mutex<f32>,
}

impl LocalClient {
    pub fn new(url: String, db_conf: DbConfig) -> Self {
        let inner = Client::new();
        let cache = Cache::new(db_conf);
        Self {
            inner,
            url,
            cache,
            total_request_tokens: AtomicUsize::new(0),
            total_response_tokens: AtomicUsize::new(0),
            total_response_time: Mutex::new(0.0),
        }
    }

    async fn send_request(
        &self,
        prompt: &str,
        new_tokens_once: usize,
        stop: Option<&str>,
    ) -> String {
        if tokens_in_str(prompt) > 2047 {
            panic!("{}", prompt);
        }

        let key = CacheKey::new(prompt, new_tokens_once, &stop);
        let (result, hit) = if let Some(result) = self.cache.get(&key).await {
            (result, true)
        } else {
            tracing::info!("send_request START");
            let now = Instant::now();
            let res: GenerationResult = self
                .inner
                .post(&self.url)
                .json(&key)
                .send()
                .await
                .expect(prompt)
                .json()
                .await
                .expect(prompt);
            let elapsed = now.elapsed().as_secs_f32();
            let response = res.result.expect(prompt);
            tracing::info!("send_request DONE ({} seconds)", elapsed);

            let request_tokens = tokens_in_str(prompt);
            let response_tokens = tokens_in_str(&response);
            let val = CacheVal::new(response, request_tokens, response_tokens, elapsed);

            self.cache.insert(key, val.clone()).await;
            (val, false)
        };

        tracing::info!(
            "send_request
cache {}
[prompt]
{}
[response]
{}",
            if hit { "hit" } else { "miss" },
            prompt,
            result.content
        );

        self.total_request_tokens
            .fetch_add(result.request_tokens, std::sync::atomic::Ordering::AcqRel);
        self.total_response_tokens
            .fetch_add(result.response_tokens, std::sync::atomic::Ordering::AcqRel);
        let mut time = self.total_response_time.lock().unwrap();
        *time += result.elapsed;

        result.content
    }
}

const HEADER: &str = "You are an excellent systems programmer skilled in both C and Rust. You complete the given tasks perfectly.";

#[async_trait]
impl LanguageModel for LocalClient {
    fn request_tokens(&self) -> usize {
        self.total_request_tokens
            .load(std::sync::atomic::Ordering::Acquire)
    }

    fn response_tokens(&self) -> usize {
        self.total_response_tokens
            .load(std::sync::atomic::Ordering::Acquire)
    }

    fn response_time(&self) -> f32 {
        *self.total_response_time.lock().unwrap()
    }

    async fn rename_type(&self, name: &str) -> String {
        if name.chars().next().unwrap().is_uppercase()
            && !name.contains('_')
            && name.contains(|c: char| c.is_lowercase())
        {
            return name.to_string();
        }
        fn convert(old: &str) -> String {
            format!("Convert `{}` to `CamelCase`.", old)
        }
        let prompt = make_prompt(
            Some("`CamelCase` is a writing convention where multiple words are combined together without underscores, and each word starts with a capital letter."),
            &[
                (convert("foo"), "`Foo`".to_string()),
                (convert("Bar"), "`Bar`".to_string()),
                (convert("foo_bar"), "`FooBar`".to_string()),
                (convert("barBaz"), "`BarBaz`".to_string()),
                (convert(name), "`".to_string()),
            ],
        );
        let res = self
            .send_request(&prompt, tokens_in_str(name) * 2, Some("`"))
            .await;
        let i = res.find('`').unwrap();
        res[..i].to_string()
    }

    async fn translate_type(&self, code: &str, sort: &str, deps: &[String]) -> Option<String> {
        fn task(code: &str, sort: &str, deps: &[String]) -> String {
            format!(
                "{}Translate the following C {} definition to Rust using Rust idioms:
```c
{}
```
Try to avoid unsafe code.",
                make_deps(deps),
                sort,
                code
            )
        }
        fn answer(code: &str, close: bool) -> String {
            format!(
                "This is the equivalent Rust definition:
```rust
{}{}",
                code,
                if close { "\n```" } else { "" }
            )
        }
        let prompt = make_prompt(
            Some(HEADER),
            &[
                (
                    task("typedef int MyInt;", "type", &[]),
                    answer("type MyInt = i32;", true),
                ),
                (
                    task(
                        "struct Point {
    int x;
    int y;
};",
                        "struct",
                        &[],
                    ),
                    answer(
                        "struct Point {
    x: i32,
    y: i32,
}",
                        true,
                    ),
                ),
                (task(code, sort, deps), answer("", false)),
            ],
        );
        let res = self
            .send_request(&prompt, tokens_in_str(code) * 2, Some("```"))
            .await;
        let i = res.find("```")?;
        Some(res[..i].to_string())
    }

    async fn rename_variable(&self, name: &str) -> String {
        if !name.contains(|c: char| c.is_lowercase()) {
            return name.to_string();
        }
        fn convert(old: &str) -> String {
            format!("Convert `{}` to `SCREAMING_SNAKE_CASE`.", old)
        }
        let prompt = make_prompt(
            Some("`SCREAMING_SNAKE_CASE` is a writing convention where multiple words are combined together with underscores, and all letters are in uppercase."),
            &[
                (convert("Foo"), "`FOO`".to_string()),
                (convert("BAR"), "`BAR`".to_string()),
                (convert("foo_bar"), "`FOO_BAR`".to_string()),
                (convert("barBaz"), "`BAR_BAZ`".to_string()),
                (convert(name), "`".to_string()),
            ],
        );
        let res = self
            .send_request(&prompt, tokens_in_str(name) * 2, Some("`"))
            .await;
        let i = res.find('`').unwrap();
        res[..i].to_string()
    }

    async fn translate_variable(&self, code: &str, deps: &[String]) -> Option<String> {
        fn task(code: &str, deps: &[String]) -> String {
            format!(
                "{}Translate the following C global variable declaration to a Rust global variable declaration:
```c
{}
```
Try to avoid unsafe code.",
                make_deps(deps),
                code
            )
        }
        fn answer(code: &str, close: bool) -> String {
            format!(
                "This is the equivalent Rust definition:
```rust
{}{}",
                code,
                if close { "\n```" } else { "" }
            )
        }
        let prompt = make_prompt(
            Some(HEADER),
            &[
                (
                    task("int MAX_VALUE = 10;", &[]),
                    answer("const MAX_VALUE: i32 = 10;", true),
                ),
                (
                    task("char **MSGS = { \"hi\", \"hello\" };", &[]),
                    answer("static MSGS: [&str; 2] = [\"hi\", \"hello\"];", true),
                ),
                (task(code, deps), answer("", false)),
            ],
        );
        let res = self
            .send_request(&prompt, tokens_in_str(code) * 2, Some("```"))
            .await;
        let i = res.find("```")?;
        Some(res[..i].to_string())
    }

    async fn rename_function(&self, name: &str) -> String {
        if !name.contains(|c: char| c.is_uppercase()) {
            return name.to_string();
        }
        fn convert(old: &str) -> String {
            format!("Convert `{}` to `snake_case`.", old)
        }
        let prompt = make_prompt(
            Some("`snake_case` is a writing convention where multiple words are combined together with underscores, and all letters are in lowercase."),
            &[
                (convert("Foo"), "`foo`".to_string()),
                (convert("BAR"), "`bar`".to_string()),
                (convert("foo_bar"), "`foo_bar`".to_string()),
                (convert("barBaz"), "`bar_baz`".to_string()),
                (convert(name), "`".to_string()),
            ],
        );
        let res = self
            .send_request(&prompt, tokens_in_str(name) * 2, Some("`"))
            .await;
        let i = res.find('`').unwrap();
        res[..i].to_string()
    }

    async fn translate_signature(
        &self,
        code: &str,
        new_name: &str,
        deps: &[String],
        n: usize,
    ) -> Vec<String> {
        fn task(code: &str, deps: &[String], n: usize) -> String {
            format!(
                "{}Consider the following C function:
```c
{}
```
If this function was written in Rust with Rust idioms, what would be its signature?
Give {} Rust-idiomatic candidate signatures.",
                make_deps(deps),
                code,
                n
            )
        }
        fn answer(sigs: &str) -> String {
            format!("These are possible signatures in Rust:\n{}", sigs)
        }
        let ex1 = (
            task(
                "int hello() {
    if (NAME == NULL) {
        return 1;
    }
    printf(\"Hello %s!\\n\", NAME);
    return 0;
}",
                &["const NAME: &str;".to_string()],
                3,
            ),
            answer(
                "1. `fn hello() -> i32;`
2. `fn hello() -> Option<()>;`
3. `fn hello() -> Result<(), ()>;`",
            ),
        );
        let ex2 = (
            task(
                "int divide(int n, int d, int *q, int *r) {
    if (d == 0) {
        return -1;
    }
    *q = n / d;
    *r = n % d;
    return 0;
}",
                &[],
                3,
            ),
            answer(
                "1. `fn divide(n: i32, d: i32, q: &mut i32, r: &mut i32) -> i32;`
2. `fn divide(n: i32, d: i32) -> Option<(i32, i32)>;`
3. `fn divide(n: i32, d: i32) -> Result<(i32, i32), ()>;`",
            ),
        );
        let mut sigs = vec![];
        while sigs.len() < n {
            let mut sigs_str = sigs
                .iter()
                .enumerate()
                .map(|(i, sig)| format!("{}. `{};`", i + 1, sig))
                .collect::<Vec<_>>()
                .join("\n");
            sigs_str += format!("\n{}. `fn {}", sigs.len() + 1, new_name).as_str();
            let prompt = make_prompt(
                Some(HEADER),
                &[
                    ex1.clone(),
                    ex2.clone(),
                    (task(code, deps, n), answer(sigs_str.as_str())),
                ],
            );
            let res = self.send_request(&prompt, 32, Some("`")).await;
            let i = res.find('`').unwrap();
            let sig = res[..i].to_string();
            if let Some(sig) = sig.strip_suffix(';') {
                sigs.push(format!("fn {}{}", new_name, sig));
            } else {
                sigs.push(format!("fn {}()", new_name));
            }
        }
        sigs
    }

    async fn translate_function(
        &self,
        code: &str,
        signature: Option<&str>,
        deps: &[String],
    ) -> Option<String> {
        todo!()
    }

    async fn fix(&self, code: &str, error: &str) -> Option<String> {
        todo!()
    }

    async fn compare(&self, code1: &str, code2: &str) -> std::cmp::Ordering {
        todo!()
    }
}

fn make_prompt<S: AsRef<str>>(header: Option<&str>, tasks: &[(S, S)]) -> String {
    let mut prompt = String::new();
    if let Some(header) = header {
        prompt.push_str(header);
        prompt.push_str("\n\n");
    }
    for (i, (task, answer)) in tasks.iter().enumerate() {
        prompt.push_str("Task: ");
        prompt.push_str(task.as_ref());
        prompt.push_str("\nAnswer: ");
        prompt.push_str(answer.as_ref());
        if i < tasks.len() - 1 {
            prompt.push_str("\n\n");
        }
    }
    prompt
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
