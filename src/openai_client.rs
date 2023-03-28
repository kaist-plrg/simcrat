use std::fs;

use async_openai::{types::*, Client};
use tokio::runtime::{Builder, Runtime};

pub struct OpenAIClient {
    inner: Client,
    runtime: Runtime,
}

const MODEL: &str = "gpt-3.5-turbo-0301";

impl OpenAIClient {
    pub fn new(api_key_file: &str) -> Self {
        let api_key = fs::read_to_string(api_key_file).unwrap().trim().to_string();
        let inner = Client::new().with_api_key(api_key);
        let runtime = Builder::new_current_thread().enable_all().build().unwrap();
        Self { inner, runtime }
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
        let pat1 = "```rust\n";
        let pat2 = "```\n";
        let result = if let Some(i) = result.find(pat1) {
            &result[i + pat1.len()..]
        } else {
            let i = result.find(pat2).unwrap();
            &result[i + pat2.len()..]
        };
        let pat = "\n```";
        let i = result.find(pat).unwrap();
        result[..i].to_string()
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

    pub fn translate_signature(&self, code: &str, new_name: &str) -> Vec<String> {
        let m1 = system("You are a helpful assistant.");
        let prompt = format!("Consider the following C function:
```
{}
```
If this function was written in Rust with Rust idioms, what would be its signature? Give 5 candidate signatures without any explanation.
Your answer looks like
1. `signature`
2. `signature`
3. `signature`
4. `signature`
5. `signature`
where each signature looks like `fn {1}(...);` or `fn {1}(...) -> ...;`.", code, new_name);
        let m2 = user(&prompt);
        let msgs = vec![m1, m2];
        let result = self.send_request(msgs, None);
        let sigs: Vec<_> = result
            .split('\n')
            .filter_map(|s| {
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
        assert_eq!(sigs.len(), 5, "{}", result);
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
            globs, callees, code, signature
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

    fn send_request(&self, msgs: Vec<ChatCompletionRequestMessage>, stop: Option<&str>) -> String {
        tracing::info!("send_request");
        for msg in &msgs {
            tracing::info!("{}\n{}", msg.role, msg.content);
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
        result
    }
}

fn num_tokens(msgs: &[ChatCompletionRequestMessage]) -> u16 {
    let bpe = tiktoken_rs::cl100k_base().unwrap();
    let count = |s: &str| bpe.encode_with_special_tokens(s).len() as u16;
    let mut num_tokens = 3;
    for msg in msgs {
        let role = if matches!(msg.role, Role::System) {
            "system"
        } else {
            "user"
        };
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
