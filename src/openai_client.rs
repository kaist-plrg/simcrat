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

    pub fn rename(&self, name: &str) -> String {
        let m1 = system("You are a helpful assistant.");
        let prompt = format!("The name of a C function is `{}`. What would be its name if it was written in Rust? Give only the name without any explanation.", name);
        let m2 = user(&prompt);
        let msgs = vec![m1, m2];
        let tokens = num_tokens(&msgs);
        let request = CreateChatCompletionRequestArgs::default()
            .model(MODEL)
            .messages(msgs)
            .max_tokens(4096 - tokens)
            .temperature(0f32)
            .build()
            .unwrap();
        let response = self
            .runtime
            .block_on(self.inner.chat().create(request))
            .unwrap();
        // println!("{}", prompt);
        println!("{} {:?}", tokens, response.usage);
        let result = &response.choices[0].message.content;
        result
            .replace('`', "")
            .replace('.', "")
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
1. [signature]
2. [signature]
3. [signature]
4. [signature]
5. [signature]
where each signature looks like `fn {1}(...);` or `fn {1}(...) -> ...;`.", code, new_name);
        let m2 = user(&prompt);
        let msgs = vec![m1, m2];
        let tokens = num_tokens(&msgs);
        let request = CreateChatCompletionRequestArgs::default()
            .model(MODEL)
            .messages(msgs)
            .max_tokens(4096 - tokens)
            .temperature(0f32)
            .build()
            .unwrap();
        let response = self
            .runtime
            .block_on(self.inner.chat().create(request))
            .unwrap();
        // println!("{}", prompt);
        println!("{} {:?}", tokens, response.usage);
        response.choices[0]
            .message
            .content
            .split('\n')
            .map(|l| l.replace('`', "").trim().to_string())
            .filter(|l| l.starts_with(|c: char| c.is_ascii_digit()))
            .map(|l| {
                (if let Some(i) = l.find(';') {
                    &l[3..i]
                } else {
                    &l[3..]
                })
                .to_string()
            })
            .collect()
    }

    pub fn translate_function(&self, code: &str, signature: &str) {
        let m1 = system("You are a helpful assistant that translates C to Rust.");
        let prompt = format!(
            "Translate the following C function to Rust using Rust idioms without any explanation:
```
{}
```
Your answer must start with:
```
{} {{
```",
            code, signature
        );
        let m2 = user(&prompt);
        let msgs = vec![m1, m2];
        let tokens = num_tokens(&msgs);
        let request = CreateChatCompletionRequestArgs::default()
            .model(MODEL)
            .messages(msgs)
            .max_tokens(4096 - tokens)
            .temperature(0f32)
            .stop("\n}")
            .build()
            .unwrap();
        let response = self
            .runtime
            .block_on(self.inner.chat().create(request))
            .unwrap();
        // println!("{}", prompt);
        println!("{} {:?}", tokens, response.usage);
        println!("SIG: {}", signature);
        println!(
            "RES:\n{}\n}}\n////////////////////////////////////////",
            response.choices[0].message.content.replace("```\n", "")
        );
    }
}

fn num_tokens(msgs: &[ChatCompletionRequestMessage]) -> u16 {
    let bpe = tiktoken_rs::cl100k_base().unwrap();
    let count = |s: &str| bpe.encode_with_special_tokens(s).len() as u16;
    let mut num_tokens = 2;
    for msg in msgs {
        let role = if matches!(msg.role, Role::System) {
            "system"
        } else {
            "user"
        };
        num_tokens += 4 + count(&role) + count(&msg.content);
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
