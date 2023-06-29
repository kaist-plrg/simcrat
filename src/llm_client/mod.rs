use async_trait::async_trait;
use lazy_static::lazy_static;

pub mod cache;
pub mod openai;

lazy_static! {
    static ref BPE: tiktoken_rs::CoreBPE = tiktoken_rs::cl100k_base().unwrap();
}

pub fn tokens_in_str(s: &str) -> usize {
    BPE.encode_with_special_tokens(s).len()
}

#[async_trait]
pub trait LanguageModel {
    fn request_tokens(&self) -> usize;
    fn response_tokens(&self) -> usize;
    fn response_time(&self) -> f32;

    async fn rename_type(&self, name: &str) -> String;
    async fn translate_type(&self, code: &str, sort: &str, deps: &[String]) -> Option<String>;
    async fn rename_variable(&self, name: &str) -> String;
    async fn translate_variable(&self, code: &str, deps: &[String]) -> Option<String>;
    async fn rename_function(&self, name: &str) -> String;
    async fn translate_signature(
        &self,
        code: &str,
        new_name: &str,
        deps: &[String],
        n: usize,
    ) -> Vec<String>;
    async fn translate_function(
        &self,
        code: &str,
        signature: Option<&str>,
        deps: &[String],
    ) -> Option<String>;
    async fn fix(&self, code: &str, error: &str) -> Option<String>;
    async fn compare(&self, code1: &str, code2: &str) -> std::cmp::Ordering;
}
