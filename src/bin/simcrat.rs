use std::fs::File;

use clap::Parser;
use simcrat::*;

#[derive(Parser, Debug)]
struct Args {
    #[arg(short, long)]
    input: String,
    #[arg(short, long)]
    log_file: Option<String>,
    #[arg(short, long)]
    api_key_file: Option<String>,
    #[arg(short, long)]
    cache_file: Option<String>,
}

fn main() {
    let args = Args::parse();

    if let Some(log) = args.log_file {
        let log_file = File::create(log).unwrap();
        tracing_subscriber::fmt()
            .with_max_level(tracing::Level::INFO)
            .with_ansi(false)
            .with_writer(log_file)
            .init();
    }

    let parsed = c_parser::parse(args.input);
    let api_key = args.api_key_file.unwrap_or(".openai_api_key".to_string());
    let client = openai_client::OpenAIClient::new(&api_key, args.cache_file);
    let mut translator = translation::Translator::new(&parsed, client);
    translator.translate_variables();
    translator.translate_functions();
    println!("{}", translator.whole_code());
}
