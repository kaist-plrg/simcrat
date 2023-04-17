use std::fs::File;

use clap::Parser;
use simcrat::*;

#[derive(Parser, Debug)]
struct Args {
    #[arg(short, long)]
    log_file: Option<String>,
    #[arg(short, long)]
    api_key_file: Option<String>,
    #[arg(short, long)]
    cache_file: Option<String>,
    #[arg(long)]
    dont_try_multiple_signatures: bool,
    #[arg(long)]
    dont_provide_signatures: bool,
    #[arg(long)]
    dont_fix_errors: bool,
    inputs: Vec<String>,
}

#[tokio::main]
async fn main() {
    let args = Args::parse();

    if let Some(log) = args.log_file {
        let log_file = File::create(log).unwrap();
        tracing_subscriber::fmt()
            .with_max_level(tracing::Level::INFO)
            .with_ansi(false)
            .with_writer(log_file)
            .init();
    }

    let prog = c_parser::Program::new(&args.inputs);
    let api_key = args.api_key_file.unwrap_or(".openai_api_key".to_string());
    let client = openai_client::OpenAIClient::new(&api_key, args.cache_file);
    let Args {
        dont_try_multiple_signatures,
        dont_provide_signatures,
        dont_fix_errors,
        ..
    } = args;
    let config = translation::Config {
        try_multiple_signatures: !dont_try_multiple_signatures,
        provide_signatures: !dont_provide_signatures,
        fix_errors: !dont_fix_errors,
    };
    let mut translator = translation::Translator::new(&prog, client, config);
    translator.translate_names().await;
    translator.translate_types().await;
    translator.translate_variables().await;
    translator.translate_functions().await;
    println!("{}", translator.code());
    println!("{}", translator.errors());
    println!("{:?}", translator.signature_only());
}
