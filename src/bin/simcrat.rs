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
    cache_db_name: Option<String>,
    #[arg(long)]
    no_signature: bool,
    #[arg(long)]
    no_dependency: bool,
    #[arg(long)]
    no_fix: bool,

    input: String,
}

#[tokio::main(flavor = "multi_thread", worker_threads = 48)]
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

    let api_key = args.api_key_file.unwrap_or(".openai_api_key".to_string());
    let config = translation::Config {
        try_multiple_signatures: !args.no_signature,
        provide_signatures: !args.no_dependency,
        fix_errors: !args.no_fix,
    };

    let prog = c_parser::Program::from_compile_commands(&args.input);
    let client = openai_client::OpenAIClient::new(&api_key, args.cache_db_name).await;
    let mut translator = translation::Translator::new(&prog, client, config);

    translator.show_information();
    translator.translate_names().await;
    translator.translate_types().await;
    translator.translate_variables().await;
    translator.translate_protos().await;
    translator.translate_functions().await;

    // println!("{}", translator.code());
    println!("{}", translator.errors());
    println!("{:?}", translator.too_long());
    translator.openai_client_stat();
}
