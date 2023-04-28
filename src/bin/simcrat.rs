use std::{fs::File, time::Instant};

use clap::Parser;
use simcrat::*;

#[derive(Parser, Debug)]
struct Args {
    #[arg(short, long)]
    log_file: Option<String>,

    #[arg(short, long)]
    api_key_file: Option<String>,

    #[arg(long)]
    db_name: Option<String>,
    #[arg(long)]
    db_host: Option<String>,
    #[arg(long)]
    db_port: Option<String>,
    #[arg(long)]
    db_password: Option<String>,

    #[arg(long)]
    show_info: bool,
    #[arg(long)]
    no_signature: bool,
    #[arg(long)]
    no_dependency: bool,
    #[arg(long)]
    no_fix: bool,
    #[arg(short, long)]
    quiet: bool,

    input: String,
}

#[tokio::main]
async fn main() {
    let start = Instant::now();

    let args = Args::parse();

    if let Some(log) = args.log_file {
        let log_file = File::create(log).unwrap();
        tracing_subscriber::fmt()
            .with_max_level(tracing::Level::INFO)
            .with_ansi(false)
            .with_writer(log_file)
            .init();
    }

    let api_key = args.api_key_file;
    let db_conf = openai_client::DbConfig {
        name: args.db_name,
        host: args.db_host,
        port: args.db_port,
        password: args.db_password,
    };
    let config = translation::Config {
        try_multiple_signatures: !args.no_signature,
        provide_signatures: !args.no_dependency,
        fix_errors: !args.no_fix,
        quiet: args.quiet,
    };

    let prog = c_parser::Program::from_compile_commands(&args.input);
    let client = openai_client::OpenAIClient::new(api_key, db_conf).await;
    let mut translator = translation::Translator::new(&prog, client, config);

    if args.show_info {
        translator.show_information();
        return;
    }

    translator.translate_names().await;
    translator.translate_types().await;
    translator.translate_variables().await;
    translator.translate_protos().await;
    translator.translate_functions().await;

    // println!("{}", translator.code());
    println!("errors: {}", translator.errors());
    if !args.quiet {
        println!("long: {:?}", translator.too_long());
        translator.openai_client_stat();
        println!("time: {:?}", start.elapsed());
    }
}
