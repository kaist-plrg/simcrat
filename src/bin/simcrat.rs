use std::{fs::File, io::Write, time::Instant};

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
    no_candidate: bool,
    #[arg(long)]
    no_augmentation: bool,
    #[arg(long)]
    no_fix: bool,
    #[arg(long)]
    no_stage: bool,

    #[arg(long)]
    parsing_only: bool,
    #[arg(long)]
    real_time: bool,

    #[arg(long)]
    show_time: bool,
    #[arg(long)]
    show_program_size: bool,
    #[arg(long)]
    show_error_num: bool,
    #[arg(long)]
    show_per_stage: bool,
    #[arg(long)]
    show_long_num: bool,
    #[arg(long)]
    show_openai_stat: bool,
    #[arg(long)]
    show_signature: bool,

    #[arg(short, long)]
    detail: bool,
    #[arg(short, long)]
    quiet: bool,

    #[arg(short, long)]
    output: Option<String>,
    input: String,
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

    let api_key = args.api_key_file;
    let db_conf = openai_client::DbConfig {
        name: args.db_name,
        host: args.db_host,
        port: args.db_port,
        password: args.db_password,
        real_time: args.real_time,
    };
    let config = translation::Config {
        try_multiple_signatures: !args.no_candidate,
        provide_signatures: !args.no_augmentation,
        fix_errors: !args.no_fix,
        consider_stages: !args.no_stage,
        quiet: args.quiet,
    };

    let _timer = if args.show_time {
        Some(Timer::new())
    } else {
        None
    };

    let prog = c_parser::Program::from_compile_commands(&args.input);
    let client = openai_client::OpenAIClient::new(api_key, db_conf).await;
    let mut translator = translation::Translator::new(&prog, client, config);

    if args.show_program_size {
        translator.show_information();
    }

    if args.parsing_only {
        return;
    }

    translator.translate_names().await;
    translator.translate_types().await;
    translator.translate_variables().await;
    translator.translate_protos().await;
    translator.translate_functions().await;

    if let Some(output) = args.output {
        let mut f = File::create(output).unwrap();
        f.write_all(translator.code().as_bytes()).unwrap();
    }

    if args.show_error_num {
        println!("{}", translator.errors());
    }

    if args.show_per_stage {
        println!("{}", translator.per_stage());
    }

    if args.show_long_num {
        println!("{}", translator.too_long().len());
    }

    if args.show_openai_stat {
        translator.show_openai_stat();
    }

    if args.show_signature {
        translator.compare_signatures(args.detail);
    }
}

struct Timer {
    start: Instant,
}

impl Timer {
    fn new() -> Self {
        Self {
            start: Instant::now(),
        }
    }
}

impl Drop for Timer {
    fn drop(&mut self) {
        println!("{:?}", self.start.elapsed().as_secs_f32());
    }
}
