use std::{
    collections::{BTreeMap, BTreeSet},
    fs::File,
};

use clap::Parser;
use simcrat::{c_parser, openai_client::OpenAIClient};

#[derive(Parser, Debug)]
struct Args {
    #[arg(short, long)]
    input: String,
    #[arg(short, long)]
    log: Option<String>,
}

fn main() {
    // simcrat::compiler::compile(
    //     "fn main() { let mut x = 1; let a = &mut x; let b = &x; let x = *b; *a = 2; }",
    // );
    // simcrat::compiler::compile("fn main() { let x = 1i32; foo(x); } fn foo(x: u32) {}");
    // simcrat::compiler::compile("fn main() {} fn foo(x: u32) {");
    // simcrat::compiler::parse_signature("fn foo() -> u32 {}");
    // simcrat::compiler::parse_signature("fn foo() -> Option<u32> {}");
    // simcrat::compiler::parse_signature("fn foo() -> std::option::Option<u32> {}");
    // simcrat::compiler::parse_signature("fn foo() -> Result<u32, ()> {}");
    // simcrat::compiler::parse_signature("fn foo() -> dyn AAA + BBB {}");

    let args = Args::parse();

    if let Some(log) = &args.log {
        let log_file = File::create(log).unwrap();
        tracing_subscriber::fmt()
            .with_max_level(tracing::Level::INFO)
            .with_ansi(false)
            .with_writer(log_file)
            .init();
    }

    let parsed = c_parser::parse(args.input);
    let global_variables = c_parser::get_global_variables(&parsed);
    let variable_names: BTreeSet<_> = global_variables
        .iter()
        .flat_map(|v| c_parser::variable_names(v.node))
        .collect();
    let mut function_definitions = c_parser::get_function_definitions(&parsed);
    function_definitions.retain(|node| !c_parser::function_name(node.node).starts_with("__"));
    let name_map: BTreeMap<_, _> = function_definitions
        .iter()
        .map(|node| {
            let fd = node.node;
            let name = c_parser::function_name(fd);
            (name, node)
        })
        .collect();
    let mut call_graph: BTreeMap<_, BTreeSet<_>> = function_definitions
        .iter()
        .map(|node| {
            let fd = node.node;
            let name = c_parser::function_name(fd);
            let mut callees = c_parser::get_callees(fd);
            (name, callees.drain(..).collect())
        })
        .collect();
    let function_names: BTreeSet<_> = call_graph.keys().copied().collect();
    for callees in call_graph.values_mut() {
        callees.retain(|f| function_names.contains(f));
    }
    let (graph, elem_map) = simcrat::graph::compute_sccs(&call_graph);
    let inv_graph = simcrat::graph::inverse(&graph);
    let post_order = simcrat::graph::post_order(&graph, &inv_graph);

    let client = OpenAIClient::new(".openai_api_key");
    let translated_global_variables: BTreeMap<_, _> = global_variables
        .iter()
        .flat_map(|node| {
            let names = c_parser::variable_names(node.node);
            let code = c_parser::node_to_string(node, &parsed);
            let translated = client.translate_global_variable(code);
            names
                .iter()
                .map(|x| (*x, translated.clone()))
                .collect::<Vec<_>>()
        })
        .collect();

    let mut translated_signatures: BTreeMap<&String, String> = BTreeMap::new();
    let mut translated_functions: BTreeMap<&String, String> = BTreeMap::new();
    for v in &post_order {
        for id in v {
            let func_set = elem_map.get(id).unwrap();
            assert_eq!(func_set.len(), 1);
            let name = *func_set.iter().next().unwrap();
            let callees = call_graph.get(name).unwrap();
            let node = *name_map.get(name).unwrap();
            let mut ids = c_parser::get_identifiers(node.node);
            ids.retain(|v| variable_names.contains(v));
            let code = c_parser::node_to_string(node, &parsed);

            let new_name = client.rename(name);
            let sigs = client.translate_signature(code, &new_name);
            let mut sig_map = BTreeMap::new();
            for sig in sigs {
                let (sig_type, sig) = simcrat::compiler::parse_signature(&sig);
                sig_map.entry(sig_type).or_insert(sig);
            }
            for (sig_type, sig) in sig_map {
                let globs: Vec<_> = ids
                    .iter()
                    .map(|x| translated_global_variables.get(x).unwrap().clone())
                    .collect();
                let callees: Vec<_> = callees
                    .iter()
                    .map(|x| translated_signatures.get(x).unwrap().clone())
                    .collect();
                let translated = client.translate_function(code, &sig, &globs, &callees);
                println!("{}", translated);
                let (errors, suggestions) =
                    simcrat::compiler::type_check(&(translated.clone() + "\nfn main(){}"));
                for (error, code) in errors {
                    println!("{}", error);
                    println!("{}", code);
                }
                for suggestion in suggestions {
                    println!("{:?}", suggestion);
                }
                if let simcrat::compiler::Type::Path(t, _) = &sig_type.ret {
                    let semipredicate = match t.as_ref() {
                        "Option" => Some(true),
                        "Result" => Some(false),
                        _ => None,
                    };
                    if let Some(option) = semipredicate {
                        let proper =
                            simcrat::compiler::is_proper_semipredicate(&translated, option);
                        println!("{}", proper);
                    }
                }
                println!("\n=============================================================\n");
                translated_signatures.insert(name, sig);
                translated_functions.insert(name, translated);
            }
            if t() {
                break;
            }
        }
        if t() {
            break;
        }
    }
}

fn t() -> bool {
    true
}
