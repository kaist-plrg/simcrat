use clap::Parser;
use lang_c::{
    ast::*,
    driver::{parse, Config},
    span::Span,
    visit::{self, Visit},
};
use simcrat::{
    openai_client::OpenAIClient,
    pretty::{WriteLine, WriteString},
};

#[derive(Parser, Debug)]
struct Args {
    #[arg(short, long)]
    input: String,
}

fn main() {
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::WARN)
        .init();

    let args = Args::parse();

    let config = Config::default();
    let parsed = parse(&config, args.input).unwrap();
    let mut visitor = Visitor::default();
    visitor.visit_translation_unit(&parsed.unit);

    simcrat::compiler::compile("fn main() { 1u32 + 1i32; }");

    // let client = OpenAIClient::new(".openai_api_key");
    // for gv in &visitor.global_vars {
    //     println!("{}", gv.pretty());
    // }
    // for fd in &visitor.fun_defs {
    //     let name = fun_name(fd);
    //     if !name.starts_with("__") {
    //         let new_name = client.rename(name);
    //         let code = fd.pretty();
    //         let sigs = client.translate_signature(&code, &new_name);
    //         for sig in sigs {
    //             client.translate_function(&code, &sig);
    //         }
    //     }
    // }
}

fn fun_name(fun_def: &FunctionDefinition) -> &String {
    if let DeclaratorKind::Identifier(x) = &fun_def.declarator.node.kind.node {
        &x.node.name
    } else {
        panic!()
    }
}

#[derive(Debug, Default)]
struct Visitor<'ast> {
    fun_defs: Vec<&'ast FunctionDefinition>,
    global_vars: Vec<&'ast Declaration>,
}

impl<'ast> Visit<'ast> for Visitor<'ast> {
    fn visit_function_definition(
        &mut self,
        function_definition: &'ast FunctionDefinition,
        span: &'ast Span,
    ) {
        self.fun_defs.push(function_definition);
        visit::visit_function_definition(self, function_definition, span);
    }

    fn visit_external_declaration(
        &mut self,
        external_declaration: &'ast ExternalDeclaration,
        span: &'ast Span,
    ) {
        if let ExternalDeclaration::Declaration(d) = external_declaration {
            if !d.node.specifiers.iter().any(|s| match &s.node {
                DeclarationSpecifier::StorageClass(s) => match &s.node {
                    StorageClassSpecifier::Extern => true,
                    StorageClassSpecifier::Typedef => true,
                    _ => false,
                },
                _ => false,
            }) && d.node.declarators.len() != 0
                && !d.node.declarators.iter().any(|d| {
                    d.node
                        .declarator
                        .node
                        .derived
                        .iter()
                        .any(|d| match &d.node {
                            DerivedDeclarator::Function(_) => true,
                            _ => false,
                        })
                })
            {
                self.global_vars.push(&d.node);
            }
        }
        visit::visit_external_declaration(self, external_declaration, span);
    }
}
