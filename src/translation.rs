use std::collections::{BTreeMap, BTreeSet};

use lang_c::{
    ast::{Declaration, FunctionDefinition},
    driver::Parse,
    span::Node,
};

use crate::{c_parser, compiler, graph, openai_client::OpenAIClient};

pub struct Translator<'ast> {
    parsed: &'ast Parse,
    variable_declarations: Vec<&'ast Node<Declaration>>,
    variables: BTreeSet<&'ast str>,
    function_definitions: BTreeMap<&'ast str, &'ast Node<FunctionDefinition>>,
    call_graph: BTreeMap<&'ast str, BTreeSet<&'ast str>>,
    function_post_order: Vec<BTreeSet<&'ast str>>,

    client: OpenAIClient,

    translated_variables: BTreeMap<&'ast str, String>,
    translated_signatures: BTreeMap<&'ast str, String>,
    translated_functions: BTreeMap<&'ast str, String>,
    use_list: Vec<String>,
}

impl<'ast> Translator<'ast> {
    pub fn new(parsed: &'ast Parse, api_key: &str) -> Self {
        let variable_declarations = c_parser::get_variable_declarations(parsed);
        let variables: BTreeSet<_> = variable_declarations
            .iter()
            .flat_map(|v| c_parser::variable_names(&v.node))
            .collect();

        let mut function_definitions = c_parser::get_function_definitions(parsed);
        function_definitions.retain(|node| !c_parser::function_name(&node.node).starts_with("__"));
        let function_definitions: BTreeMap<_, _> = function_definitions
            .into_iter()
            .map(|node| {
                let name = c_parser::function_name(&node.node);
                (name, node)
            })
            .collect();

        let function_names: BTreeSet<_> = function_definitions.keys().copied().collect();
        let mut call_graph: BTreeMap<_, BTreeSet<_>> = function_definitions
            .iter()
            .map(|(name, node)| {
                let callees = c_parser::get_callees(&node.node);
                (*name, callees.into_iter().collect())
            })
            .collect();
        for callees in call_graph.values_mut() {
            callees.retain(|f| function_names.contains(f));
        }

        let (graph, mut elem_map) = graph::compute_sccs(&call_graph);
        let inv_graph = graph::inverse(&graph);
        let function_post_order: Vec<_> = graph::post_order(&graph, &inv_graph)
            .into_iter()
            .flatten()
            .map(|id| elem_map.remove(&id).unwrap())
            .collect();

        let client = OpenAIClient::new(api_key);

        Self {
            parsed,
            variable_declarations,
            variables,
            function_definitions,
            call_graph,
            function_post_order,
            client,
            translated_variables: BTreeMap::new(),
            translated_signatures: BTreeMap::new(),
            translated_functions: BTreeMap::new(),
            use_list: vec![],
        }
    }

    pub fn translate_variables(&mut self) {
        for node in self.variable_declarations.iter().copied() {
            let names = c_parser::variable_names(&node.node);
            assert_eq!(names.len(), 1);
            let code = c_parser::node_to_string(node, self.parsed);
            let translated = self.client.translate_global_variable(code);
            for name in names {
                self.translated_variables.insert(name, translated.clone());
            }
        }
    }

    pub fn translate_functions(&mut self) {
        for set in &self.function_post_order {
            assert_eq!(set.len(), 1);
            let name = *set.iter().next().unwrap();
            let (sig, translated, mut add_use) = self.translate_function(name);
            self.translated_signatures.insert(name, sig);
            self.translated_functions.insert(name, translated);
            self.use_list.append(&mut add_use);
        }
    }

    fn translate_function(&self, name: &str) -> (String, String, Vec<String>) {
        println!("{}", name);
        let node = *self.function_definitions.get(name).unwrap();

        let mut ids = c_parser::get_identifiers(&node.node);
        ids.retain(|v| self.variables.contains(v));
        let variables: Vec<_> = ids
            .iter()
            .map(|x| self.translated_variables.get(x).unwrap().as_str())
            .collect();

        let callees = self.call_graph.get(name).unwrap();
        let functions: Vec<_> = callees
            .iter()
            .map(|x| self.translated_signatures.get(x).unwrap().as_str())
            .collect();

        let functions_for_check: Vec<_> = functions
            .iter()
            .map(|s| {
                let s = s.strip_suffix("{}").unwrap();
                format!(
                    "#[allow(dead_code, unused_variables)] {} {{ todo!() }}\n",
                    s
                )
            })
            .collect();

        let has_main = self.translated_functions.contains_key("main") || name == "main";

        let prefix = format!(
            "{}\n{}\n{}\n{}\n",
            self.use_list.join("\n"),
            variables.join("\n"),
            functions_for_check.join("\n"),
            if has_main { "" } else { "fn main() {}" },
        );

        let new_name = self.client.rename(name);
        let code = c_parser::node_to_string(node, self.parsed);
        let sigs = self.client.translate_signature(code, &new_name);

        let mut sig_map = BTreeMap::new();
        for sig in sigs {
            let (sig_type, sig) = compiler::parse_signature(&sig);
            sig_map.entry(sig_type).or_insert(sig);
        }

        let mut candidates = vec![];
        for (sig_type, sig) in sig_map {
            println!("{}\n........................................", sig);
            let translated = self
                .client
                .translate_function(code, &sig, &variables, &functions);
            println!("{}\n........................................", translated);

            let (real_sig_type, _) = compiler::parse_signature(&translated);
            if sig_type != real_sig_type {
                println!("diff\n----------------------------------------");
                continue;
            }

            let code = format!("{}{}", prefix, translated);
            println!("{}\n........................................", code);
            let result = compiler::type_check(&code);
            let (code, result) = compiler::apply_suggestions(code, result);
            println!("{}\n........................................", code);
            let translated = code[prefix.len()..].to_string();
            println!("{}\n........................................", translated);

            let proper_semipredicate = if let compiler::Type::Path(t, _) = &sig_type.ret {
                let semipredicate = match t.as_ref() {
                    "Option" => Some(true),
                    "Result" => Some(false),
                    _ => None,
                };
                if let Some(option) = semipredicate {
                    let proper = compiler::is_proper_semipredicate(&translated, option);
                    if proper {
                        0
                    } else {
                        1
                    }
                } else {
                    0
                }
            } else {
                0
            };
            let score = result.errors.len() * 100 + result.warnings * 10 + proper_semipredicate;
            println!("{}\n----------------------------------------", score);
            candidates.push((sig, translated, result, score))
        }

        let (sig, translated, result, _) = candidates
            .into_iter()
            .min_by_key(|(_, _, _, score)| *score)
            .unwrap();

        println!("{}", translated);
        for (error, code) in result.errors {
            println!("{}", error);
            println!("{}", code);
        }
        println!("========================================");

        (sig, translated, result.add_use)
    }
}
