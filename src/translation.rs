use std::collections::{BTreeMap, BTreeSet};

use lang_c::span::Span;

use crate::{
    c_parser::{
        self, CustomType, Function, Program, Struct, TypeDependency, TypeSort, Typedef, Variable,
    },
    compiler::{self, FunTySig, TypeCheckingResult},
    graph,
    openai_client::OpenAIClient,
};

pub struct Translator<'ast> {
    program: &'ast Program,
    typedefs: BTreeMap<&'ast str, Typedef<'ast>>,
    structs: BTreeMap<&'ast str, Struct<'ast>>,
    variables: BTreeMap<&'ast str, Variable<'ast>>,
    functions: BTreeMap<&'ast str, Function<'ast>>,

    type_post_order: Vec<BTreeSet<CustomType<'ast>>>,
    function_post_order: Vec<BTreeSet<&'ast str>>,

    client: OpenAIClient,

    translated_types: BTreeMap<CustomType<'ast>, TranslatedType>,
    translated_variable_names: BTreeMap<&'ast str, String>,
    translated_variable_types: BTreeMap<&'ast str, String>,
    translated_variables: BTreeMap<&'ast str, String>,
    translated_function_names: BTreeMap<&'ast str, String>,
    translated_signatures: BTreeMap<&'ast str, String>,
    translated_functions: BTreeMap<&'ast str, String>,
    use_list: Vec<String>,

    num_signatures: usize,
}

pub struct TranslatedType {
    name: String,
    #[allow(unused)]
    code: String,
    #[allow(unused)]
    copied: bool,
}

impl<'ast> Translator<'ast> {
    pub fn new(program: &'ast Program, client: OpenAIClient, num_signatures: usize) -> Self {
        let typedefs = program.typedefs();
        let structs = program.structs();
        let variables = program.variables();
        let functions = program.functions();

        let mut cg: BTreeMap<_, BTreeSet<_>> = BTreeMap::new();
        for (name, t) in &typedefs {
            cg.insert(
                CustomType::mk_typedef(name),
                t.dependencies.iter().map(|t| t.typ).collect(),
            );
        }
        for (name, s) in &structs {
            let x = if s.strct {
                CustomType::mk_struct(name)
            } else {
                CustomType::mk_union(name)
            };
            cg.insert(x, s.dependencies.iter().map(|t| t.typ).collect());
        }
        let (graph, mut elem_map) = graph::compute_sccs(&cg);
        let inv_graph = graph::inverse(&graph);
        let type_post_order: Vec<_> = graph::post_order(&graph, &inv_graph)
            .into_iter()
            .flatten()
            .map(|id| elem_map.remove(&id).unwrap())
            .collect();

        let cg = functions
            .iter()
            .map(|(name, function)| {
                (
                    *name,
                    function
                        .callees
                        .iter()
                        .map(|callee| callee.node.name.as_str())
                        .collect(),
                )
            })
            .collect();
        let (graph, mut elem_map) = graph::compute_sccs(&cg);
        let inv_graph = graph::inverse(&graph);
        let function_post_order: Vec<_> = graph::post_order(&graph, &inv_graph)
            .into_iter()
            .flatten()
            .map(|id| elem_map.remove(&id).unwrap())
            .collect();

        Self {
            program,
            typedefs,
            structs,
            variables,
            functions,
            type_post_order,
            function_post_order,
            client,
            translated_types: BTreeMap::new(),
            translated_variable_names: BTreeMap::new(),
            translated_variable_types: BTreeMap::new(),
            translated_variables: BTreeMap::new(),
            translated_function_names: BTreeMap::new(),
            translated_signatures: BTreeMap::new(),
            translated_functions: BTreeMap::new(),
            use_list: vec![],
            num_signatures,
        }
    }

    pub fn whole_code(&self) -> String {
        let has_main = self.translated_functions.contains_key("main");
        format!(
            "{}\n{}\n{}\n{}\n{}",
            self.use_list.join("\n"),
            self.translated_types
                .values()
                .filter_map(|t| if t.copied {
                    None
                } else {
                    Some(t.code.as_str())
                })
                .collect::<Vec<_>>()
                .join("\n"),
            self.translated_variables
                .values()
                .cloned()
                .collect::<Vec<_>>()
                .join("\n"),
            self.translated_functions
                .values()
                .cloned()
                .collect::<Vec<_>>()
                .join("\n"),
            if has_main { "" } else { "fn main() {}" }
        )
    }

    fn make_type_replace_vec<'a>(
        &'a self,
        deps: &[TypeDependency<'a>],
        typ: &CustomType<'_>,
        name: &'a str,
    ) -> Vec<(Span, &'a str)> {
        deps.iter()
            .map(|d| {
                let s = if &d.typ == typ {
                    name
                } else {
                    self.translated_types.get(&d.typ).unwrap().name.as_str()
                };
                (d.span, s)
            })
            .collect()
    }

    fn make_type_prefix<'a>(&'a self, deps: &[TypeDependency<'a>]) -> Vec<&'a str> {
        let types: BTreeSet<_> = deps.iter().map(|d| &d.typ).collect();
        types
            .into_iter()
            .filter_map(|d| Some(self.translated_types.get(d)?.code.as_str()))
            .collect()
    }

    pub fn translate_types(&mut self) {
        for set in &self.type_post_order {
            assert_eq!(set.len(), 1);
            let typ = *set.iter().next().unwrap();
            let sort = typ.sort.to_string();
            let new_name = self.client.rename_type(typ.name);
            let new_name = if new_name == "Option" {
                format!("My{}", new_name)
            } else {
                new_name
            };

            let translated = if matches!(typ.sort, TypeSort::Typedef) {
                let typedef = self.typedefs.get(typ.name).unwrap();
                let deps = &typedef.dependencies;
                if typedef.is_struct_alias {
                    let aliased = self.translated_types.get(&deps[0].typ).unwrap();
                    TranslatedType {
                        name: aliased.name.clone(),
                        code: aliased.code.clone(),
                        copied: true,
                    }
                } else {
                    let vec = self.make_type_replace_vec(deps, &typ, &new_name);
                    let (code, sort) = match self
                        .program
                        .typedef_to_struct_string(typedef, vec, &new_name)
                    {
                        Ok((s, sort)) => (s, sort.to_string()),
                        Err(mut vec) => {
                            vec.push((typedef.identifier.span, new_name.as_str()));
                            (self.program.typedef_to_string(typedef, vec), sort)
                        }
                    };
                    let prefix = self.make_type_prefix(deps);
                    println!("{}\n{}", prefix.join("\n"), code);
                    let translated = self.client.translate_type(&code, &sort, &prefix);
                    TranslatedType {
                        name: new_name,
                        code: translated,
                        copied: false,
                    }
                }
            } else {
                let strct = self.structs.get(typ.name).unwrap();
                let deps = &strct.dependencies;
                let mut vec = self.make_type_replace_vec(deps, &typ, &new_name);
                vec.push((
                    strct.struct_type.node.identifier.as_ref().unwrap().span,
                    new_name.as_str(),
                ));
                let code = self.program.struct_to_string(strct, vec);
                let prefix = self.make_type_prefix(deps);
                println!("{}\n{}", prefix.join("\n"), code);
                let translated = self.client.translate_type(&code, &sort, &prefix);
                TranslatedType {
                    name: new_name,
                    code: translated,
                    copied: false,
                }
            };
            println!("----------\n{}\n==========", translated.code);
            self.translated_types.insert(typ, translated);
        }
    }

    pub fn translate_variables(&mut self) {
        for (name, variable) in &self.variables {
            let code = self.program.variable_to_string(variable);
            let translated = self.client.translate_global_variable(&code);
            let mut parsed = compiler::parse_global_variable(&translated);
            assert_eq!(parsed.len(), 1);
            let (new_name, ty) = parsed.pop().unwrap();
            self.translated_variable_names.insert(name, new_name);
            self.translated_variable_types.insert(name, ty);
            self.translated_variables.insert(name, translated.clone());
        }
    }

    pub fn translate_functions(&mut self) {
        for set in &self.function_post_order {
            assert_eq!(set.len(), 1);
            let name = *set.iter().next().unwrap();
            let mut function = self.translate_function(name);
            self.translated_function_names.insert(name, function.name);
            self.translated_signatures.insert(name, function.signature);
            self.translated_functions.insert(name, function.translated);
            self.use_list.append(&mut function.uses);
        }
    }

    fn translate_function(&self, name: &str) -> TranslatedFunction {
        let function = self.functions.get(name).unwrap();

        let ids = &function.dependencies;

        let variable_names: Vec<_> = ids.iter().map(|x| x.node.name.as_str()).collect();
        let variables: Vec<_> = variable_names
            .iter()
            .map(|x| self.translated_variables.get(x).unwrap().as_str())
            .collect();

        let mut callee_names: Vec<_> = function
            .callees
            .iter()
            .map(|x| x.node.name.as_str())
            .collect();
        callee_names.sort();
        callee_names.dedup();
        let functions: Vec<_> = callee_names
            .iter()
            .map(|x| self.translated_signatures.get(x).unwrap().as_str())
            .collect();

        let use_list_for_check: Vec<_> = self
            .use_list
            .iter()
            .map(|s| format!("#[allow(unused_imports)] {}", s))
            .collect();
        let variables_for_check: Vec<_> = variable_names
            .iter()
            .map(|x| {
                let s = self.translated_variables.get(x).unwrap();
                let i = s.find('=').unwrap();
                let ty = self.translated_variable_types.get(x).unwrap();
                format!(
                    "#[deny(unused)] {} unsafe {{ std::mem::transmute([0u8; std::mem::size_of::<{}>()]) }}",
                    &s[..=i], ty
                )
            })
            .collect();
        let functions_for_check: Vec<_> = functions
            .iter()
            .map(|s| {
                let s = s.strip_suffix("{}").unwrap();
                format!(
                    "#[deny(dead_code)] #[allow(unused_variables)] {} {{ todo!() }}\n",
                    s
                )
            })
            .collect();
        let has_main = self.translated_functions.contains_key("main") || name == "main";
        let mut prefixes = vec![
            use_list_for_check.join("\n"),
            variables_for_check.join("\n"),
            if has_main { "" } else { "fn main() {}" }.to_string(),
            functions_for_check.join("\n"),
            "#[allow(dead_code)]".to_string(),
        ];
        prefixes.retain(|s| !s.is_empty());
        let prefix = format!("{}\n", prefixes.join("\n"));

        let new_name = self.client.rename(name);
        let mut replace_vec = vec![(
            c_parser::function_name_span(&function.definition.node),
            new_name.as_str(),
        )];
        for x in ids {
            replace_vec.push((
                x.span,
                self.translated_variable_names
                    .get(x.node.name.as_str())
                    .unwrap()
                    .as_str(),
            ));
        }
        for x in &function.callees {
            replace_vec.push((
                x.span,
                self.translated_function_names
                    .get(x.node.name.as_str())
                    .unwrap()
                    .as_str(),
            ));
        }
        let code = self.program.function_to_string(function, replace_vec);
        let sigs = self
            .client
            .translate_signature(&code, &new_name, self.num_signatures);

        let mut sig_map = BTreeMap::new();
        for sig in sigs {
            let (sig_type, sig) = compiler::parse_signature(&sig, false, true);
            sig_map.entry(sig_type).or_insert(sig);
        }

        println!("{}\n........................................", prefix);
        println!("{}\n........................................", code);

        let mut candidates = vec![];
        for (sig_type, sig) in sig_map {
            println!("{}\n........................................", sig);
            let translated = self
                .client
                .translate_function(&code, &sig, &variables, &functions);
            println!("{}\n........................................", translated);

            // let (real_sig_type, _) = compiler::parse_signature(&translated, false, true);
            // if sig_type != real_sig_type {
            if !translated.starts_with(sig.strip_suffix("{}").unwrap()) {
                println!("diff\n----------------------------------------");
                continue;
            }

            let function = TranslatedFunction {
                name: new_name.clone(),
                prefix: prefix.clone(),
                signature_type: sig_type,
                signature: sig,
                translated: translated.clone(),
                uses: vec![],
                errors: vec![],
            };
            let fixed = self.fix_function(function);
            if translated != fixed.translated {
                for diff in diff::lines(&translated, &fixed.translated) {
                    match diff {
                        diff::Result::Left(l) => println!("-{}", l),
                        diff::Result::Both(l, _) => println!(" {}", l),
                        diff::Result::Right(r) => println!("+{}", r),
                    }
                }
            }
            // let proper_semipredicate = if let compiler::Type::Path(t, _) = &sig_type.ret {
            //     let semipredicate = match t.as_ref() {
            //         "Option" => Some(true),
            //         "Result" => Some(false),
            //         _ => None,
            //     };
            //     if let Some(option) = semipredicate {
            //         let proper = compiler::is_proper_semipredicate(&translated, option);
            //         if proper {
            //             0
            //         } else {
            //             1
            //         }
            //     } else {
            //         0
            //     }
            // } else {
            //     0
            // };
            // let score = result.errors.len() * 100 + result.warnings * 10 + proper_semipredicate;
            let score = fixed.errors.len();
            println!("{}\n----------------------------------------", score);
            candidates.push((fixed, score))
        }

        let best_score = candidates.iter().map(|(_, score)| *score).min().unwrap();
        candidates.retain(|(_, score)| *score == best_score);
        let (function, _) = candidates
            .into_iter()
            .max_by(|(a, _), (b, _)| self.client.compare(&a.translated, &b.translated))
            .unwrap();

        println!("{}", function.translated);
        for (error, _) in &function.errors {
            println!("{}", error);
        }
        println!("{}\n========================================", best_score);

        function
    }

    fn fix_function_llm(&self, function: TranslatedFunction) -> TranslatedFunction {
        for (error, code) in &function.errors {
            let fixed = self.client.fix(code, error);
            if fixed.starts_with("use ")
                || fixed.starts_with("fn ") != code.starts_with("fn ")
                || fixed.contains("extern crate ")
                || fixed.contains("[dependencies]")
            {
                continue;
            }
            let indentation: String = code.chars().take_while(|c| c.is_whitespace()).collect();
            let fixed = indentation + fixed.trim();
            let translated = function.translated.replace(code, &fixed);
            let fixed = fix_function_compiler(TranslatedFunction {
                translated,
                ..function.clone()
            });
            if fixed.errors.len() < function.errors.len() {
                return self.fix_function_llm(fixed);
            }
        }
        function
    }

    fn fix_function(&self, function: TranslatedFunction) -> TranslatedFunction {
        self.fix_function_llm(fix_function_compiler(function))
    }
}

fn fix_function_compiler(mut function: TranslatedFunction) -> TranslatedFunction {
    let new_prefix = format!("{}\n{}", function.uses.join("\n"), function.prefix);
    let code = format!("{}{}", new_prefix, function.translated);
    let result = compiler::type_check(&code);
    let (
        code,
        TypeCheckingResult {
            errors,
            mut add_use,
            ..
        },
    ) = compiler::apply_suggestions(code, result);
    let translated = code[new_prefix.len()..].to_string();
    if add_use.is_empty() {
        TranslatedFunction {
            translated,
            errors,
            ..function
        }
    } else {
        function.uses.append(&mut add_use);
        fix_function_compiler(function)
    }
}

#[derive(Clone)]
struct TranslatedFunction {
    prefix: String,
    name: String,
    #[allow(unused)]
    signature_type: FunTySig,
    signature: String,
    translated: String,
    uses: Vec<String>,
    errors: Vec<(String, String)>,
}

#[allow(unused)]
fn add_comments<S: AsRef<str>>(code: &str, comments: BTreeMap<usize, Vec<S>>) -> String {
    let mut s = String::new();
    for (i, line) in code.split('\n').enumerate() {
        if let Some(comments) = comments.get(&i) {
            let indentation: String = line.chars().take_while(|c| c.is_whitespace()).collect();
            for comment in comments {
                s.push_str(&indentation);
                s.push_str("// ");
                s.push_str(comment.as_ref());
                s.push('\n');
            }
        }
        s.push_str(line);
        s.push('\n');
    }
    let _ = s.strip_suffix('\n');
    s
}
