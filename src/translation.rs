use std::collections::{BTreeMap, BTreeSet};

use lang_c::{
    ast::Identifier,
    span::{Node, Span},
};

#[allow(unused_imports)]
use crate::{
    c_parser::{
        self, CustomType, Function, Program, Struct, TypeDependency, TypeSort, Typedef, Variable,
    },
    compiler::{self, FunTySig, ItemSort, ParsedItem, ParsedType, TypeCheckingResult},
    graph,
    openai_client::OpenAIClient,
};

#[allow(unused)]
pub struct Translator<'ast> {
    program: &'ast Program,
    typedefs: BTreeMap<&'ast str, Typedef<'ast>>,
    structs: BTreeMap<&'ast str, Struct<'ast>>,
    variables: BTreeMap<&'ast str, Variable<'ast>>,
    functions: BTreeMap<&'ast str, Function<'ast>>,

    transitive_types: BTreeMap<CustomType<'ast>, BTreeSet<CustomType<'ast>>>,
    type_post_order: Vec<BTreeSet<CustomType<'ast>>>,
    variable_post_order: Vec<BTreeSet<&'ast str>>,
    function_post_order: Vec<BTreeSet<&'ast str>>,

    client: OpenAIClient,

    new_type_names: BTreeMap<CustomType<'ast>, String>,
    new_term_names: BTreeMap<&'ast str, String>,

    translated_type_names: BTreeSet<String>,
    translated_term_names: BTreeSet<String>,

    translated_types: BTreeMap<CustomType<'ast>, TranslationResult>,
    translated_variables: BTreeMap<&'ast str, TranslationResult>,
    translated_functions: BTreeMap<&'ast str, TranslationResult>,
    uses: Vec<String>,
    num_signatures: usize,
}

#[derive(Debug, Clone)]
struct TranslationResult {
    items: Vec<ParsedItem>,
    uses: Vec<String>,
    copied: bool,
}

impl TranslationResult {
    #[inline]
    fn mk_code<F>(&self, f: F) -> String
    where F: FnMut(&ParsedItem) -> String {
        self.items.iter().map(f).collect::<Vec<_>>().join("\n")
    }

    fn code(&self) -> String {
        self.mk_code(|i| i.get_code())
    }

    fn simple_code(&self) -> String {
        self.mk_code(|i| i.get_simple_code())
    }

    fn checking_code(&self) -> String {
        self.mk_code(|i| i.get_checking_code())
    }
}

#[derive(Debug, Clone)]
struct FixContext<'a> {
    uses: Vec<String>,
    prefix: &'a str,
    code: String,
    result: TypeCheckingResult,
}

impl<'a> FixContext<'a> {
    fn new(prefix: &'a str, code: String) -> Self {
        let result = compiler::type_check(&format!("{}\n{}", prefix, code));
        Self {
            uses: vec![],
            prefix,
            code,
            result,
        }
    }

    fn add_uses(&mut self) {
        self.uses.append(&mut self.result.uses);
        self.result = compiler::type_check(&self.code());
    }

    fn update(&mut self, code: String) {
        self.code = code;
        self.result = compiler::type_check(&self.code());
    }

    fn update_whole(&mut self, code: &str) {
        let prefix = self.uses_and_prefix();
        self.code = code
            .strip_prefix(&prefix)
            .unwrap()
            .strip_prefix('\n')
            .unwrap()
            .to_string();
        self.result = compiler::type_check(code);
    }

    fn prefix_lines(&self) -> usize {
        self.uses_and_prefix().split('\n').count()
    }

    fn code(&self) -> String {
        format!("{}\n{}", self.uses_and_prefix(), self.code)
    }

    fn uses_and_prefix(&self) -> String {
        format!("{}{}", self.uses.join("\n"), self.prefix)
    }
}

static DERIVES: [&str; 9] = [
    "Clone",
    "Copy",
    "Debug",
    "Default",
    "PartialOrd",
    "Ord",
    "PartialEq",
    "Eq",
    "Hash",
];

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
        let type_post_order = post_order(&cg);
        let transitive_types = graph::transitive_closure(cg);

        let cg = variables
            .iter()
            .map(|(name, variable)| {
                (
                    *name,
                    variable
                        .dependencies
                        .iter()
                        .map(|callee| callee.node.name.as_str())
                        .collect(),
                )
            })
            .collect();
        let variable_post_order = post_order(&cg);

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
        let function_post_order = post_order(&cg);

        Self {
            program,
            typedefs,
            structs,
            variables,
            functions,
            transitive_types,
            type_post_order,
            variable_post_order,
            function_post_order,
            client,
            new_type_names: BTreeMap::new(),
            new_term_names: BTreeMap::new(),
            translated_type_names: BTreeSet::new(),
            translated_term_names: BTreeSet::new(),
            translated_types: BTreeMap::new(),
            translated_variables: BTreeMap::new(),
            translated_functions: BTreeMap::new(),
            uses: vec![
                "extern crate once_cell;".to_string(),
                "#[macro_use] extern crate lazy_static;".to_string(),
                "use once_cell::sync::Lazy;".to_string(),
            ],
            num_signatures,
        }
    }

    #[inline]
    fn mk_code<F>(&self, put_main: bool, f: F) -> String
    where F: FnMut(&TranslationResult) -> String {
        let mut v: Vec<_> = self
            .uses
            .iter()
            .cloned()
            .chain(
                self.translated_types
                    .values()
                    .chain(self.translated_variables.values())
                    .chain(self.translated_functions.values())
                    .filter(|r| !r.copied)
                    .map(f),
            )
            .collect();
        if put_main && !self.translated_functions.contains_key("main") {
            v.push("fn main() {}".to_string());
        }
        v.join("\n")
    }

    pub fn code(&self, put_main: bool) -> String {
        self.mk_code(put_main, |r| r.code())
    }

    fn checking_code(&self, put_main: bool) -> String {
        self.mk_code(put_main, |r| r.checking_code())
    }

    fn make_replace_vec<'a>(
        &'a self,
        types: Option<&[TypeDependency<'a>]>,
        vars: Option<&[&Node<Identifier>]>,
        callees: Option<&[&Node<Identifier>]>,
    ) -> Vec<(Span, &'a str)> {
        let mut vec = vec![];

        if let Some(deps) = types {
            for d in deps {
                if let Some(x) = self.new_type_names.get(&d.typ) {
                    vec.push((d.span, x.as_str()));
                }
            }
        }

        if let Some(deps) = vars {
            for d in deps {
                if let Some(x) = self.new_term_names.get(d.node.name.as_str()) {
                    vec.push((d.span, x.as_str()));
                }
            }
        }

        if let Some(deps) = callees {
            for d in deps {
                if let Some(x) = self.new_term_names.get(d.node.name.as_str()) {
                    vec.push((d.span, x.as_str()));
                }
            }
        }

        vec
    }

    fn make_translation_prefix(
        &self,
        types: Option<&[TypeDependency<'_>]>,
        vars: Option<&[&Node<Identifier>]>,
        callees: Option<&[&Node<Identifier>]>,
        transitive: bool,
    ) -> Vec<String> {
        let mut vec = vec![];

        if let Some(deps) = types {
            let deps: BTreeSet<_> = if transitive {
                deps.iter()
                    .flat_map(|t| self.transitive_types.get(&t.typ).unwrap())
                    .copied()
                    .chain(deps.iter().map(|t| t.typ))
                    .collect()
            } else {
                deps.iter().map(|t| t.typ).collect()
            };
            let types: BTreeSet<_> = deps
                .into_iter()
                .filter_map(|d| Some(self.translated_types.get(&d)?.simple_code()))
                .collect();
            for t in types {
                vec.push(t);
            }
        }

        if let Some(vars) = vars {
            let vars: BTreeSet<_> = vars.iter().map(|x| x.node.name.as_str()).collect();
            for x in vars {
                if let Some(t) = self.translated_variables.get(x) {
                    vec.push(t.simple_code());
                }
            }
        }

        if let Some(callees) = callees {
            let callees: BTreeSet<_> = callees.iter().map(|x| x.node.name.as_str()).collect();
            for x in callees {
                if let Some(t) = self.translated_functions.get(x) {
                    vec.push(t.code());
                }
            }
        }

        vec
    }

    fn dedup_and_check(&self, items: &mut Vec<ParsedItem>, new_name: &str) {
        items.retain(|i| {
            if matches!(i.sort, ItemSort::Type(_)) {
                !self.translated_type_names.contains(&i.name)
            } else {
                !self.translated_term_names.contains(&i.name)
            }
        });
        assert!(items.iter().any(|i| i.name == new_name));
    }

    fn fix_by_suggestions(ctxt: &mut FixContext<'_>) {
        while !ctxt.result.suggestions.is_empty() {
            let code = rustfix::apply_suggestions(&ctxt.code(), &ctxt.result.suggestions).unwrap();
            ctxt.update_whole(&code);
        }
    }

    fn fix_by_compiler(ctxt: &mut FixContext<'_>) {
        Self::fix_by_suggestions(ctxt);
        while !ctxt.result.uses.is_empty() {
            ctxt.add_uses();
            Self::fix_by_suggestions(ctxt);
        }
    }

    fn fix_by_llm(&self, ctxt: &mut FixContext<'_>, whole: bool) {
        Self::fix_by_compiler(ctxt);
        while !ctxt.result.errors.is_empty() {
            let mut fixed = false;
            for error in ctxt.result.errors.clone() {
                assert!(error.line() > ctxt.prefix_lines());
                let mut new_ctxt = if whole {
                    let fix = self.client.fix(&ctxt.code, &error.message);
                    let fix = fix
                        .split('\n')
                        .filter(|s| !s.trim().is_empty() && !s.starts_with("use "))
                        .collect::<Vec<_>>()
                        .join("\n");
                    let mut new_ctxt = ctxt.clone();
                    new_ctxt.update(fix);
                    new_ctxt
                } else {
                    let fix = self.client.fix(error.code(), &error.message);
                    let suggestion = compiler::make_suggestion(error.snippet.clone(), &fix);
                    let code = rustfix::apply_suggestions(&ctxt.code(), &[suggestion]).unwrap();
                    let mut new_ctxt = ctxt.clone();
                    new_ctxt.update_whole(&code);
                    new_ctxt
                };
                Self::fix_by_compiler(&mut new_ctxt);
                if new_ctxt.result.errors.len() < ctxt.result.errors.len() {
                    *ctxt = new_ctxt;
                    fixed = true;
                    break;
                }
            }
            if !fixed {
                break;
            }
        }
    }

    pub fn translate_names(&mut self) {
        for set in &self.type_post_order {
            for ty in set {
                let new_name = self.client.rename_type(ty.name);
                let new_name = if new_name == "Option" {
                    format!("My{}", new_name)
                } else {
                    new_name
                };
                self.new_type_names.insert(*ty, new_name);
            }
        }
        for set in &self.variable_post_order {
            for name in set {
                let new_name = self.client.rename_variable(name);
                self.new_term_names.insert(name, new_name);
            }
        }
        for set in &self.function_post_order {
            for name in set {
                let new_name = self.client.rename_function(name);
                self.new_term_names.insert(name, new_name);
            }
        }
    }

    fn translate_typedef(&self, typedef: &Typedef<'_>, new_name: &str) -> TranslationResult {
        let deps = &typedef.dependencies;

        if typedef.is_struct_alias {
            let aliased = self.translated_types.get(&deps[0].typ).unwrap().clone();
            return TranslationResult {
                copied: true,
                ..aliased
            };
        }

        let vec = self.make_replace_vec(Some(deps), None, None);
        let (code, sort) = match self
            .program
            .typedef_to_struct_string(typedef, vec, new_name)
        {
            Ok((s, sort)) => (s, sort),
            Err(mut vec) => {
                vec.push((typedef.identifier.span, new_name));
                (self.program.typedef_to_string(typedef, vec), "type")
            }
        };
        let prefix = self.make_translation_prefix(Some(deps), None, None, false);
        let translated = self.client.translate_type(&code, sort, &prefix);
        println!("{}", prefix.join("\n"));
        println!("----------------");
        println!("{}", code);
        println!("----------------");
        println!("{}", translated);
        println!("----------------");
        let items = compiler::parse(&translated).unwrap();
        TranslationResult {
            items,
            uses: vec![],
            copied: false,
        }
    }

    fn translate_struct(&self, strct: &Struct<'_>, new_name: &str) -> TranslationResult {
        let deps = &strct.dependencies;
        let mut vec = self.make_replace_vec(Some(deps), None, None);
        vec.push((
            strct.struct_type.node.identifier.as_ref().unwrap().span,
            new_name,
        ));
        let code = self.program.struct_to_string(strct, vec);
        let prefix = self.make_translation_prefix(Some(deps), None, None, false);
        let sort = if strct.strct { "struct" } else { "union" };
        let translated = self.client.translate_type(&code, sort, &prefix);
        println!("{}", prefix.join("\n"));
        println!("----------------");
        println!("{}", code);
        println!("----------------");
        println!("{}", translated);
        println!("----------------");
        let items = compiler::parse(&translated).unwrap();
        TranslationResult {
            items,
            uses: vec![],
            copied: false,
        }
    }

    fn translate_type(&self, ty: &CustomType<'_>) -> TranslationResult {
        let new_name = self.new_type_names.get(ty).unwrap();
        let mut translated = if matches!(ty.sort, TypeSort::Typedef) {
            let typedef = self.typedefs.get(ty.name).unwrap();
            self.translate_typedef(typedef, new_name)
        } else {
            let strct = self.structs.get(ty.name).unwrap();
            self.translate_struct(strct, new_name)
        };

        if translated.copied {
            return translated;
        }

        self.dedup_and_check(&mut translated.items, new_name);

        let checking_prefix = self.checking_code(true);
        println!("{}", checking_prefix);
        println!("----------------");
        println!("{}", translated.code());
        println!("----------------");
        let res = compiler::type_check(&format!("{}\n{}", checking_prefix, translated.code()));
        assert!(res.errors.is_empty());
        assert!(res.suggestions.is_empty());
        assert!(res.uses.is_empty());

        for item in &mut translated.items {
            if let ItemSort::Type(t) = &mut item.sort {
                let ds = match t.sort {
                    TypeSort::Typedef => &DERIVES[..0],
                    TypeSort::Union => &DERIVES[..2],
                    _ => &DERIVES[..],
                };
                for d in ds {
                    t.derives.insert(d.to_string());
                }
            }
        }
        Self::remove_wrong_derives(&mut translated, &checking_prefix);
        println!("{}", translated.code());
        println!("================");
        translated
    }

    fn remove_wrong_derives(translated: &mut TranslationResult, checking_prefix: &str) {
        loop {
            let mut errors =
                compiler::check_derive(&format!("{}\n{}", checking_prefix, translated.code()));
            if errors.is_empty() {
                break;
            }
            for item in &mut translated.items {
                if let ItemSort::Type(t) = &mut item.sort {
                    if let Some(ds) = errors.remove(&item.name) {
                        t.derives.retain(|d| !ds.contains(d));
                    }
                }
            }
            assert!(errors.is_empty());
        }
    }

    pub fn translate_types(&mut self) {
        for set in &self.type_post_order {
            assert_eq!(set.len(), 1);
            let typ = set.iter().next().unwrap();
            let mut translated = self.translate_type(typ);
            for i in &translated.items {
                let name = i.name.clone();
                if matches!(i.sort, ItemSort::Type(_)) {
                    self.translated_type_names.insert(name);
                } else {
                    self.translated_term_names.insert(name);
                }
            }
            self.uses.append(&mut translated.uses);
            self.translated_types.insert(*typ, translated);
        }
    }

    fn translate_variable(&self, var: &Variable<'_>, new_name: &str) -> TranslationResult {
        let tdeps = &var.type_dependencies;
        let deps = &var.dependencies;
        let mut vec = self.make_replace_vec(Some(tdeps), Some(deps), None);
        vec.push((var.identifier.span, new_name));
        let code = self.program.variable_to_string(var, vec);
        let prefix = self.make_translation_prefix(Some(tdeps), Some(deps), None, true);
        let translated = self.client.translate_variable(&code, &prefix);
        // println!("{}", prefix.join("\n"));
        // println!("----------------");
        println!("{}", code);
        println!("----------------");
        // println!("{}", translated);
        // println!("----------------");

        let mut items = compiler::parse(&translated).unwrap();
        self.dedup_and_check(&mut items, new_name);
        let item_names: BTreeSet<_> = items.iter().map(|i| i.name.clone()).collect();
        let mut translated = TranslationResult {
            items,
            uses: vec![],
            copied: false,
        };

        let checking_prefix = self.checking_code(true);
        // println!("{}", checking_prefix);
        // println!("----------------");
        // println!("{}", translated.code());
        // println!("----------------");

        let translated_code = translated.code();
        let mut ctxt = FixContext::new(&checking_prefix, translated_code.clone());
        self.fix_by_llm(&mut ctxt, true);
        translated.uses = ctxt.uses;
        if translated_code != ctxt.code {
            println!("{}", difference(&translated_code, &ctxt.code));
            println!("----------------");

            let mut fixed_items = compiler::parse(&ctxt.code).unwrap();
            self.dedup_and_check(&mut fixed_items, new_name);
            let fixed_item_names: BTreeSet<_> =
                fixed_items.iter().map(|i| i.name.clone()).collect();
            assert_eq!(item_names, fixed_item_names);
            translated.items = fixed_items;
        }

        println!("{}", translated.code());
        println!("----------------");
        for (i, e) in ctxt.result.errors.iter().enumerate() {
            println!("{} {}", i + 1, e.message);
        }
        println!("================");

        translated
    }

    pub fn translate_variables(&mut self) {
        for set in &self.variable_post_order {
            assert_eq!(set.len(), 1);
            let name = *set.iter().next().unwrap();
            let variable = self.variables.get(name).unwrap();
            let new_name = self.new_term_names.get(name).unwrap();
            let mut translated = self.translate_variable(variable, new_name);
            for i in &translated.items {
                let name = i.name.clone();
                if matches!(i.sort, ItemSort::Type(_)) {
                    self.translated_type_names.insert(name);
                } else {
                    self.translated_term_names.insert(name);
                }
            }
            self.uses.append(&mut translated.uses);
            self.translated_variables.insert(name, translated);
        }
    }

    // pub fn translate_functions(&mut self) {
    //     for set in &self.function_post_order {
    //         assert_eq!(set.len(), 1);
    //         let name = *set.iter().next().unwrap();
    //         let mut function = self.translate_function(name);
    //         self.translated_function_names.insert(name, function.name);
    //         self.translated_signatures.insert(name, function.signature);
    //         self.translated_functions.insert(name, function.translated);
    //         self.use_list.append(&mut function.uses);
    //     }
    // }

    // fn translate_function(&self, name: &str) -> TranslatedFunction {
    //     let function = self.functions.get(name).unwrap();

    //     let ids = &function.dependencies;

    //     let variable_names: Vec<_> = ids.iter().map(|x| x.node.name.as_str()).collect();
    //     let variables: Vec<_> = variable_names
    //         .iter()
    //         .map(|x| self.translated_variables.get(x).unwrap().code.as_str())
    //         .collect();

    //     let mut callee_names: Vec<_> = function
    //         .callees
    //         .iter()
    //         .map(|x| x.node.name.as_str())
    //         .collect();
    //     callee_names.sort();
    //     callee_names.dedup();
    //     let functions: Vec<_> = callee_names
    //         .iter()
    //         .map(|x| self.translated_signatures.get(x).unwrap().as_str())
    //         .collect();

    //     let use_list_for_check: Vec<_> = self
    //         .use_list
    //         .iter()
    //         .map(|s| format!("#[allow(unused_imports)] {}", s))
    //         .collect();
    //     let variables_for_check: Vec<_> = variable_names
    //         .iter()
    //         .map(|x| {
    //             let variable = self.translated_variables.get(x).unwrap();
    //             let s = variable.code.as_str();
    //             let i = s.find('=').unwrap();
    //             let ty = variable.typ.as_str();
    //             format!(
    //                 "#[deny(unused)] {} unsafe {{ std::mem::transmute([0u8; std::mem::size_of::<{}>()]) }}",
    //                 &s[..=i], ty
    //             )
    //         })
    //         .collect();
    //     let functions_for_check: Vec<_> = functions
    //         .iter()
    //         .map(|s| {
    //             let s = s.strip_suffix("{}").unwrap();
    //             format!(
    //                 "#[deny(dead_code)] #[allow(unused_variables)] {} {{ todo!() }}\n",
    //                 s
    //             )
    //         })
    //         .collect();
    //     let has_main = self.translated_functions.contains_key("main") || name == "main";
    //     let mut prefixes = vec![
    //         use_list_for_check.join("\n"),
    //         variables_for_check.join("\n"),
    //         if has_main { "" } else { "fn main() {}" }.to_string(),
    //         functions_for_check.join("\n"),
    //         "#[allow(dead_code)]".to_string(),
    //     ];
    //     prefixes.retain(|s| !s.is_empty());
    //     let prefix = format!("{}\n", prefixes.join("\n"));

    //     let new_name = self.client.rename_function(name);
    //     let mut replace_vec = vec![(
    //         c_parser::function_name_span(&function.definition.node),
    //         new_name.as_str(),
    //     )];
    //     for x in ids {
    //         replace_vec.push((
    //             x.span,
    //             self.translated_variables
    //                 .get(x.node.name.as_str())
    //                 .unwrap()
    //                 .name
    //                 .as_str(),
    //         ));
    //     }
    //     for x in &function.callees {
    //         replace_vec.push((
    //             x.span,
    //             self.translated_function_names
    //                 .get(x.node.name.as_str())
    //                 .unwrap()
    //                 .as_str(),
    //         ));
    //     }
    //     let code = self.program.function_to_string(function, replace_vec);
    //     let sigs = self
    //         .client
    //         .translate_signature(&code, &new_name, self.num_signatures);

    //     let mut sig_map = BTreeMap::new();
    //     for sig in sigs {
    //         let (sig_type, sig) = compiler::parse_signature(&sig, false, true);
    //         sig_map.entry(sig_type).or_insert(sig);
    //     }

    //     println!("{}\n........................................", prefix);
    //     println!("{}\n........................................", code);

    //     let mut candidates = vec![];
    //     for (sig_type, sig) in sig_map {
    //         println!("{}\n........................................", sig);
    //         let translated = self
    //             .client
    //             .translate_function(&code, &sig, &variables, &functions);
    //         println!("{}\n........................................", translated);

    //         // let (real_sig_type, _) = compiler::parse_signature(&translated, false, true);
    //         // if sig_type != real_sig_type {
    //         if !translated.starts_with(sig.strip_suffix("{}").unwrap()) {
    //             println!("diff\n----------------------------------------");
    //             continue;
    //         }

    //         let function = TranslatedFunction {
    //             name: new_name.clone(),
    //             prefix: prefix.clone(),
    //             signature_type: sig_type,
    //             signature: sig,
    //             translated: translated.clone(),
    //             uses: vec![],
    //             errors: vec![],
    //         };
    //         let fixed = self.fix_function(function);
    //         if translated != fixed.translated {
    //             for diff in diff::lines(&translated, &fixed.translated) {
    //                 match diff {
    //                     diff::Result::Left(l) => println!("-{}", l),
    //                     diff::Result::Both(l, _) => println!(" {}", l),
    //                     diff::Result::Right(r) => println!("+{}", r),
    //                 }
    //             }
    //         }
    //         // let proper_semipredicate = if let compiler::Type::Path(t, _) = &sig_type.ret {
    //         //     let semipredicate = match t.as_ref() {
    //         //         "Option" => Some(true),
    //         //         "Result" => Some(false),
    //         //         _ => None,
    //         //     };
    //         //     if let Some(option) = semipredicate {
    //         //         let proper = compiler::is_proper_semipredicate(&translated, option);
    //         //         if proper {
    //         //             0
    //         //         } else {
    //         //             1
    //         //         }
    //         //     } else {
    //         //         0
    //         //     }
    //         // } else {
    //         //     0
    //         // };
    //         // let score = result.errors.len() * 100 + result.warnings * 10 + proper_semipredicate;
    //         let score = fixed.errors.len();
    //         println!("{}\n----------------------------------------", score);
    //         candidates.push((fixed, score))
    //     }

    //     let best_score = candidates.iter().map(|(_, score)| *score).min().unwrap();
    //     candidates.retain(|(_, score)| *score == best_score);
    //     let (function, _) = candidates
    //         .into_iter()
    //         .max_by(|(a, _), (b, _)| self.client.compare(&a.translated, &b.translated))
    //         .unwrap();

    //     println!("{}", function.translated);
    //     for (error, _) in &function.errors {
    //         println!("{}", error);
    //     }
    //     println!("{}\n========================================", best_score);

    //     function
    // }
}

// #[derive(Clone)]
// struct TranslatedFunction {
//     prefix: String,
//     name: String,
//     #[allow(unused)]
//     signature_type: FunTySig,
//     signature: String,
//     translated: String,
//     uses: Vec<String>,
//     errors: Vec<(String, String)>,
// }

fn post_order<T: Clone + Eq + PartialOrd + Ord>(g: &BTreeMap<T, BTreeSet<T>>) -> Vec<BTreeSet<T>> {
    let (graph, mut elem_map) = graph::compute_sccs(g);
    let inv_graph = graph::inverse(&graph);
    graph::post_order(&graph, &inv_graph)
        .into_iter()
        .flatten()
        .map(|id| elem_map.remove(&id).unwrap())
        .collect()
}

#[allow(unused)]
fn difference(s1: &str, s2: &str) -> String {
    let mut result = String::new();
    for diff in diff::lines(s1, s2) {
        match diff {
            diff::Result::Left(l) => result.push_str(&format!("-{}\n", l)),
            diff::Result::Both(l, _) => result.push_str(&format!(" {}\n", l)),
            diff::Result::Right(r) => result.push_str(&format!("+{}\n", r)),
        }
    }
    result
}
