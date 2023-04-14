use std::collections::{BTreeMap, BTreeSet};

use etrace::some_or;
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
    openai_client::{OpenAIClient, OpenAIError},
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
    uses: BTreeSet<String>,
    num_signatures: usize,
}

#[derive(Debug, Clone)]
struct TranslationResult {
    items: Vec<ParsedItem>,
    uses: BTreeSet<String>,
    errors: usize,
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
        if self.errors == 0 {
            self.code()
        } else {
            self.mk_code(|i| i.get_checking_code())
        }
    }
}

#[derive(Debug, Clone)]
struct FixContext<'a> {
    uses: BTreeSet<String>,
    prefix: &'a str,
    code: String,
    names: &'a BTreeSet<String>,
    result: Option<TypeCheckingResult>,
}

impl<'a> FixContext<'a> {
    fn new(
        uses: BTreeSet<String>,
        prefix: &'a str,
        code: String,
        names: &'a BTreeSet<String>,
    ) -> Self {
        let result = compiler::type_check(&format!(
            "{}{}\n{}",
            uses.iter()
                .map(|s| s.as_str())
                .intersperse("\n")
                .collect::<String>(),
            prefix,
            code
        ));
        tracing::info!("{:?}", result);
        Self {
            uses,
            prefix,
            code,
            names,
            result,
        }
    }

    fn add_uses(&mut self) -> bool {
        let uses = std::mem::take(&mut self.result.as_mut().unwrap().uses);
        let mut updated = false;
        for u in uses {
            if self.uses.insert(u) {
                updated = true;
            }
        }
        if updated {
            self.result = compiler::type_check(&self.code());
            tracing::info!("{:?}", self.result);
        }
        updated
    }

    fn update(&mut self, code: String) {
        self.code = code;
        self.result = compiler::type_check(&self.code());
        tracing::info!("{:?}", self.result);
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
        tracing::info!("{:?}", self.result);
    }

    fn prefix_lines(&self) -> usize {
        self.uses_and_prefix().split('\n').count()
    }

    fn code(&self) -> String {
        format!("{}\n{}", self.uses_and_prefix(), self.code)
    }

    fn uses_and_prefix(&self) -> String {
        format!("{}{}", self.uses_str(), self.prefix)
    }

    fn uses_str(&self) -> String {
        self.uses
            .iter()
            .map(|s| s.as_str())
            .intersperse("\n")
            .collect()
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
        let mut uses = BTreeSet::new();
        uses.insert("extern crate once_cell;".to_string());
        uses.insert("extern crate libc;".to_string());

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
            uses,
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
                    vec.push(t.simple_code());
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

    fn take_uses(items: &mut Vec<ParsedItem>) -> BTreeSet<String> {
        items
            .drain_filter(|i| matches!(i.sort, ItemSort::Use))
            .filter_map(|i| {
                let res = compiler::type_check(&format!("{}\nfn main() {{}}", i.code));
                if res.map(|r| r.passed()).unwrap_or(false) {
                    Some(i.code.trim().to_string())
                } else {
                    None
                }
            })
            .collect()
    }

    fn fix_by_suggestions(ctxt: &mut FixContext<'_>) {
        while let Some(res) = &ctxt.result {
            if res.suggestions.is_empty() {
                break;
            }
            let code = rustfix::apply_suggestions(&ctxt.code(), &res.suggestions).unwrap();
            ctxt.update_whole(&code);
        }
    }

    fn fix_by_compiler(ctxt: &mut FixContext<'_>) {
        Self::fix_by_suggestions(ctxt);
        while let Some(res) = &ctxt.result {
            if res.uses.is_empty() {
                break;
            }
            if !ctxt.add_uses() {
                break;
            }
            Self::fix_by_suggestions(ctxt);
        }
    }

    fn fix_by_llm(&self, ctxt: &mut FixContext<'_>) {
        Self::fix_by_compiler(ctxt);
        while let Some(res) = &ctxt.result {
            if res.errors.is_empty() {
                break;
            }
            let mut fixed = false;
            for error in res.errors.clone() {
                assert!(error.line > ctxt.prefix_lines(), "{}", error.message);
                let fix = some_or!(self.client.fix(&ctxt.code, &error.message), continue);
                let mut fixed_items = some_or!(compiler::parse(&fix), continue);
                fixed_items.retain(|i| ctxt.names.contains(&i.name));
                if ctxt.names.len() != fixed_items.len() {
                    continue;
                }
                let fix = TranslationResult {
                    items: fixed_items,
                    uses: BTreeSet::new(),
                    errors: 0,
                    copied: false,
                }
                .code();
                if ctxt.code == fix {
                    continue;
                }
                let mut new_ctxt = ctxt.clone();
                new_ctxt.update(fix);
                Self::fix_by_compiler(&mut new_ctxt);
                if let Some(new_res) = &new_ctxt.result {
                    if new_res.errors.len() < res.errors.len() {
                        *ctxt = new_ctxt;
                        fixed = true;
                        break;
                    }
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
        tracing::info!("translate_typedef code\n{}", code);

        let prefix = self.make_translation_prefix(Some(deps), None, None, false);
        tracing::info!("translate_typedef prefix\n{}", prefix.join("\n"));

        let translated = self.client.translate_type(&code, sort, &prefix);
        tracing::info!("translate_typedef translated\n{}", translated);

        let items = compiler::parse(&translated).unwrap();
        TranslationResult {
            items,
            uses: BTreeSet::new(),
            errors: 0,
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
        tracing::info!("translate_struct code\n{}", code);

        let prefix = self.make_translation_prefix(Some(deps), None, None, false);
        tracing::info!("translate_struct prefix\n{}", prefix.join("\n"));

        let sort = if strct.strct { "struct" } else { "union" };
        let translated = self.client.translate_type(&code, sort, &prefix);
        tracing::info!("translate_struct translated\n{}", translated);

        let items = compiler::parse(&translated).unwrap();
        TranslationResult {
            items,
            uses: BTreeSet::new(),
            errors: 0,
            copied: false,
        }
    }

    fn translate_type(&self, ty: &CustomType<'_>) -> TranslationResult {
        let new_name = self.new_type_names.get(ty).unwrap();
        tracing::info!("translate_type: {}", new_name);

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
        let generated_uses = Self::take_uses(&mut translated.items);
        translated.uses = generated_uses;

        let checking_prefix = self.checking_code(true);
        tracing::info!("translate_type prefix\n{}", checking_prefix);
        tracing::info!("translate_type code\n{}", translated.code());

        let res = compiler::type_check(&format!("{}\n{}", checking_prefix, translated.code()));
        assert!(res.unwrap().passed());

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
        tracing::info!("translate_type code\n{}", translated.code());

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
            let translated = self.translate_type(typ);
            for i in &translated.items {
                let name = i.name.clone();
                if matches!(i.sort, ItemSort::Type(_)) {
                    self.translated_type_names.insert(name);
                } else {
                    self.translated_term_names.insert(name);
                }
            }
            for u in &translated.uses {
                self.uses.insert(u.trim().to_string());
            }
            self.translated_types.insert(*typ, translated);
        }
    }

    fn translate_variable(&self, name: &str) -> TranslationResult {
        let var = self.variables.get(name).unwrap();
        let new_name = self.new_term_names.get(name).unwrap();
        tracing::info!("translate_variable: {}", new_name);

        let tdeps = &var.type_dependencies;
        let deps = &var.dependencies;
        let mut vec = self.make_replace_vec(Some(tdeps), Some(deps), None);
        vec.push((var.identifier.span, new_name));
        let code = self.program.variable_to_string(var, vec.clone(), false);
        tracing::info!("translate_variable code\n{}", code);

        let prefix = self.make_translation_prefix(Some(tdeps), Some(deps), None, true);
        tracing::info!("translate_variable prefix\n{}", prefix.join("\n"));

        let translated = match self.client.translate_variable(&code, &prefix) {
            Ok(translated) => translated,
            Err(OpenAIError::TooLong) => {
                let code = self.program.variable_to_string(var, vec, true);
                self.client.translate_variable(&code, &prefix).unwrap()
            }
            Err(OpenAIError::NoAnswer) => panic!(),
        };
        tracing::info!("translate_variable translated\n{}", translated);

        let mut items = compiler::parse(&translated).unwrap();
        self.dedup_and_check(&mut items, new_name);
        let generated_uses = Self::take_uses(&mut items);
        let item_names: BTreeSet<_> = items.iter().map(|i| i.name.clone()).collect();
        let mut translated = TranslationResult {
            items,
            uses: generated_uses,
            errors: 0,
            copied: false,
        };
        tracing::info!("translate_variable translated\n{}", translated.code());

        let checking_prefix = self.checking_code(true);
        tracing::info!("{}", checking_prefix);

        let translated_code = translated.code();
        let mut ctxt = FixContext::new(
            translated.uses,
            &checking_prefix,
            translated_code.clone(),
            &item_names,
        );
        self.fix_by_llm(&mut ctxt);
        translated.uses = ctxt.uses;
        translated.errors = ctxt.result.as_ref().unwrap().errors.len();
        if translated_code != ctxt.code {
            tracing::info!(
                "translate_variable diff\n{}",
                difference(&translated_code, &ctxt.code)
            );

            let fixed_items = compiler::parse(&ctxt.code).unwrap();
            let fixed_item_names: BTreeSet<_> =
                fixed_items.iter().map(|i| i.name.clone()).collect();
            assert_eq!(item_names, fixed_item_names);
            translated.items = fixed_items;
        }
        for e in &ctxt.result.unwrap().errors {
            tracing::info!("translate_variable error\n{}", e.message);
        }

        translated
    }

    pub fn translate_variables(&mut self) {
        for set in &self.variable_post_order {
            assert_eq!(set.len(), 1);
            let name = *set.iter().next().unwrap();
            let translated = self.translate_variable(name);
            for i in &translated.items {
                let name = i.name.clone();
                if matches!(i.sort, ItemSort::Type(_)) {
                    self.translated_type_names.insert(name);
                } else {
                    self.translated_term_names.insert(name);
                }
            }
            for u in &translated.uses {
                self.uses.insert(u.trim().to_string());
            }
            self.translated_variables.insert(name, translated);
        }
    }

    pub fn translate_functions(&mut self) {
        for set in &self.function_post_order {
            assert_eq!(set.len(), 1);
            let name = *set.iter().next().unwrap();
            let translated = self.translate_function(name);
            for i in &translated.items {
                let name = i.name.clone();
                if matches!(i.sort, ItemSort::Type(_)) {
                    self.translated_type_names.insert(name);
                } else {
                    self.translated_term_names.insert(name);
                }
            }
            for u in &translated.uses {
                self.uses.insert(u.trim().to_string());
            }
            self.translated_functions.insert(name, translated);
        }
    }

    fn translate_function(&self, name: &str) -> TranslationResult {
        let func = self.functions.get(name).unwrap();
        let new_name = self.new_term_names.get(name).unwrap();
        tracing::info!("translate_function: {}", new_name);

        let tdeps = &func.type_dependencies;
        let deps = &func.dependencies;
        let callees = &func.callees;
        let mut vec = self.make_replace_vec(Some(tdeps), Some(deps), Some(callees));
        vec.push((func.identifier.span, new_name));
        let code = self.program.function_to_string(func, vec.clone());
        println!("translate_function code\n{}", code);

        let prefix = self.make_translation_prefix(Some(tdeps), Some(deps), Some(callees), true);
        tracing::info!("translate_function prefix\n{}", prefix.join("\n"));

        let sigs = self
            .client
            .translate_signature(&code, new_name, &prefix, self.num_signatures);
        tracing::info!("translate_function sigs\n{}", sigs.join("\n"));
        let mut sig_map = BTreeMap::new();
        for sig in sigs {
            let s = sig.replace("->", "");
            if s.chars().filter(|c| *c == '<').count() != s.chars().filter(|c| *c == '>').count() {
                continue;
            }
            let mut parsed_items = compiler::parse(&format!("{}{{}}", sig)).unwrap();
            assert_eq!(parsed_items.len(), 1);
            let item = parsed_items.pop().unwrap();
            assert_eq!(&item.name, new_name);
            if let ItemSort::Function(f) = item.sort {
                sig_map
                    .entry(f.normalized_signature_ty)
                    .or_insert(f.normalized_signature);
            } else {
                panic!()
            };
        }
        let param_len = func.params;
        if sig_map.keys().any(|sig| sig.params.len() <= param_len) {
            sig_map.retain(|sig, _| sig.params.len() <= param_len);
        }
        println!("translate_function sigs");
        for s in sig_map.values() {
            println!("{}", s);
        }

        let checking_prefix = self.checking_code(true);
        tracing::info!("translate_function checking_prefix\n{}", checking_prefix);

        let mut candidates = vec![];
        for sig in sig_map.into_values() {
            let translated = self.client.translate_function(&code, &sig, &prefix);

            let mut items = compiler::parse(&translated).unwrap();
            self.dedup_and_check(&mut items, new_name);
            Self::take_uses(&mut items);
            let item_names: BTreeSet<_> = items.iter().map(|i| i.name.clone()).collect();
            let mut translated = TranslationResult {
                items,
                uses: BTreeSet::new(),
                errors: 0,
                copied: false,
            };
            tracing::info!("translate_function translated\n{}", translated.code());

            let translated_code = translated.code();
            let mut ctxt = FixContext::new(
                translated.uses,
                &checking_prefix,
                translated_code.clone(),
                &item_names,
            );
            self.fix_by_llm(&mut ctxt);
            let res = some_or!(ctxt.result, continue);
            translated.uses = ctxt.uses;
            translated.errors = res.errors.len();
            if translated_code != ctxt.code {
                println!(
                    "translate_function diff\n{}",
                    difference(&translated_code, &ctxt.code)
                );

                let fixed_items = compiler::parse(&ctxt.code).unwrap();
                let fixed_item_names: BTreeSet<_> =
                    fixed_items.iter().map(|i| i.name.clone()).collect();
                assert_eq!(item_names, fixed_item_names);
                translated.items = fixed_items;
            }

            println!("translate_function translated\n{}", translated.code());
            for (i, e) in res.errors.iter().enumerate() {
                println!("{} {}", i + 1, e.message);
            }
            candidates.push(translated);
        }

        let best = candidates.iter().map(|c| c.errors).min().unwrap();
        candidates.retain(|c| c.errors == best);
        for (i, c) in candidates.iter().enumerate() {
            println!("translate_function candidate {}\n{}", i + 1, c.code());
        }
        let translated = candidates
            .into_iter()
            .max_by(|c1, c2| self.client.compare(&c1.code(), &c2.code()))
            .unwrap();
        println!("translate_function\n{}", translated.code());
        translated
    }
}

fn post_order<T: Clone + Eq + PartialOrd + Ord>(g: &BTreeMap<T, BTreeSet<T>>) -> Vec<BTreeSet<T>> {
    let (graph, mut elem_map) = graph::compute_sccs(g);
    let inv_graph = graph::inverse(&graph);
    graph::post_order(&graph, &inv_graph)
        .into_iter()
        .flatten()
        .map(|id| elem_map.remove(&id).unwrap())
        .collect()
}

fn difference(s1: &str, s2: &str) -> String {
    let mut result = String::new();
    for (i, diff) in diff::lines(s1, s2).iter().enumerate() {
        if i != 0 {
            result.push('\n');
        }
        let line = match diff {
            diff::Result::Left(l) => format!("-{}", l),
            diff::Result::Both(l, _) => format!(" {}", l),
            diff::Result::Right(r) => format!("+{}", r),
        };
        result.push_str(&line);
    }
    result
}
