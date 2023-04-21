use std::{
    collections::{BTreeMap, BTreeSet},
    sync::RwLock,
};

use etrace::some_or;
use futures::{future, FutureExt};
use lang_c::{
    ast::Identifier,
    span::{Node, Span},
};

use crate::{
    c_parser::{
        self, CustomType, Function, Program, Struct, TypeDependency, TypeSort, Typedef, Variable,
    },
    compiler::{self, ItemSort, ParsedItem, TypeCheckingResult},
    graph,
    graph::Id,
    openai_client::OpenAIClient,
};

#[derive(Clone, Copy, Debug)]
pub struct Config {
    pub try_multiple_signatures: bool,
    pub provide_signatures: bool,
    pub fix_errors: bool,
}

pub struct Translator<'ast> {
    program: &'ast Program,
    typedefs: BTreeMap<&'ast str, Typedef<'ast>>,
    structs: BTreeMap<&'ast str, Struct<'ast>>,
    variables: BTreeMap<&'ast str, Variable<'ast>>,
    functions: BTreeMap<&'ast str, Function<'ast>>,

    custom_types: Vec<CustomType<'ast>>,
    transitive_types: BTreeMap<CustomType<'ast>, BTreeSet<CustomType<'ast>>>,
    term_types: BTreeMap<&'ast str, BTreeSet<CustomType<'ast>>>,

    type_graph: BTreeMap<Id, BTreeSet<Id>>,
    type_elem_map: BTreeMap<Id, BTreeSet<CustomType<'ast>>>,
    variable_graph: BTreeMap<Id, BTreeSet<Id>>,
    variable_elem_map: BTreeMap<Id, BTreeSet<&'ast str>>,
    function_graph: BTreeMap<Id, BTreeSet<Id>>,
    function_elem_map: BTreeMap<Id, BTreeSet<&'ast str>>,

    client: OpenAIClient,

    new_type_names: BTreeMap<CustomType<'ast>, String>,
    new_term_names: BTreeMap<&'ast str, String>,

    inner: RwLock<TranslatorInner<'ast>>,

    config: Config,
}

#[derive(Default)]
pub struct TranslatorInner<'ast> {
    translated_type_names: BTreeSet<String>,
    translated_term_names: BTreeSet<String>,

    translated_types: BTreeMap<CustomType<'ast>, TranslationResult>,
    translated_variables: BTreeMap<&'ast str, TranslationResult>,
    translated_functions: BTreeMap<&'ast str, TranslationResult>,
}

impl<'ast> TranslatorInner<'ast> {
    fn existing_names(&self) -> BTreeSet<&str> {
        self.translated_type_names
            .iter()
            .chain(&self.translated_term_names)
            .map(|s| s.as_str())
            .collect()
    }
}

#[derive(Debug, Clone)]
struct TranslationResult {
    items: Vec<ParsedItem>,
    errors: usize,
    copied: bool,
    signature_only: bool,
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

    #[allow(unused)]
    fn simple_code(&self) -> String {
        self.mk_code(|i| i.get_simple_code())
    }

    #[allow(unused)]
    fn checking_code(&self) -> String {
        self.mk_code(|i| i.get_checking_code())
    }
}

#[derive(Debug)]
struct DependencyPrefixes {
    translation_prefix: Vec<String>,
    checking_prefix: String,
    signature_checking_prefix: String,
}

impl DependencyPrefixes {
    fn new(translated_deps: &[&ParsedItem]) -> Self {
        let translation_prefix = translated_deps
            .iter()
            .map(|i| i.get_simple_code())
            .collect();

        let checking_prefix = std::iter::once(PREAMBLE.to_string())
            .chain(translated_deps.iter().map(|i| i.get_checking_code()))
            .chain(std::iter::once("fn main() {}".to_string()))
            .intersperse("\n".to_string())
            .collect();

        let signature_checking_prefix = std::iter::once(PREAMBLE.to_string())
            .chain(
                translated_deps
                    .iter()
                    .filter(|i| matches!(i.sort, ItemSort::Type(_)))
                    .map(|i| i.get_checking_code()),
            )
            .chain(std::iter::once("fn main() {}".to_string()))
            .intersperse("\n".to_string())
            .collect();

        Self {
            translation_prefix,
            checking_prefix,
            signature_checking_prefix,
        }
    }
}

#[derive(Debug, Clone)]
struct FixContext<'a> {
    prefix: &'a str,
    code: String,
    names: &'a BTreeSet<String>,
    result: Option<TypeCheckingResult>,
}

impl<'a> FixContext<'a> {
    fn new(prefix: &'a str, code: String, names: &'a BTreeSet<String>) -> Self {
        let mut this = Self {
            prefix,
            code,
            names,
            result: None,
        };
        this.check();
        this
    }

    fn check(&mut self) {
        self.result = compiler::type_check(&self.code());
        tracing::info!("{:?}", self.result);
        if let Some(res) = &self.result {
            let prefix_lines = self.prefix_lines();
            for error in &res.errors {
                assert!(error.line > prefix_lines, "{}", error.message);
            }
        }
    }

    fn add_uses<'i, I: IntoIterator<Item = &'i String>>(&mut self, uses: I) -> bool {
        let prefix: String = std::iter::once(PREAMBLE)
            .chain(uses.into_iter().map(|s| s.as_str()))
            .collect();
        let new_code = compiler::resolve_imports(&self.code, &prefix).unwrap();
        if self.code == new_code {
            return false;
        }
        assert!(compiler::parse(&new_code).is_some());
        self.code = new_code;
        self.check();
        true
    }

    fn add_trait_uses<'i, I: IntoIterator<Item = &'i String>>(&mut self, uses: I) -> bool {
        let new_code = compiler::add_trait_uses(&self.code, uses).unwrap();
        if self.code == new_code {
            return false;
        }
        assert!(compiler::parse(&new_code).is_some());
        self.code = new_code;
        self.check();
        true
    }

    fn update(&mut self, code: String) {
        self.code = code;
        self.check();
    }

    fn update_whole(&mut self, code: &str) {
        let code = code
            .strip_prefix(self.prefix)
            .unwrap_or_else(|| panic!("{}\n{}", self.prefix, code))
            .strip_prefix('\n')
            .unwrap()
            .to_string();
        self.update(code);
    }

    fn prefix_lines(&self) -> usize {
        self.prefix.split('\n').count()
    }

    fn code(&self) -> String {
        format!("{}\n{}", self.prefix, self.code)
    }
}

static PREAMBLE: &str = "extern crate once_cell;extern crate libc;";

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
    pub fn new(program: &'ast Program, client: OpenAIClient, config: Config) -> Self {
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
        let custom_types = cg.keys().copied().collect();
        let (type_graph, type_elem_map) = graph::compute_sccs(&cg);
        let transitive_types = graph::transitive_closure(cg);
        let term_types = variables
            .iter()
            .map(|(name, variable)| (*name, &variable.type_dependencies))
            .chain(
                functions
                    .iter()
                    .map(|(name, variable)| (*name, &variable.type_dependencies)),
            )
            .map(|(name, tdeps)| (name, tdeps.iter().map(|t| t.typ).collect()))
            .collect();

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
        let (variable_graph, variable_elem_map) = graph::compute_sccs(&cg);

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
        let (function_graph, function_elem_map) = graph::compute_sccs(&cg);

        let inner = TranslatorInner::default();

        Self {
            program,
            typedefs,
            structs,
            variables,
            functions,
            custom_types,
            transitive_types,
            term_types,
            type_graph,
            type_elem_map,
            variable_graph,
            variable_elem_map,
            function_graph,
            function_elem_map,
            client,
            new_type_names: BTreeMap::new(),
            new_term_names: BTreeMap::new(),
            inner: RwLock::new(inner),
            config,
        }
    }

    pub fn signature_only(&self) -> Vec<&str> {
        let inner = self.inner.read().unwrap();
        inner
            .translated_variables
            .iter()
            .chain(&inner.translated_functions)
            .filter_map(|(name, res)| {
                if res.signature_only {
                    Some(*name)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn errors(&self) -> usize {
        let inner = self.inner.read().unwrap();
        inner
            .translated_types
            .values()
            .chain(inner.translated_variables.values())
            .chain(inner.translated_functions.values())
            .map(|res| res.errors)
            .sum()
    }

    fn existing_names(&self) -> BTreeSet<String> {
        let inner = self.inner.read().unwrap();
        inner
            .existing_names()
            .into_iter()
            .chain(self.new_type_names.keys().map(|ty| ty.name))
            .chain(self.new_term_names.keys().copied())
            .map(|s| s.to_string())
            .collect()
    }

    #[inline]
    fn mk_code<F>(&self, f: F) -> String
    where F: FnMut(&TranslationResult) -> String {
        let inner = self.inner.read().unwrap();
        let mut v: Vec<_> = std::iter::once(PREAMBLE.to_string())
            .chain(
                inner
                    .translated_types
                    .values()
                    .chain(inner.translated_variables.values())
                    .chain(inner.translated_functions.values())
                    .filter(|r| !r.copied)
                    .map(f),
            )
            .collect();
        v.push("fn main() {}".to_string());
        v.join("\n")
    }

    pub fn code(&self) -> String {
        self.mk_code(|r| r.code())
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

    fn collect_dependencies(
        &self,
        types: Option<&[TypeDependency<'_>]>,
        vars: Option<&[&Node<Identifier>]>,
        funcs: Option<&[&Node<Identifier>]>,
    ) -> DependencyPrefixes {
        let mut dep_types = vec![];
        let mut dep_vars = vec![];
        let mut dep_funcs = vec![];

        if let Some(types) = types {
            for t in types {
                dep_types.push(&t.typ);
            }
        }

        if let Some(vars) = vars {
            for v in vars {
                let name = v.node.name.as_str();
                dep_vars.push(name);
                let ts = some_or!(self.term_types.get(name), continue);
                for t in ts {
                    dep_types.push(t);
                }
            }
        }

        if let Some(funcs) = funcs {
            for f in funcs {
                let name = f.node.name.as_str();
                dep_funcs.push(name);
                let ts = some_or!(self.term_types.get(name), continue);
                for t in ts {
                    dep_types.push(t);
                }
            }
        }

        for t in dep_types.clone() {
            for t in self.transitive_types.get(t).unwrap() {
                dep_types.push(t);
            }
        }

        let inner = self.inner.read().unwrap();
        let mut items: BTreeMap<(u8, &str), &ParsedItem> = BTreeMap::new();

        for item in dep_types
            .into_iter()
            .flat_map(|x| inner.translated_types.get(x))
            .chain(
                dep_vars
                    .into_iter()
                    .flat_map(|x| inner.translated_variables.get(x)),
            )
            .chain(
                dep_funcs
                    .into_iter()
                    .flat_map(|x| inner.translated_functions.get(x)),
            )
            .flat_map(|x| &x.items)
        {
            let n = match &item.sort {
                ItemSort::Type(_) => 0,
                ItemSort::Variable(_) => 1,
                ItemSort::Function(_) => 2,
                _ => panic!(),
            };
            items.insert((n, &item.name), item);
        }

        let deps: Vec<_> = items.into_values().collect();
        DependencyPrefixes::new(&deps)
    }

    fn take_uses(items: &mut Vec<ParsedItem>) -> Vec<String> {
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
            let suggs: Vec<_> = res
                .errors
                .iter()
                .filter_map(|error| error.suggestion())
                .collect();
            if suggs.is_empty() {
                break;
            }
            let mut suggestions = vec![];
            for s in suggs {
                if !s
                    .iter()
                    .any(|s1| suggestions.iter().any(|s2| compiler::overlap(s1, s2)))
                {
                    suggestions.append(&mut s.clone());
                }
            }
            suggestions.sort_by_key(|s| s.snippets[0].range.start);
            let code = rustfix::apply_suggestions(&ctxt.code(), &suggestions).unwrap();
            ctxt.update_whole(&code);
        }
    }

    fn fix_by_uses(ctxt: &mut FixContext<'_>) {
        Self::fix_by_suggestions(ctxt);
        while let Some(res) = &ctxt.result {
            let uses: BTreeSet<_> = res
                .errors
                .iter()
                .flat_map(|error| error.new_use())
                .map(|s| s.to_string())
                .collect();
            if uses.is_empty() {
                break;
            }
            if !ctxt.add_uses(&uses) {
                break;
            }
            Self::fix_by_suggestions(ctxt);
        }
    }

    fn fix_by_trait_uses(ctxt: &mut FixContext<'_>) {
        Self::fix_by_uses(ctxt);
        while let Some(res) = &ctxt.result {
            let uses: BTreeSet<_> = res
                .errors
                .iter()
                .flat_map(|error| error.trait_use())
                .map(|s| s.to_string())
                .collect();
            if uses.is_empty() {
                break;
            }
            if !ctxt.add_trait_uses(&uses) {
                break;
            }
            Self::fix_by_uses(ctxt);
        }
    }

    async fn fix_by_llm(&self, ctxt: &mut FixContext<'_>, is_func: bool) {
        Self::fix_by_trait_uses(ctxt);
        let mut failed = BTreeSet::new();
        while let Some(res) = &ctxt.result {
            if res.errors.is_empty() {
                break;
            }

            let current_errors = res.errors.len();
            let msgs: BTreeSet<_> = res
                .errors
                .iter()
                .filter_map(|e| {
                    if failed.contains(&e.message) {
                        None
                    } else {
                        Some(e.message.clone())
                    }
                })
                .collect();

            let futures = msgs.clone().into_iter().map(|msg| {
                async {
                    let msg = msg;
                    let fix = self.client.fix(&ctxt.code, &msg).await.ok()?;
                    let mut fixed_items = compiler::parse(&fix)?;
                    fixed_items.retain(|i| ctxt.names.contains(&i.name));
                    if ctxt.names.len() != fixed_items.len() {
                        return None;
                    }
                    let fix = TranslationResult {
                        items: fixed_items,
                        errors: 0,
                        copied: false,
                        signature_only: false,
                    }
                    .code();
                    if ctxt.code == fix {
                        return None;
                    }
                    if ctxt
                        .code
                        .split('\n')
                        .count()
                        .abs_diff(fix.split('\n').count())
                        >= 10
                    {
                        return None;
                    }
                    if is_func {
                        let (_, info) = some_or!(compiler::parse_signature(&fix), return None);
                        let sig = &info.signature;
                        let sig_checking_code = format!("{}{}{{todo!()}}", ctxt.prefix, sig);
                        let result =
                            some_or!(compiler::type_check(&sig_checking_code), return None);
                        if !result.passed() {
                            return None;
                        }
                    }
                    let mut new_ctxt = ctxt.clone();
                    new_ctxt.update(fix);
                    Self::fix_by_trait_uses(&mut new_ctxt);
                    Some(new_ctxt)
                }
                .boxed()
            });
            let results = future::join_all(futures).await;
            let (successes, failures): (Vec<_>, _) = results
                .into_iter()
                .zip(msgs)
                .map(|(new_ctxt, error)| {
                    let new_res = new_ctxt
                        .as_ref()
                        .and_then(|new_ctxt| new_ctxt.result.as_ref());
                    let new_errors = new_res
                        .map(|new_res| new_res.errors.len())
                        .unwrap_or(current_errors);
                    (new_ctxt, new_errors, error)
                })
                .partition(|(_, new_errors, _)| *new_errors < current_errors);

            for (_, _, msg) in failures {
                failed.insert(msg);
            }

            if let Some((new_ctxt, _, _)) = successes
                .into_iter()
                .min_by_key(|(_, new_errors, _)| *new_errors)
            {
                *ctxt = new_ctxt.unwrap();
            } else {
                break;
            }
        }
    }

    pub async fn translate_names(&mut self) {
        let type_names = future::join_all(
            self.custom_types
                .iter()
                .map(|ty| self.client.rename_type(ty.name)),
        )
        .await;

        for (ty, new_name) in self.custom_types.iter().zip(type_names) {
            let new_name = if new_name == "Option" {
                format!("My{}", new_name)
            } else {
                new_name
            };
            self.new_type_names.insert(*ty, new_name);
        }

        let var_names = future::join_all(
            self.variables
                .keys()
                .map(|var| self.client.rename_variable(var)),
        )
        .await;
        for (var, new_name) in self.variables.keys().zip(var_names) {
            self.new_term_names.insert(*var, new_name);
        }

        let func_names = future::join_all(
            self.functions
                .keys()
                .map(|func| self.client.rename_function(func)),
        )
        .await;
        for (func, new_name) in self.functions.keys().zip(func_names) {
            let new_name = if new_name == "main" {
                format!("my_{}", new_name)
            } else {
                new_name
            };
            self.new_term_names.insert(*func, new_name);
        }
    }

    async fn translate_typedef(
        &self,
        typedef: &Typedef<'_>,
        new_name: &str,
    ) -> (TranslationResult, DependencyPrefixes) {
        let deps = &typedef.dependencies;
        let prefixes = self.collect_dependencies(Some(deps), None, None);
        tracing::info!(
            "translate_typedef translation_prefix ({})\n{}",
            new_name,
            prefixes.translation_prefix.join("\n")
        );

        if typedef.is_struct_alias {
            let inner = self.inner.read().unwrap();
            let aliased = inner.translated_types.get(&deps[0].typ).unwrap().clone();
            let translated = TranslationResult {
                copied: true,
                ..aliased
            };
            return (translated, prefixes);
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
        tracing::info!("translate_typedef code ({})\n{}", new_name, code);

        let translated = self
            .client
            .translate_type(&code, sort, &prefixes.translation_prefix)
            .await;
        tracing::info!(
            "translate_typedef translated ({})\n{}",
            new_name,
            translated
        );

        let items = compiler::parse(&translated).unwrap();
        let translated = TranslationResult {
            items,
            errors: 0,
            copied: false,
            signature_only: false,
        };
        (translated, prefixes)
    }

    async fn translate_struct(
        &self,
        strct: &Struct<'_>,
        new_name: &str,
    ) -> (TranslationResult, DependencyPrefixes) {
        let deps = &strct.dependencies;
        let prefixes = self.collect_dependencies(Some(deps), None, None);
        tracing::info!(
            "translate_struct translation_prefix ({})\n{}",
            new_name,
            prefixes.translation_prefix.join("\n")
        );

        let mut vec = self.make_replace_vec(Some(deps), None, None);
        vec.push((
            strct.struct_type.node.identifier.as_ref().unwrap().span,
            new_name,
        ));
        let code = self.program.struct_to_string(strct, vec);
        tracing::info!("translate_struct code ({})\n{}", new_name, code);

        let sort = if strct.strct { "struct" } else { "union" };
        let translated = self
            .client
            .translate_type(&code, sort, &prefixes.translation_prefix)
            .await;
        tracing::info!("translate_struct translated ({})\n{}", new_name, translated);

        let items = compiler::parse(&translated).unwrap();
        let translated = TranslationResult {
            items,
            errors: 0,
            copied: false,
            signature_only: false,
        };
        (translated, prefixes)
    }

    async fn translate_type(&self, ty: &CustomType<'_>) -> TranslationResult {
        let new_name = self.new_type_names.get(ty).unwrap();
        tracing::info!("translate_type: {}", new_name);

        let (mut translated, prefixes) = if matches!(ty.sort, TypeSort::Typedef) {
            let typedef = self.typedefs.get(ty.name).unwrap();
            self.translate_typedef(typedef, new_name).await
        } else {
            let strct = self.structs.get(ty.name).unwrap();
            self.translate_struct(strct, new_name).await
        };

        if translated.copied {
            return translated;
        }

        let uses = Self::take_uses(&mut translated.items);

        let mut existing_name = self.existing_names();
        existing_name.remove(new_name);
        translated
            .items
            .retain(|i| !existing_name.contains(&i.name) && matches!(i.sort, ItemSort::Type(_)));
        assert!(
            translated.items.iter().any(|i| i.name == *new_name),
            "{}",
            new_name
        );

        let translated_code = translated.code();
        let translated_code = compiler::resolve_imports(&translated_code, &uses.join("")).unwrap();
        translated.items = compiler::parse(&translated_code).unwrap();

        tracing::info!(
            "translate_type checking_prefix ({})\n{}",
            new_name,
            &prefixes.checking_prefix
        );
        tracing::info!("translate_type code ({})\n{}", new_name, translated.code());

        let item_names: BTreeSet<_> = translated.items.iter().map(|i| i.name.clone()).collect();
        let translated_code = translated.code();
        let mut ctxt = FixContext::new(
            &prefixes.checking_prefix,
            translated_code.clone(),
            &item_names,
        );
        self.fix_by_llm(&mut ctxt, false).await;
        assert!(ctxt.result.as_ref().unwrap().passed());
        translated.errors = ctxt.result.as_ref().unwrap().errors.len();
        if translated_code != ctxt.code {
            tracing::info!(
                "translate_type diff ({})\n{}",
                new_name,
                difference(&translated_code, &ctxt.code)
            );

            let fixed_items = compiler::parse(&ctxt.code).unwrap();
            let fixed_item_names: BTreeSet<_> =
                fixed_items.iter().map(|i| i.name.clone()).collect();
            assert_eq!(item_names, fixed_item_names);
            translated.items = fixed_items;
        }

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
        Self::remove_wrong_derives(&mut translated, &prefixes.checking_prefix);
        tracing::info!(
            "translate_type result ({})\n{}",
            new_name,
            translated.code()
        );
        println!("type: {} ({})", new_name, translated.errors);

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

    pub async fn translate_types(&mut self) {
        let mut graph = self.type_graph.clone();
        let mut futures = vec![];

        loop {
            let mut new_futures: Vec<_> = graph
                .drain_filter(|_, s| s.is_empty())
                .map(|(id, _)| self.type_elem_map.get(&id).unwrap())
                .map(|set| {
                    async {
                        assert_eq!(set.len(), 1);
                        let typ = set.first().unwrap();
                        let translated = self.translate_type(typ).await;
                        (typ, translated)
                    }
                    .boxed()
                })
                .collect();
            futures.append(&mut new_futures);

            if futures.is_empty() {
                break;
            }

            let ((typ, translated), _, remaining) = future::select_all(futures).await;
            futures = remaining;

            let id = self
                .type_elem_map
                .iter()
                .find_map(|(id, set)| if set.contains(typ) { Some(id) } else { None })
                .unwrap();
            for ids in graph.values_mut() {
                ids.remove(id);
            }

            let mut inner = self.inner.write().unwrap();
            for i in &translated.items {
                let name = i.name.clone();
                if matches!(i.sort, ItemSort::Type(_)) {
                    inner.translated_type_names.insert(name);
                } else {
                    inner.translated_term_names.insert(name);
                }
            }
            inner.translated_types.insert(*typ, translated);
        }
    }

    async fn translate_variable(&self, name: &str) -> TranslationResult {
        let var = self.variables.get(name).unwrap();
        let new_name = self.new_term_names.get(name).unwrap();
        tracing::info!("translate_variable: {}", new_name);

        let tdeps = &var.type_dependencies;
        let deps = &var.dependencies;
        let prefixes = self.collect_dependencies(Some(tdeps), Some(deps), None);

        let mut vec = self.make_replace_vec(Some(tdeps), Some(deps), None);
        vec.push((var.identifier.span, new_name));
        let code = self.program.variable_to_string(var, vec.clone(), false);
        tracing::info!("translate_variable code ({})\n{}", new_name, code);

        let empty = vec![];
        let translation_prefix = if self.config.provide_signatures {
            &prefixes.translation_prefix
        } else {
            &empty
        };
        tracing::info!(
            "translate_variable translation_prefix ({})\n{}",
            new_name,
            translation_prefix.join("\n")
        );

        let (translated, signature_only) = match self
            .client
            .translate_variable(&code, translation_prefix)
            .await
        {
            Ok(translated) => (translated, false),
            Err(_) => {
                let code = self.program.variable_to_string(var, vec, true);
                (
                    self.client
                        .translate_variable(&code, translation_prefix)
                        .await
                        .unwrap(),
                    true,
                )
            }
        };
        tracing::info!(
            "translate_variable translated ({})\n{}",
            new_name,
            translated
        );

        let mut items = compiler::parse(&translated).unwrap();
        let uses = Self::take_uses(&mut items);
        let item = items
            .into_iter()
            .find(|item| item.name == *new_name && matches!(item.sort, ItemSort::Variable(_)))
            .unwrap();

        let translated = item.get_code();
        let translated = compiler::resolve_imports(&translated, &uses.join("")).unwrap();
        let item = compiler::parse_one(&translated).unwrap();

        let translated =
            compiler::resolve_free_types(&item.get_code(), &prefixes.signature_checking_prefix)
                .unwrap();
        let item = compiler::parse_one(&translated).unwrap();
        let items = vec![item];
        let item_names: BTreeSet<_> = items.iter().map(|i| i.name.clone()).collect();

        let mut translated = TranslationResult {
            items,
            errors: 0,
            copied: false,
            signature_only,
        };
        tracing::info!(
            "translate_variable translated ({})\n{}",
            new_name,
            translated.code()
        );

        tracing::info!(
            "translate_variable checking_prefix ({})\n{}",
            new_name,
            prefixes.checking_prefix
        );

        let translated_code = translated.code();
        let mut ctxt = FixContext::new(
            &prefixes.checking_prefix,
            translated_code.clone(),
            &item_names,
        );
        if self.config.fix_errors {
            self.fix_by_llm(&mut ctxt, false).await;
            if translated_code != ctxt.code {
                let fixed_items = compiler::parse(&ctxt.code).unwrap();
                let fixed_item_names: BTreeSet<_> =
                    fixed_items.iter().map(|i| i.name.clone()).collect();
                assert_eq!(item_names, fixed_item_names);

                tracing::info!(
                    "translate_variable diff ({})\n{}",
                    new_name,
                    difference(&translated_code, &ctxt.code)
                );
                translated.items = fixed_items;
            }
        }
        translated.errors = ctxt.result.as_ref().unwrap().errors.len();

        tracing::info!(
            "translate_variable result ({})\n{}",
            new_name,
            translated.code()
        );
        for e in &ctxt.result.unwrap().errors {
            tracing::info!("translate_variable error ({})\n{}", new_name, e.message);
        }
        println!("variable: {} ({})", new_name, translated.errors);

        translated
    }

    pub async fn translate_variables(&mut self) {
        let mut graph = self.variable_graph.clone();
        let mut futures = vec![];

        loop {
            let mut new_futures: Vec<_> = graph
                .drain_filter(|_, s| s.is_empty())
                .map(|(id, _)| self.variable_elem_map.get(&id).unwrap())
                .map(|set| {
                    async {
                        assert_eq!(set.len(), 1);
                        let var = *set.first().unwrap();
                        let translated = self.translate_variable(var).await;
                        (var, translated)
                    }
                    .boxed()
                })
                .collect();
            futures.append(&mut new_futures);

            if futures.is_empty() {
                break;
            }

            let ((var, translated), _, remaining) = future::select_all(futures).await;
            futures = remaining;

            let id = self
                .variable_elem_map
                .iter()
                .find_map(|(id, set)| if set.contains(var) { Some(id) } else { None })
                .unwrap();
            for ids in graph.values_mut() {
                ids.remove(id);
            }

            let mut inner = self.inner.write().unwrap();
            for i in &translated.items {
                let name = i.name.clone();
                if matches!(i.sort, ItemSort::Type(_)) {
                    inner.translated_type_names.insert(name);
                } else {
                    inner.translated_term_names.insert(name);
                }
            }
            inner.translated_variables.insert(var, translated);
        }
    }

    async fn translate_function(&self, name: &str) -> TranslationResult {
        let func = self.functions.get(name).unwrap();
        let new_name = self.new_term_names.get(name).unwrap();
        tracing::info!("translate_function: {}", new_name);

        let tdeps = &func.type_dependencies;
        let deps = &func.dependencies;
        let callees = &func.callees;
        let mut vec = self.make_replace_vec(Some(tdeps), Some(deps), Some(callees));
        let in_spans = c_parser::find_names(func.definition, "in");
        for span in in_spans {
            vec.push((span, "in_data"));
        }
        vec.push((func.identifier.span, new_name));
        let code = self.program.function_to_string(func, vec.clone());
        tracing::info!("translate_function code ({})\n{}", new_name, code);

        let prefixes = self.collect_dependencies(Some(tdeps), Some(deps), Some(callees));
        let empty = vec![];
        let translation_prefix = if self.config.provide_signatures {
            &prefixes.translation_prefix
        } else {
            &empty
        };
        tracing::info!(
            "translate_function translation_prefix ({})\n{}",
            new_name,
            translation_prefix.join("\n")
        );

        let sigs = self
            .client
            .translate_signature(&code, new_name, translation_prefix, 3)
            .await;
        tracing::info!(
            "translate_function sigs ({})\n{}",
            new_name,
            sigs.join("\n")
        );

        let mut sig_map = BTreeMap::new();
        for sig in sigs {
            let s = sig.replace("->", "");
            if s.chars().filter(|c| *c == '<').count() != s.chars().filter(|c| *c == '>').count() {
                continue;
            }
            let sig = format!("{}{{todo!()}}", sig);
            let sig = some_or!(compiler::normalize_result(&sig), continue);
            let sig = some_or!(
                compiler::resolve_free_types(&sig, &prefixes.signature_checking_prefix),
                continue
            );
            let result = some_or!(
                compiler::type_check(&format!("{}{}", prefixes.signature_checking_prefix, sig)),
                continue
            );
            if !result.passed() {
                continue;
            }
            let (parsed_name, info) = some_or!(compiler::parse_signature(&sig), continue);
            assert_eq!(&parsed_name, new_name);
            sig_map.entry(info.signature_ty).or_insert(info.signature);
        }
        assert!(!sig_map.is_empty());
        let param_len = func.params;
        if sig_map.keys().any(|sig| sig.params.len() <= param_len) {
            sig_map.retain(|sig, _| sig.params.len() <= param_len);
        }
        tracing::info!(
            "translate_function sigs ({})\n{}",
            new_name,
            sig_map
                .values()
                .map(|s| s.as_str())
                .collect::<Vec<_>>()
                .join("\n")
        );

        tracing::info!(
            "translate_function checking_prefix ({})\n{}",
            new_name,
            prefixes.checking_prefix
        );

        let candidates = future::join_all(
            sig_map
                .values()
                .map(|sig| self.try_signature(sig, new_name, &code, &prefixes)),
        )
        .await;
        let mut candidates = candidates.into_iter().flatten().collect::<Vec<_>>();
        if candidates.is_empty() {
            let code = self.program.function_to_signature_string(func, vec.clone());
            let new_candidates = future::join_all(
                sig_map
                    .values()
                    .map(|sig| self.try_signature(sig, new_name, &code, &prefixes)),
            )
            .await;
            candidates = new_candidates.into_iter().flatten().collect::<Vec<_>>();
            for c in &mut candidates {
                c.signature_only = true;
            }
        }

        let min_errors = candidates.iter().map(|c| c.errors).min().expect(new_name);
        candidates.retain(|c| c.errors == min_errors);
        for (i, c) in candidates.iter().enumerate() {
            tracing::info!(
                "translate_function candidate {} ({})\n{}",
                i + 1,
                new_name,
                c.code()
            );
        }
        candidates.reverse();
        let mut best = candidates.pop().unwrap();
        while let Some(cand) = candidates.pop() {
            if self.client.compare(&best.code(), &cand.code()).await == std::cmp::Ordering::Less {
                best = cand;
            }
        }
        tracing::info!("translate_function result ({})\n{}", new_name, best.code());
        println!("function: {} ({})", new_name, best.errors);
        best
    }

    async fn try_signature(
        &self,
        sig: &str,
        new_name: &str,
        code: &str,
        prefixes: &DependencyPrefixes,
    ) -> Option<TranslationResult> {
        let translated = self
            .client
            .translate_function(code, sig, &prefixes.translation_prefix)
            .await
            .ok()?;

        let mut items = compiler::parse(&translated)?;
        let uses = Self::take_uses(&mut items);
        let item = items
            .into_iter()
            .find(|item| item.name == *new_name && matches!(item.sort, ItemSort::Function(_)))
            .unwrap();

        let translated = item.get_code();
        let translated = compiler::resolve_imports(&translated, &uses.join("")).unwrap();
        let item = compiler::parse_one(&translated).unwrap();

        let translated =
            compiler::resolve_free_types(&item.get_code(), &prefixes.signature_checking_prefix)
                .unwrap();
        let item = compiler::parse_one(&translated).unwrap();
        let items = vec![item];
        let item_names: BTreeSet<_> = items.iter().map(|i| i.name.clone()).collect();

        let mut translated = TranslationResult {
            items,
            errors: 0,
            copied: false,
            signature_only: false,
        };
        tracing::info!(
            "try_signature translated ({})\n{}\n{}",
            new_name,
            sig,
            translated.code()
        );

        let translated_code = translated.code();
        let mut ctxt = FixContext::new(
            &prefixes.checking_prefix,
            translated_code.clone(),
            &item_names,
        );
        if self.config.fix_errors {
            self.fix_by_llm(&mut ctxt, true).await;
            if translated_code != ctxt.code {
                let fixed_items = compiler::parse(&ctxt.code).expect(&ctxt.code);
                let fixed_item_names: BTreeSet<_> =
                    fixed_items.iter().map(|i| i.name.clone()).collect();
                assert_eq!(item_names, fixed_item_names);

                tracing::info!(
                    "try_signature diff ({})\n{}\n{}",
                    new_name,
                    sig,
                    difference(&translated_code, &ctxt.code)
                );
                translated.items = fixed_items;
            }
        }
        let res = ctxt.result?;
        translated.errors = res.errors.len();

        tracing::info!(
            "try_signature translated ({})\n{}\n{}",
            new_name,
            sig,
            translated.code()
        );
        for (i, e) in res.errors.iter().enumerate() {
            tracing::info!(
                "try_signature error {} ({})\n{}\n{}",
                i + 1,
                new_name,
                sig,
                e.message
            );
        }
        Some(translated)
    }

    pub async fn translate_functions(&mut self) {
        let mut graph = self.function_graph.clone();
        let mut futures = vec![];

        loop {
            let mut new_futures: Vec<_> = graph
                .drain_filter(|_, s| s.is_empty())
                .map(|(id, _)| self.function_elem_map.get(&id).unwrap())
                .map(|set| {
                    async {
                        assert_eq!(set.len(), 1);
                        let func = *set.first().unwrap();
                        let translated = self.translate_function(func).await;
                        (func, translated)
                    }
                    .boxed()
                })
                .collect();
            futures.append(&mut new_futures);

            if futures.is_empty() {
                break;
            }

            let ((func, translated), _, remaining) = future::select_all(futures).await;
            futures = remaining;

            let id = self
                .function_elem_map
                .iter()
                .find_map(|(id, set)| if set.contains(func) { Some(id) } else { None })
                .unwrap();
            for ids in graph.values_mut() {
                ids.remove(id);
            }

            let mut inner = self.inner.write().unwrap();
            for i in &translated.items {
                let name = i.name.clone();
                if matches!(i.sort, ItemSort::Type(_)) {
                    inner.translated_type_names.insert(name);
                } else {
                    inner.translated_term_names.insert(name);
                }
            }
            inner.translated_functions.insert(func, translated);
        }
    }
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
