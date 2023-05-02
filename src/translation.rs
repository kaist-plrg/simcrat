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
        self, CustomType, Enum, Function, Program, Struct, TypeDependency, TypeSort, Typedef,
        Variable,
    },
    compiler::{self, FunTySig, FunctionInfo, ItemSort, ParsedItem, Type, TypeCheckingResult},
    graph,
    graph::Id,
    openai_client::{tokens_in_str, OpenAIClient},
};

#[derive(Clone, Copy, Debug)]
pub struct Config {
    pub try_multiple_signatures: bool,
    pub provide_signatures: bool,
    pub fix_errors: bool,
    pub quiet: bool,
}

pub struct Translator<'ast> {
    program: &'ast Program,
    typedefs: BTreeMap<&'ast str, Typedef<'ast>>,
    structs: BTreeMap<&'ast str, Struct<'ast>>,
    enums: BTreeMap<&'ast str, Enum<'ast>>,
    variables: BTreeMap<&'ast str, Variable<'ast>>,
    protos: BTreeMap<&'ast str, Variable<'ast>>,
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
    called_functions: BTreeSet<&'ast str>,

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

    signatures: BTreeMap<String, SigDiff>,
}

impl<'ast> TranslatorInner<'ast> {
    fn existing_names(&self) -> BTreeSet<&str> {
        self.translated_type_names
            .iter()
            .chain(&self.translated_term_names)
            .map(|s| s.as_str())
            .collect()
    }

    fn add_names(&mut self, translated: &TranslationResult) {
        for i in &translated.items {
            let name = i.name.clone();
            if matches!(i.sort, ItemSort::Type(_)) {
                self.translated_type_names.insert(name);
            } else {
                self.translated_term_names.insert(name);
            }
        }
    }
}

#[derive(Debug, Clone)]
struct TranslationResult {
    items: Vec<ParsedItem>,
    errors: usize,
    too_long: bool,
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
        self.prefix.lines().count()
    }

    fn code(&self) -> String {
        format!("{}\n{}", self.prefix, self.code)
    }
}

#[allow(unused)]
#[derive(Debug, Clone)]
struct SigDiff {
    name: String,
    c_signature: String,
    c_signature_ty: FunTySig,
    rust_signature: String,
    rust_signature_ty: FunTySig,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub enum SigDiffReason {
    Option,
    String,
    Vec,
    Tuple,
    File,
    Never,
    Generic,
    VoidPtr,
    Ptr,
    Etc,
}

static REASONS: [SigDiffReason; 10] = [
    SigDiffReason::Option,
    SigDiffReason::String,
    SigDiffReason::Vec,
    SigDiffReason::Tuple,
    SigDiffReason::VoidPtr,
    SigDiffReason::File,
    SigDiffReason::Never,
    SigDiffReason::Generic,
    SigDiffReason::Ptr,
    SigDiffReason::Etc,
];

static PREAMBLE: &str = "extern crate once_cell;extern crate libc;";

impl<'ast> Translator<'ast> {
    pub fn new(program: &'ast Program, client: OpenAIClient, config: Config) -> Self {
        let typedefs = program.typedefs();
        let structs = program.structs();
        let enums = program.enums();
        let (variables, protos) = program.variables();
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
        for name in enums.keys() {
            cg.insert(CustomType::mk_enum(name), BTreeSet::new());
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
            .chain(protos.keys().map(|name| (*name, BTreeSet::new())))
            .collect();
        let (function_graph, function_elem_map) = graph::compute_sccs(&cg);
        let called_functions = cg.values().flatten().copied().collect();

        let inner = TranslatorInner::default();

        Self {
            program,
            typedefs,
            structs,
            enums,
            variables,
            protos,
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
            called_functions,
            client,
            new_type_names: BTreeMap::new(),
            new_term_names: BTreeMap::new(),
            inner: RwLock::new(inner),
            config,
        }
    }

    pub fn show_information(&self) {
        let _lines = self.lines_of_code();
        let types = self.typedefs.len() + self.structs.len() + self.enums.len();
        let variables = self.variables.len();
        let _protos = self.protos.len();
        let functions = self.functions.len();
        println!("{}\t{}\t{}", types, variables, functions);
    }

    pub fn show_openai_stat(&self) {
        println!(
            "{} {} {}",
            self.client.request_tokens(),
            self.client.response_tokens(),
            self.client.response_time(),
        );
    }

    fn lines_of_code(&self) -> usize {
        let spans: Vec<_> = self
            .typedefs
            .values()
            .map(|t| (t.path, t.declaration.span))
            .chain(self.structs.values().map(|s| (s.path, s.declaration.span)))
            .chain(self.enums.values().map(|s| (s.path, s.declaration.span)))
            .chain(
                self.variables
                    .values()
                    .chain(self.protos.values())
                    .map(|v| (v.path, v.declaration.span)),
            )
            .chain(self.functions.values().map(|f| (f.path, f.definition.span)))
            .collect();
        let mut span_map: BTreeMap<_, Vec<_>> = BTreeMap::new();
        for (path, span) in spans {
            span_map.entry(path).or_default().push(span);
        }
        let mut lines = 0;
        for (path, spans) in &mut span_map {
            spans.sort_by_key(|span| span.start);
            spans.reverse();
            let mut nspans = vec![spans.pop().unwrap()];
            while let Some(span) = spans.pop() {
                let span2 = nspans.last_mut().unwrap();
                if c_parser::overlap(span, *span2) {
                    span2.end = span.end.max(span2.end);
                } else {
                    nspans.push(span);
                }
            }
            lines += nspans
                .into_iter()
                .map(|span| self.program.lines(path, span))
                .sum::<usize>();
        }
        lines
    }

    pub fn too_long(&self) -> Vec<&str> {
        let inner = self.inner.read().unwrap();
        inner
            .translated_variables
            .iter()
            .chain(&inner.translated_functions)
            .filter_map(|(name, res)| if res.too_long { Some(*name) } else { None })
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

    pub fn compare_signatures(&self, detail: bool) {
        let name_map: BTreeMap<_, _> = self
            .new_type_names
            .iter()
            .map(|(ty, name)| (name.as_str(), ty.name))
            .collect();

        let mut inner = self.inner.write().unwrap();
        let signatures = std::mem::take(&mut inner.signatures);
        drop(inner);

        let diffs: Vec<_> = signatures
            .into_values()
            .filter_map(|mut diff| {
                if diff.rust_signature_ty.params.is_empty()
                    && diff.rust_signature_ty.ret == compiler::UNIT
                {
                    return None;
                }
                diff.c_signature_ty = diff.c_signature_ty.into_c(&name_map);
                diff.rust_signature_ty = diff.rust_signature_ty.into_c(&name_map);
                if diff.c_signature_ty == diff.rust_signature_ty {
                    return None;
                }
                Some(diff)
            })
            .collect();

        let total = diffs.len();

        let mut diff_map = BTreeMap::new();
        for diff in diffs {
            let mut reasons = BTreeSet::new();
            let len_diff = diff.c_signature_ty.params.len() != diff.rust_signature_ty.params.len();
            for rp in std::iter::once(&diff.rust_signature_ty.ret)
                .chain(diff.rust_signature_ty.params.iter())
            {
                if rp.contains("Option") || rp.contains("Result") {
                    reasons.insert(SigDiffReason::Option);
                }
                if rp.contains("String")
                    || rp.contains("OsString")
                    || rp.contains("str")
                    || rp.contains("OsStr")
                {
                    reasons.insert(SigDiffReason::String);
                }
                if rp.contains("Vec") || rp.contains_slice() {
                    reasons.insert(SigDiffReason::Vec);
                }
                if rp.contains_tuple() {
                    reasons.insert(SigDiffReason::Tuple);
                }
                if rp.contains("File") || rp.contains("Path") {
                    reasons.insert(SigDiffReason::File);
                }
                if rp == &Type::Never {
                    reasons.insert(SigDiffReason::Never);
                }
            }
            for (cp, rp) in std::iter::once(&diff.c_signature_ty.ret)
                .chain(diff.c_signature_ty.params.iter())
                .zip(
                    std::iter::once(&diff.rust_signature_ty.ret)
                        .chain(diff.rust_signature_ty.params.iter()),
                )
            {
                if cp != rp {
                    if cp.contains("FILE") && !rp.contains("FILE") {
                        reasons.insert(SigDiffReason::File);
                    }
                    if cp.is_void_ptr() && !rp.is_void_ptr() {
                        reasons.insert(SigDiffReason::VoidPtr);
                    }
                    if rp == &Type::Ptr(Box::new(cp.clone()), true)
                        || cp == &Type::Ptr(Box::new(rp.clone()), true)
                    {
                        reasons.insert(SigDiffReason::Ptr);
                    }
                }
                if len_diff {
                    break;
                }
            }
            if diff.rust_signature_ty.generic {
                reasons.insert(SigDiffReason::Generic);
            }
            if reasons.is_empty() {
                reasons.insert(SigDiffReason::Etc);
            }
            for reason in reasons {
                diff_map
                    .entry(reason)
                    .or_insert_with(Vec::new)
                    .push(diff.clone());
            }
        }

        if detail {
            for r in &REASONS {
                let diffs = some_or!(diff_map.get(r), continue);
                println!("{:?} ({})", r, diffs.len());
                for diff in diffs {
                    let i = diff.c_signature.find('{').unwrap();
                    println!(
                        "{}\n{}\n",
                        diff.c_signature[..i].replace('\n', " "),
                        diff.rust_signature
                    );
                }
                println!("====================");
            }
        } else {
            let result: String = std::iter::once(total)
                .chain(
                    REASONS
                        .iter()
                        .map(|r| diff_map.get(r).map(|v| v.len()).unwrap_or(0)),
                )
                .map(|n| n.to_string())
                .intersperse(" ".to_string())
                .collect();
            println!("{}", result);
        }
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

    fn dedup_items(item_vec: Vec<&ParsedItem>) -> Vec<&ParsedItem> {
        let mut items: BTreeMap<(u8, &str), &ParsedItem> = BTreeMap::new();

        for item in item_vec {
            let n = match &item.sort {
                ItemSort::Type(_) => 0,
                ItemSort::Variable(_) => 1,
                ItemSort::Function(_) => 2,
                _ => panic!(),
            };
            items.insert((n, &item.name), item);
        }

        items.into_values().collect()
    }

    pub fn code(&self) -> String {
        let inner = self.inner.read().unwrap();
        let items = Self::dedup_items(
            inner
                .translated_types
                .values()
                .chain(inner.translated_variables.values())
                .chain(inner.translated_functions.values())
                .flat_map(|t| &t.items)
                .collect(),
        );
        std::iter::once(PREAMBLE.to_string())
            .chain(items.into_iter().map(|i| i.get_code()))
            .chain(std::iter::once("fn main() {}".to_string()))
            .intersperse("\n".to_string())
            .collect()
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
        let deps = Self::dedup_items(
            dep_types
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
                .collect(),
        );
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
        let mut fixes = 0;
        while let Some(res) = &ctxt.result {
            let prefix_lines = ctxt.prefix_lines();
            let suggs: Vec<_> = res
                .errors
                .iter()
                .filter_map(|error| error.suggestion())
                .filter(|suggs| {
                    suggs.iter().all(|sugg| {
                        sugg.snippets
                            .iter()
                            .all(|snippet| snippet.line_range.start.line > prefix_lines)
                    })
                })
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
            let mut new_ctxt = ctxt.clone();
            new_ctxt.update_whole(&code);
            if new_ctxt.result.is_none() {
                break;
            }
            *ctxt = new_ctxt;
            fixes += 1;
            if fixes >= 20 {
                break;
            }
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
                .filter_map(|s| {
                    if ctxt.code.contains(s) {
                        None
                    } else {
                        Some(s.to_string())
                    }
                })
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

            let code_tokens = tokens_in_str(&ctxt.code);
            if code_tokens >= 1900 {
                break;
            }
            let max_len = 1900 - code_tokens;
            let msg_tokens: Vec<_> = res
                .errors
                .iter()
                .filter_map(|e| {
                    let msg = e.message.as_str();
                    let tokens = tokens_in_str(msg);
                    if tokens > max_len {
                        None
                    } else {
                        Some((msg, tokens))
                    }
                })
                .collect();
            if msg_tokens.is_empty() {
                break;
            }

            let mut msgs = vec![];
            let mut current_msg = "".to_string();
            let mut current_tokens = 0;
            for (msg, tokens) in &msg_tokens {
                if current_tokens + tokens > max_len {
                    assert!(!current_msg.is_empty());
                    assert!(current_tokens > 0);
                    if !failed.contains(&current_msg) {
                        msgs.push(current_msg);
                    }
                    current_msg = "".to_string();
                    current_tokens = 0;
                }

                if !current_msg.is_empty() {
                    current_msg += "\n\n";
                }
                current_msg += msg;
                current_tokens += tokens;
            }
            if !current_msg.is_empty() && !failed.contains(&current_msg) {
                msgs.push(current_msg);
            }

            let futures = msgs.clone().into_iter().map(|msg| {
                async {
                    let msg = msg;
                    let fix = self.client.fix(&ctxt.code, &msg).await?;
                    let mut fixed_items = compiler::parse(&fix)?;
                    fixed_items.retain(|i| ctxt.names.contains(&i.name));
                    if ctxt.names.len() != fixed_items.len() {
                        return None;
                    }
                    let fix = TranslationResult {
                        items: fixed_items,
                        errors: 0,
                        too_long: false,
                    }
                    .code();
                    if ctxt.code == fix {
                        return None;
                    }
                    if ctxt.code.lines().count().abs_diff(fix.lines().count()) >= 10 {
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

            let current_errors = res.errors.len();
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
        let aliased: Vec<_> = self
            .custom_types
            .iter()
            .filter_map(|ty| {
                if ty.sort == TypeSort::Typedef {
                    let typedef = self.typedefs.get(ty.name).unwrap();
                    if typedef.is_struct_alias {
                        return Some((typedef.dependencies[0].typ, ty));
                    }
                }
                None
            })
            .collect();
        let alias_set: BTreeSet<_> = aliased.iter().map(|(ty, _)| *ty).collect();
        let custom_types: Vec<_> = self
            .custom_types
            .iter()
            .filter(|ty| !alias_set.contains(ty))
            .collect();

        let type_names = future::join_all(
            custom_types
                .iter()
                .map(|ty| self.client.rename_type(ty.name)),
        )
        .await;

        for (ty, new_name) in custom_types.into_iter().zip(type_names) {
            let new_name = match new_name.as_str() {
                "Option" => "MyOption",
                "Cmatrix" => "Matrix",
                "Crec" => "Rec",
                "Crecid" => "RecId",
                "PollLoopArgsT" => "PollLoopArgs",
                new_name => new_name,
            }
            .to_string();
            self.new_type_names.insert(*ty, new_name);
        }

        for (struct_ty, ty) in aliased {
            let new_name = self.new_type_names.get(ty).unwrap().clone();
            self.new_type_names.insert(struct_ty, new_name);
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

        let proto_names = future::join_all(
            self.protos
                .keys()
                .map(|proto| self.client.rename_function(proto)),
        )
        .await;
        for (proto, new_name) in self.protos.keys().zip(proto_names) {
            self.new_term_names.insert(*proto, new_name);
        }

        let func_names = future::join_all(
            self.functions
                .keys()
                .map(|func| self.client.rename_function(func)),
        )
        .await;
        for (func, new_name) in self.functions.keys().zip(func_names) {
            let new_name = if new_name == "main" || new_name == "loop" {
                format!("my_{}", new_name)
            } else {
                new_name
            };
            self.new_term_names.insert(*func, new_name);
        }
    }

    async fn fix_types_after_translation(
        &self,
        new_names: Vec<&str>,
        mut translated: TranslationResult,
        prefixes: DependencyPrefixes,
    ) -> TranslationResult {
        let uses = Self::take_uses(&mut translated.items);
        let mut existing_name = self.existing_names();
        for new_name in &new_names {
            existing_name.remove(*new_name);
        }
        translated
            .items
            .retain(|i| !existing_name.contains(&i.name) && matches!(i.sort, ItemSort::Type(_)));
        for new_name in &new_names {
            assert!(
                translated.items.iter().any(|i| i.name == **new_name),
                "{}",
                new_name
            );
        }

        let translated_code = translated.code();
        let translated_code = compiler::resolve_imports(&translated_code, &uses.join("")).unwrap();
        let translated_code = compiler::resolve_free_types(
            &translated_code,
            &prefixes.signature_checking_prefix,
            self.config.quiet,
        )
        .unwrap();
        let translated_code =
            compiler::resolve_free_consts(&translated_code, self.config.quiet).unwrap();

        translated.items = compiler::parse(&translated_code).unwrap();

        tracing::info!(
            "translate_type checking_prefix ({:?})\n{}",
            new_names,
            &prefixes.checking_prefix
        );
        tracing::info!(
            "translate_type code ({:?})\n{}",
            new_names,
            translated.code()
        );

        let item_names: BTreeSet<_> = translated.items.iter().map(|i| i.name.clone()).collect();
        let translated_code = translated.code();
        let mut ctxt = FixContext::new(
            &prefixes.checking_prefix,
            translated_code.clone(),
            &item_names,
        );
        self.fix_by_llm(&mut ctxt, false).await;
        if !ctxt.result.as_ref().unwrap().passed() {
            if !self.config.quiet {
                println!("Type not translated: {:?}", new_names);
            }
            let new_code = new_names
                .iter()
                .map(|new_name| format!("type {} = usize;", new_name))
                .collect::<Vec<_>>()
                .join("\n");
            ctxt.update(new_code);
        }
        assert!(ctxt.result.as_ref().unwrap().passed());

        translated.errors = ctxt.result.as_ref().unwrap().errors.len();
        if translated_code != ctxt.code {
            tracing::info!(
                "translate_type diff ({:?})\n{}",
                new_names,
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
                    TypeSort::Typedef => &compiler::DERIVES[..0],
                    TypeSort::Union => &compiler::DERIVES[..2],
                    _ => &compiler::DERIVES[..],
                };
                for d in ds {
                    t.derives.insert(d.to_string());
                }
            }
        }
        Self::remove_wrong_derives(&mut translated, &prefixes.checking_prefix);
        tracing::info!(
            "translate_type result ({:?})\n{}",
            new_names,
            translated.code()
        );
        if !self.config.quiet {
            println!("type: {:?}", new_names);
        }

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

    async fn translate_type(&self, tys: Vec<&CustomType<'_>>) -> TranslationResult {
        let mut all_deps = vec![];
        let mut all_code = vec![];
        let mut sorts = BTreeSet::new();
        let mut new_names = vec![];
        for ty in tys {
            let new_name = self.new_type_names.get(ty).unwrap().as_str();
            new_names.push(new_name);
            match ty.sort {
                TypeSort::Typedef => {
                    let typedef = self.typedefs.get(ty.name).unwrap();
                    assert!(!typedef.is_struct_alias);
                    let deps = &typedef.dependencies;
                    all_deps.append(&mut deps.clone());

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
                    all_code.push(code);
                    sorts.insert(sort);
                }
                TypeSort::Struct | TypeSort::Union => {
                    let strct = self.structs.get(ty.name).unwrap();
                    let deps = &strct.dependencies;
                    all_deps.append(&mut deps.clone());

                    let mut vec = self.make_replace_vec(Some(deps), None, None);
                    vec.push((
                        strct.struct_type.node.identifier.as_ref().unwrap().span,
                        new_name,
                    ));
                    let code = self.program.struct_to_string(strct, vec);
                    let sort = if strct.strct { "struct" } else { "union" };
                    all_code.push(code);
                    sorts.insert(sort);
                }
                TypeSort::Enum => {
                    let enm = self.enums.get(ty.name).unwrap();
                    let vec = vec![(
                        enm.enum_type.node.identifier.as_ref().unwrap().span,
                        new_name,
                    )];
                    let code = self.program.enum_to_string(enm, vec);
                    all_code.push(code);
                    sorts.insert("enum");
                }
            }
        }

        let prefixes = self.collect_dependencies(Some(&all_deps), None, None);
        tracing::info!(
            "translate_type translation_prefix ({:?})\n{}",
            new_names,
            prefixes.translation_prefix.join("\n")
        );

        let code = all_code.join("\n");
        tracing::info!(
            "translate_type translation_prefix ({:?})\n{}",
            new_names,
            code,
        );

        let sort = if sorts.len() == 1 {
            sorts.into_iter().next().unwrap()
        } else {
            "type"
        };
        let translated = self
            .client
            .translate_type(&code, sort, &prefixes.translation_prefix)
            .await;
        tracing::info!(
            "translate_type translated ({:?})\n{}",
            new_names,
            translated
        );

        let items = compiler::parse(&translated).unwrap();
        let translated = TranslationResult {
            items,
            errors: 0,
            too_long: false,
        };
        self.fix_types_after_translation(new_names, translated, prefixes)
            .await
    }

    pub async fn translate_types(&self) {
        let mut graph = self.type_graph.clone();
        let mut futures = vec![];

        loop {
            let mut new_futures: Vec<_> = graph
                .drain_filter(|_, s| s.is_empty())
                .map(|(id, _)| self.type_elem_map.get(&id).unwrap())
                .map(|set| {
                    async {
                        let (non_aliases, mut aliases): (Vec<_>, _) = set
                            .iter()
                            .map(|ty| {
                                if ty.sort == TypeSort::Typedef {
                                    let typedef = self.typedefs.get(ty.name).unwrap();
                                    if typedef.is_struct_alias {
                                        return (ty, Some(typedef.dependencies[0].typ));
                                    }
                                }
                                (ty, None)
                            })
                            .partition(|(_, ty)| ty.is_none());
                        let non_aliases: Vec<_> =
                            non_aliases.into_iter().map(|(ty, _)| ty).collect();
                        let translated = if non_aliases.is_empty() {
                            assert!(aliases.len() == 1);
                            let inner = self.inner.read().unwrap();
                            let (_, dep) = aliases.pop().unwrap();
                            inner.translated_types.get(&dep.unwrap()).unwrap().clone()
                        } else {
                            self.translate_type(non_aliases).await
                        };
                        (set.iter().collect::<Vec<_>>(), translated)
                    }
                    .boxed()
                })
                .collect();
            futures.append(&mut new_futures);

            if futures.is_empty() {
                break;
            }

            let ((tys, translated), _, remaining) = future::select_all(futures).await;
            futures = remaining;

            let id = self
                .type_elem_map
                .iter()
                .find_map(|(id, set)| {
                    if tys.iter().any(|ty| set.contains(ty)) {
                        Some(id)
                    } else {
                        None
                    }
                })
                .unwrap();
            for ids in graph.values_mut() {
                ids.remove(id);
            }

            let mut inner = self.inner.write().unwrap();
            inner.add_names(&translated);
            for ty in tys {
                inner.translated_types.insert(*ty, translated.clone());
            }
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
        let too_long = tokens_in_str(&code) > 1500;
        let code = if too_long {
            self.program.variable_to_string(var, vec, true)
        } else {
            code
        };
        tracing::info!(
            "translate_variable code ({})\ntoo_long: {}\n{}",
            new_name,
            too_long,
            code
        );

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

        let on_failure = || {
            if !self.config.quiet {
                println!("Variable not translated: {}", new_name);
            }
            format!("const {}: usize = 0;", new_name)
        };

        let translated = self
            .client
            .translate_variable(&code, translation_prefix)
            .await
            .unwrap_or_else(on_failure);
        tracing::info!(
            "translate_variable translated ({})\n{}",
            new_name,
            translated
        );

        let mut items = compiler::parse(&translated).unwrap_or_else(|| {
            let translated = on_failure();
            compiler::parse(&translated).unwrap()
        });
        let uses = Self::take_uses(&mut items);
        let item = items
            .into_iter()
            .find(|item| item.name == *new_name && matches!(item.sort, ItemSort::Variable(_)))
            .unwrap();

        let translated = item.get_code();
        let translated = compiler::resolve_imports(&translated, &uses.join("")).unwrap();
        let item = compiler::parse_one(&translated).unwrap();

        let translated = compiler::resolve_free_types(
            &item.get_code(),
            &prefixes.signature_checking_prefix,
            self.config.quiet,
        )
        .unwrap();
        let item = compiler::parse_one(&translated).unwrap();

        let translated =
            compiler::resolve_sync(&item.get_code(), &prefixes.signature_checking_prefix).unwrap();
        let item = compiler::parse_one(&translated).unwrap();

        let items = vec![item];
        let item_names: BTreeSet<_> = items.iter().map(|i| i.name.clone()).collect();

        let mut translated = TranslationResult {
            items,
            errors: 0,
            too_long,
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

        let ctxt2 = FixContext::new(
            &prefixes.checking_prefix,
            translated.checking_code(),
            &item_names,
        );
        if !ctxt2.result.unwrap().passed() {
            let code = on_failure();
            let items = compiler::parse(&code).unwrap();
            translated.items = items;
            translated.errors = 0;
        }

        tracing::info!(
            "translate_variable result ({})\n{}",
            new_name,
            translated.code()
        );
        for e in &ctxt.result.unwrap().errors {
            tracing::info!("translate_variable error ({})\n{}", new_name, e.message);
        }
        if !self.config.quiet {
            println!("variable: {} ({})", new_name, translated.errors);
        }

        translated
    }

    pub async fn translate_variables(&self) {
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
            inner.add_names(&translated);
            inner.translated_variables.insert(var, translated);
        }
    }

    async fn translate_proto(&self, name: &str) -> TranslationResult {
        let proto = self.protos.get(name).unwrap();
        let new_name = self.new_term_names.get(name).unwrap();
        tracing::info!("translate_proto: {}", new_name);

        let tdeps = &proto.type_dependencies;
        let deps = &proto.dependencies;
        let mut vec = self.make_replace_vec(Some(tdeps), Some(deps), None);
        vec.push((proto.identifier.span, new_name));
        let code = self.program.variable_to_string(proto, vec, false);
        tracing::info!("translate_proto code ({})\n{}", new_name, code);

        let prefixes = self.collect_dependencies(Some(tdeps), Some(deps), None);
        let empty = vec![];
        let translation_prefix = if self.config.provide_signatures {
            &prefixes.translation_prefix
        } else {
            &empty
        };
        tracing::info!(
            "translate_proto translation_prefix ({})\n{}",
            new_name,
            translation_prefix.join("\n")
        );
        tracing::info!(
            "translate_proto checking_prefix ({})\n{}",
            new_name,
            prefixes.checking_prefix
        );

        let sig_map = self.translate_signature(&code, new_name, &prefixes).await;
        let (_, sig) = sig_map.iter().next().unwrap();
        let translated = format!("{}{{todo!()}}", sig);
        tracing::info!("translate_proto result ({})\n{}", new_name, translated);

        if !self.config.quiet {
            println!("proto: {}", new_name);
        }
        TranslationResult {
            items: vec![compiler::parse_one(&translated).unwrap()],
            errors: 0,
            too_long: false,
        }
    }

    pub async fn translate_protos(&self) {
        let translated = future::join_all(
            self.protos
                .keys()
                .map(|name| async { (*name, self.translate_proto(name).await) }.boxed()),
        )
        .await;
        for (name, translated) in translated {
            let mut inner = self.inner.write().unwrap();
            inner.add_names(&translated);
            inner.translated_functions.insert(name, translated);
        }
    }

    async fn translate_function(
        &self,
        name: &str,
        target_sig: Option<&FunctionInfo>,
    ) -> TranslationResult {
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
        let too_long = tokens_in_str(&code) > 1500;
        let code = if too_long {
            self.program.function_to_signature_string(func, vec.clone())
        } else {
            code
        };
        tracing::info!(
            "translate_function code ({})\ntoo_long: {}\n{}",
            new_name,
            too_long,
            code
        );

        let prefixes = self.collect_dependencies(Some(tdeps), Some(deps), Some(callees));
        tracing::info!(
            "translate_function translation_prefix ({})\n{}",
            new_name,
            prefixes.translation_prefix.join("\n")
        );
        tracing::info!(
            "translate_function checking_prefix ({})\n{}",
            new_name,
            prefixes.checking_prefix
        );

        let translated = if let Some(target_sig) = target_sig {
            let translated = self
                .try_signature(
                    Some(&target_sig.signature),
                    name,
                    new_name,
                    &code,
                    &prefixes,
                    too_long,
                )
                .await
                .unwrap();
            assert_eq!(translated.items.len(), 1);
            let item = &translated.items[0];
            let f = if let ItemSort::Function(f) = &item.sort {
                f
            } else {
                panic!()
            };
            assert_eq!(target_sig.signature, f.signature);
            Some(translated)
        } else if self.config.try_multiple_signatures {
            let mut sig_map = self.translate_signature(&code, new_name, &prefixes).await;
            if sig_map.is_empty() {
                if !self.config.quiet {
                    println!("Signature not translated: {}", new_name);
                }
                self.try_signature(None, name, new_name, &code, &prefixes, too_long)
                    .await
            } else {
                let param_len = func.type_signature.params.len();
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

                let candidates = future::join_all(sig_map.values().map(|sig| {
                    self.try_signature(Some(sig), name, new_name, &code, &prefixes, too_long)
                }))
                .await;
                let mut candidates = candidates.into_iter().flatten().collect::<Vec<_>>();

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
                    if self.client.compare(&best.code(), &cand.code()).await
                        == std::cmp::Ordering::Less
                    {
                        best = cand;
                    }
                }
                Some(best)
            }
        } else {
            self.try_signature(None, name, new_name, &code, &prefixes, too_long)
                .await
        };
        let mut translated = translated.unwrap_or_else(|| {
            if !self.config.quiet {
                println!("Function not translated: {}", new_name);
            }
            let code = format!("fn {}() {{todo!()}}", new_name);
            let items = compiler::parse(&code).unwrap();
            TranslationResult {
                items,
                errors: 0,
                too_long: false,
            }
        });
        translated.too_long = too_long;

        tracing::info!(
            "translate_function result ({})\n{}",
            new_name,
            translated.code()
        );

        let c_signature = self.program.function_to_signature_string(func, vec);
        let c_signature_ty = func.type_signature.clone();
        let func_info = compiler::parse_signature(&translated.code()).unwrap().1;
        let rust_signature = func_info.signature;
        let rust_signature_ty = func_info.signature_ty;
        let sig_diff = SigDiff {
            name: name.to_string(),
            c_signature,
            c_signature_ty,
            rust_signature,
            rust_signature_ty,
        };
        self.inner
            .write()
            .unwrap()
            .signatures
            .insert(name.to_string(), sig_diff);

        if !self.config.quiet {
            println!("function: {} ({})", new_name, translated.errors);
        }
        translated
    }

    async fn translate_signature(
        &self,
        code: &str,
        new_name: &str,
        prefixes: &DependencyPrefixes,
    ) -> BTreeMap<FunTySig, String> {
        let empty = vec![];
        let translation_prefix = if self.config.provide_signatures {
            &prefixes.translation_prefix
        } else {
            &empty
        };
        let sigs = self
            .client
            .translate_signature(code, new_name, translation_prefix, 3)
            .await;
        tracing::info!("translate_signature ({})\n{}", new_name, sigs.join("\n"));

        let mut sig_map = BTreeMap::new();
        for sig in sigs {
            let s = sig.replace("->", "");
            if s.chars().filter(|c| *c == '<').count() != s.chars().filter(|c| *c == '>').count() {
                continue;
            }
            let sig = format!("{}{{todo!()}}", sig);
            let mut parsed = some_or!(compiler::parse(&sig), continue);
            parsed.retain(|item| matches!(item.sort, ItemSort::Function(_)));
            let item = if let Some(item) = parsed.iter().find(|item| item.name == new_name) {
                item.clone()
            } else {
                let sig = compiler::rename_function(&sig, new_name).unwrap();
                compiler::parse(&sig).unwrap().pop().unwrap()
            };
            let sig = item.get_code();
            let sig = compiler::rename_params(&sig).unwrap();
            let sig = some_or!(compiler::normalize_result(&sig), continue);
            let sig = some_or!(
                compiler::resolve_free_types(
                    &sig,
                    &prefixes.signature_checking_prefix,
                    self.config.quiet
                ),
                continue
            );
            let mut item_names = BTreeSet::new();
            item_names.insert(new_name.to_string());
            let mut ctxt = FixContext::new(&prefixes.checking_prefix, sig, &item_names);
            Self::fix_by_uses(&mut ctxt);
            let result = some_or!(ctxt.result, continue);
            if !result.passed() {
                continue;
            }
            let (parsed_name, info) = some_or!(compiler::parse_signature(&ctxt.code), continue);
            assert_eq!(&parsed_name, new_name);
            sig_map.entry(info.signature_ty).or_insert(info.signature);
        }
        sig_map
    }

    async fn try_signature(
        &self,
        sig: Option<&str>,
        name: &str,
        new_name: &str,
        code: &str,
        prefixes: &DependencyPrefixes,
        too_long: bool,
    ) -> Option<TranslationResult> {
        let empty = vec![];
        let translation_prefix = if self.config.provide_signatures && !too_long {
            &prefixes.translation_prefix
        } else {
            &empty
        };
        let translated = self
            .client
            .translate_function(code, sig, translation_prefix)
            .await?;

        let mut items = compiler::parse(&translated)?;
        let uses = Self::take_uses(&mut items);
        let item = items
            .into_iter()
            .find(|item| item.name == *new_name && matches!(item.sort, ItemSort::Function(_)))
            .unwrap();

        let translated = item.get_code();
        let translated = compiler::rename_params(&translated).unwrap();
        let translated = compiler::resolve_imports(&translated, &uses.join("")).unwrap();
        let item = compiler::parse_one(&translated).unwrap();

        let translated = compiler::resolve_free_types(
            &item.get_code(),
            &prefixes.signature_checking_prefix,
            self.config.quiet,
        )
        .unwrap();
        let item = compiler::parse_one(&translated).unwrap();
        let items = vec![item];
        let item_names: BTreeSet<_> = items.iter().map(|i| i.name.clone()).collect();

        let mut translated = TranslationResult {
            items,
            errors: 0,
            too_long: false,
        };
        tracing::info!(
            "try_signature translated ({})\n{}\n{}",
            new_name,
            sig.as_ref().unwrap_or(&""),
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
                    sig.as_ref().unwrap_or(&""),
                    difference(&translated_code, &ctxt.code)
                );
                translated.items = fixed_items;
            }
        }
        let res = ctxt.result?;
        translated.errors = res.errors.len();

        if self.called_functions.contains(name) {
            let (_, info) = some_or!(compiler::parse_signature(&translated.code()), return None);
            let sig_checking_code = format!("{}{}{{todo!()}}", ctxt.prefix, info.signature);
            let result = some_or!(compiler::type_check(&sig_checking_code), return None);
            if !result.passed() {
                return None;
            }
        }

        tracing::info!(
            "try_signature translated ({})\n{}\n{}",
            new_name,
            sig.as_ref().unwrap_or(&""),
            translated.code()
        );
        for (i, e) in res.errors.iter().enumerate() {
            tracing::info!(
                "try_signature error {} ({})\n{}\n{}",
                i + 1,
                new_name,
                sig.as_ref().unwrap_or(&""),
                e.message
            );
        }
        Some(translated)
    }

    async fn translate_recursive_functions(&self, names: BTreeSet<&'ast str>) {
        if !self.config.quiet && names.len() > 1 {
            println!("{:?}", names);
        }

        let mut cg: BTreeMap<_, BTreeSet<_>> = names
            .iter()
            .map(|name| {
                let func = self.functions.get(name).unwrap();
                let callees = func
                    .callees
                    .iter()
                    .map(|callee| callee.node.name.as_str())
                    .filter(|callee| names.contains(callee) && callee != name)
                    .collect();
                (*name, callees)
            })
            .collect();

        let mut sig_map = BTreeMap::new();
        while !cg.is_empty() {
            if let Some((name, _)) = cg.iter().find(|(_, callees)| callees.is_empty()) {
                self.remove_func(name);
                let target_sig = sig_map.get(name);
                let translated = self.translate_function(name, target_sig).await;

                let mut inner = self.inner.write().unwrap();
                inner.add_names(&translated);
                inner.translated_functions.insert(name, translated);

                let name = *name;
                cg.remove(&name);
                for callees in cg.values_mut() {
                    callees.remove(&name);
                }
            } else {
                let name = Self::pick_function(&mut cg);
                if !self.config.quiet {
                    println!("pick: {}", name);
                }
                let translated = self.translate_function(name, None).await;
                assert_eq!(translated.items.len(), 1);
                let f = if let ItemSort::Function(f) = &translated.items[0].sort {
                    f
                } else {
                    panic!()
                };
                sig_map.insert(name, f.clone());

                let mut inner = self.inner.write().unwrap();
                inner.translated_functions.insert(name, translated);
            }
        }
    }

    fn remove_func(&self, name: &str) {
        self.inner
            .write()
            .unwrap()
            .translated_functions
            .remove(name);
    }

    fn pick_function<'a>(cg: &mut BTreeMap<&'a str, BTreeSet<&'a str>>) -> &'a str {
        let mut scores: BTreeMap<&str, usize> = BTreeMap::new();
        for callees in cg.values() {
            if callees.len() == 1 {
                *scores.entry(callees.first().unwrap()).or_default() += 1;
            }
        }
        let (func, score) = scores.into_iter().max_by_key(|(_, score)| *score).unwrap();
        assert_ne!(score, 0);
        for callees in cg.values_mut() {
            callees.remove(func);
        }
        func
    }

    pub async fn translate_functions(&self) {
        let mut function_elem_map = self.function_elem_map.clone();
        for funcs in function_elem_map.values_mut() {
            funcs.retain(|f| self.functions.contains_key(f));
        }
        let mut graph = self.function_graph.clone();
        graph.retain(|id, _| !function_elem_map.get(id).unwrap().is_empty());
        let mut futures = vec![];

        loop {
            let mut new_futures: Vec<_> = graph
                .drain_filter(|_, s| s.is_empty())
                .map(|(id, _)| function_elem_map.get(&id).unwrap())
                .map(|set| {
                    async {
                        self.translate_recursive_functions(set.iter().copied().collect())
                            .await;
                        *set.first().unwrap()
                    }
                    .boxed()
                })
                .collect();
            futures.append(&mut new_futures);

            if futures.is_empty() {
                break;
            }

            let (func, _, remaining) = future::select_all(futures).await;
            futures = remaining;

            let id = self
                .function_elem_map
                .iter()
                .find_map(|(id, set)| if set.contains(func) { Some(id) } else { None })
                .unwrap();
            for ids in graph.values_mut() {
                ids.remove(id);
            }
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
