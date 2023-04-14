use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
    path::PathBuf,
    process::Command,
    sync::{Arc, Mutex},
};

use rustc_data_structures::sync::Lrc;
use rustc_errors::{
    emitter::Emitter, registry::Registry, translation::Translate, Applicability, DiagnosticId,
    DiagnosticMessage, FluentBundle, Handler, LazyFallbackBundle, Level, SubstitutionPart,
};
use rustc_hash::{FxHashMap, FxHashSet};
use rustc_hir::{
    intravisit::{self, Visitor},
    FnDecl, FnRetTy, GenericArg, GenericBound, ItemKind, MutTy, Mutability, Path, PathSegment,
    QPath, TraitRef, Ty, TyKind,
};
use rustc_interface::Config;
use rustc_middle::{dep_graph::DepContext, hir::nested_filter, ty::TyCtxt};
use rustc_session::{
    config::{CheckCfg, Input, Options},
    parse::ParseSess,
};
use rustc_span::{
    hygiene::{ExpnKind, MacroKind},
    source_map::{FileName, SourceMap},
    BytePos, Pos, Span, SpanData,
};
use rustfix::{LinePosition, LineRange, Replacement, Snippet, Solution, Suggestion};

use crate::c_parser::TypeSort;

#[derive(Debug, Clone)]
struct Substitution {
    parts: Vec<(SpanData, String)>,
}

#[derive(Debug, Clone)]
struct CodeSuggestion {
    substitutions: Vec<Substitution>,
    msg: String,
    applicability: Applicability,
}

#[derive(Debug, Clone)]
struct SpanLabel {
    span: SpanData,
    primary: bool,
    msg: Option<String>,
}

#[derive(Debug, Clone)]
struct MultiSpan {
    span_labels: Vec<SpanLabel>,
}

impl MultiSpan {
    fn internal_labels(&self, source_map: &SourceMap) -> Vec<&SpanLabel> {
        self.span_labels
            .iter()
            .filter(|l| {
                source_map.span_to_filename(l.span.span())
                    == FileName::Custom("main.rs".to_string())
            })
            .collect()
    }

    fn entire_span(&self, source_map: &SourceMap) -> Option<Span> {
        let span_labels = self.internal_labels(source_map);
        if span_labels.is_empty() {
            return None;
        }
        let lo = span_labels.iter().map(|l| l.span.lo).min().unwrap();
        let hi = span_labels.iter().map(|l| l.span.hi).max().unwrap();
        let span = span_labels[0].span.span().with_lo(lo).with_hi(hi);
        Some(source_map.span_extend_to_line(span))
    }

    fn primary_line(&self, source_map: &SourceMap) -> usize {
        let labels = self.internal_labels(source_map);
        let label = labels.iter().find(|l| l.primary).unwrap();
        pos_of_span(label.span.span(), source_map).0
    }
}

impl fmt::Display for WithSourceMap<'_, &MultiSpan> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut labels: BTreeMap<_, Vec<_>> = BTreeMap::new();
        for label in self.inner.internal_labels(self.source_map) {
            let span = label.span.span();
            let (line, col) = pos_of_span(span, self.source_map);
            labels
                .entry(line)
                .or_default()
                .push((col, span, label.msg.as_ref()));
        }
        let mut prev = None;
        for (line, ls) in &mut labels {
            if let Some(prev) = prev {
                writeln!(f)?;
                if prev + 1 < *line {
                    writeln!(f, "...")?;
                }
            }
            prev = Some(line);

            let span = self.source_map.span_extend_to_line(ls[0].1.shrink_to_lo());
            let line = self.source_map.span_to_snippet(span).unwrap();
            write!(f, "{}", line)?;

            ls.sort_by_key(|(col, _, _)| *col);
            let mut b = true;
            for (_, span, msg) in ls {
                if let Some(msg) = msg {
                    if msg.is_empty() {
                        continue;
                    }
                    if b {
                        write!(f, " //")?;
                        b = false;
                    }
                    if !span.is_empty() && !self.source_map.is_multiline(*span) {
                        write!(f, " {}: ", self.source_map.span_to_snippet(*span).unwrap())?;
                    };
                    write!(f, "{}.", msg)?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
struct SubDiagnostic {
    level: &'static str,
    message: Vec<String>,
    span: MultiSpan,
}

impl fmt::Display for WithSourceMap<'_, &SubDiagnostic> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "= {}: ", self.inner.level)?;
        for msg in &self.inner.message {
            writeln!(f, "{}", msg)?;
        }
        write!(
            f,
            "{}",
            WithSourceMap::new(self.source_map, &self.inner.span,)
        )
    }
}

#[derive(Debug, Clone)]
struct Diagnostic {
    code: Option<String>,
    message: Vec<String>,
    span: MultiSpan,
    children: Vec<SubDiagnostic>,
    suggestions: Vec<CodeSuggestion>,
}

impl fmt::Display for WithSourceMap<'_, &Diagnostic> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error")?;
        if let Some(code) = &self.inner.code {
            write!(f, "[{}]", code)?;
        }
        write!(f, ": ")?;
        for msg in &self.inner.message {
            writeln!(f, "{}", msg)?;
        }
        writeln!(
            f,
            "{}",
            WithSourceMap::new(self.source_map, &self.inner.span,)
        )?;
        for child in &self.inner.children {
            writeln!(f, "{}", WithSourceMap::new(self.source_map, child,))?;
        }
        Ok(())
    }
}

fn code_to_string(code: DiagnosticId) -> Option<String> {
    match code {
        DiagnosticId::Error(s) => Some(s),
        _ => None,
    }
}

struct WithSourceMap<'a, T> {
    source_map: &'a SourceMap,
    inner: T,
}

impl<'a, T> WithSourceMap<'a, T> {
    fn new(source_map: &'a SourceMap, inner: T) -> Self {
        Self { source_map, inner }
    }
}

#[derive(Default)]
struct EmitterInner {
    diagnostics: Vec<Diagnostic>,
    warning_counter: usize,
}

struct CollectingEmitter {
    inner: Arc<Mutex<EmitterInner>>,
    source_map: Lrc<SourceMap>,
    bundle: Option<Lrc<FluentBundle>>,
    fallback_bundle: LazyFallbackBundle,
}

impl CollectingEmitter {
    fn new(inner: Arc<Mutex<EmitterInner>>, source_map: Lrc<SourceMap>) -> Self {
        let bundle = get_bundle();
        let fallback_bundle = get_fallback_bundle();
        Self {
            inner,
            source_map,
            bundle,
            fallback_bundle,
        }
    }

    fn message_to_string(
        &self,
        msg: &DiagnosticMessage,
        diag: &rustc_errors::Diagnostic,
    ) -> String {
        let args = rustc_errors::translation::to_fluent_args(diag.args());
        self.translate_message(msg, &args).unwrap().into_owned()
    }

    fn substitution(&self, subst: &rustc_errors::Substitution) -> Substitution {
        let parts = subst
            .parts
            .iter()
            .map(|SubstitutionPart { span, snippet }| (span.data(), snippet.clone()))
            .collect();
        Substitution { parts }
    }

    fn code_suggestion(
        &self,
        suggestion: &rustc_errors::CodeSuggestion,
        diag: &rustc_errors::Diagnostic,
    ) -> CodeSuggestion {
        let substitutions = suggestion
            .substitutions
            .iter()
            .map(|s| self.substitution(s))
            .collect();
        let msg = self.message_to_string(&suggestion.msg, diag);
        let applicability = suggestion.applicability;
        CodeSuggestion {
            substitutions,
            msg,
            applicability,
        }
    }

    fn span_label(
        &self,
        label: rustc_errors::SpanLabel,
        diag: &rustc_errors::Diagnostic,
    ) -> SpanLabel {
        let span = label.span.data();
        let primary = label.is_primary;
        let msg = label
            .label
            .as_ref()
            .map(|msg| self.message_to_string(msg, diag));
        SpanLabel { span, primary, msg }
    }

    fn multi_span(
        &self,
        multi_span: &rustc_errors::MultiSpan,
        diag: &rustc_errors::Diagnostic,
    ) -> MultiSpan {
        let span_labels = multi_span
            .span_labels()
            .into_iter()
            .map(|l| self.span_label(l, diag))
            .collect();
        MultiSpan { span_labels }
    }

    fn sub_diagnostic(
        &self,
        sub_diag: &rustc_errors::SubDiagnostic,
        diag: &rustc_errors::Diagnostic,
    ) -> SubDiagnostic {
        let level = sub_diag.level.to_str();
        let message = sub_diag
            .message
            .iter()
            .map(|(msg, _)| self.message_to_string(msg, diag))
            .collect();
        let span = self.multi_span(&sub_diag.span, diag);
        SubDiagnostic {
            level,
            message,
            span,
        }
    }

    fn diagnostic(&self, diag: &rustc_errors::Diagnostic) -> Diagnostic {
        let message = diag
            .message
            .iter()
            .map(|(msg, _)| self.message_to_string(msg, diag))
            .collect();
        let code = diag.code.clone().and_then(code_to_string);
        let span = self.multi_span(&diag.span, diag);
        let children = diag
            .children
            .iter()
            .map(|c| self.sub_diagnostic(c, diag))
            .collect();
        let empty = vec![];
        let suggestions = diag
            .suggestions
            .as_ref()
            .unwrap_or(&empty)
            .iter()
            .map(|s| self.code_suggestion(s, diag))
            .collect();
        Diagnostic {
            message,
            code,
            span,
            children,
            suggestions,
        }
    }
}

impl Translate for CollectingEmitter {
    fn fluent_bundle(&self) -> Option<&Lrc<FluentBundle>> {
        self.bundle.as_ref()
    }

    fn fallback_fluent_bundle(&self) -> &FluentBundle {
        &self.fallback_bundle
    }
}

impl Emitter for CollectingEmitter {
    fn emit_diagnostic(&mut self, diag: &rustc_errors::Diagnostic) {
        #[cfg(test)]
        println!("{:?}", diag);
        match diag.level() {
            Level::Error { .. } => {
                let diag = self.diagnostic(diag);
                tracing::info!("{:?}", diag);
                self.inner.lock().unwrap().diagnostics.push(diag);
            }
            Level::Warning(_) => self.inner.lock().unwrap().warning_counter += 1,
            Level::Help => panic!(),
            _ => (),
        }
    }

    fn source_map(&self) -> Option<&Lrc<SourceMap>> {
        Some(&self.source_map)
    }
}

struct SilentEmitter;

impl Translate for SilentEmitter {
    fn fluent_bundle(&self) -> Option<&Lrc<FluentBundle>> {
        None
    }

    fn fallback_fluent_bundle(&self) -> &FluentBundle {
        panic!()
    }
}

impl Emitter for SilentEmitter {
    fn emit_diagnostic(&mut self, _: &rustc_errors::Diagnostic) {}

    fn source_map(&self) -> Option<&Lrc<SourceMap>> {
        None
    }
}

fn make_config(code: &str) -> Config {
    let opts = find_deps();
    Config {
        opts: Options {
            maybe_sysroot: Some(PathBuf::from(sys_root())),
            search_paths: opts.search_paths,
            externs: opts.externs,
            ..Options::default()
        },
        crate_cfg: FxHashSet::default(),
        crate_check_cfg: CheckCfg::default(),
        input: Input::Str {
            name: FileName::Custom("main.rs".to_string()),
            input: code.to_string(),
        },
        output_dir: None,
        output_file: None,
        file_loader: None,
        locale_resources: rustc_driver_impl::DEFAULT_LOCALE_RESOURCES,
        lint_caps: FxHashMap::default(),
        parse_sess_created: Some(Box::new(|ps| {
            ps.span_diagnostic = Handler::with_emitter(false, None, Box::new(SilentEmitter));
        })),
        register_lints: None,
        override_queries: None,
        make_codegen_backend: None,
        registry: Registry::new(rustc_error_codes::DIAGNOSTICS),
    }
}

fn find_deps() -> Options {
    let dep = "deps_crate/target/debug/deps";
    let mut args: Vec<_> = vec![
        "a.rs".to_string(),
        "-L".to_string(),
        format!("dependency={}", dep),
    ];
    let files: BTreeMap<_, _> = std::fs::read_dir(dep)
        .unwrap()
        .filter_map(|f| {
            let f = f.ok()?;
            let f = f.file_name().to_str().unwrap().to_string();
            if !f.ends_with(".rlib") {
                return None;
            }
            let i = f.find('-')?;
            Some((f[3..i].to_string(), f))
        })
        .collect();
    for d in &["once_cell", "lazy_static", "libc"] {
        let d = format!("{}={}/{}", d, dep, files.get(&d.to_string()).unwrap());
        args.push("--extern".to_string());
        args.push(d);
    }
    let matches = rustc_driver::handle_options(&args).unwrap();
    rustc_session::config::build_session_options(&matches)
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunTySig {
    pub params: Vec<Type>,
    pub ret: Type,
}

impl FunTySig {
    fn from_fn_decl(decl: &FnDecl<'_>, tcx: TyCtxt<'_>) -> Self {
        let params: Vec<_> = decl
            .inputs
            .iter()
            .map(|ty| Type::from_ty(ty, tcx))
            .collect();
        let ret = if let FnRetTy::Return(ty) = decl.output {
            Type::from_ty(ty, tcx)
        } else {
            Type::Tup(vec![])
        };
        Self { params, ret }
    }

    pub fn normalize_result(self) -> Self {
        Self {
            params: self
                .params
                .into_iter()
                .map(Type::normalize_result)
                .collect(),
            ret: self.ret.normalize_result(),
        }
    }
}

impl fmt::Display for FunTySig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_list(f, self.params.iter(), "fn(", ", ", ")")?;
        write!(f, " -> {}", self.ret)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Slice(Box<Type>),
    Array(Box<Type>, String),
    Ptr(Box<Type>, bool),
    Ref(Box<Type>, bool),
    Tup(Vec<Type>),
    Path(String, Vec<Type>),
    TraitObject(Vec<Type>),
    BareFn(Box<FunTySig>),
    Never,
    Impl(Vec<Type>),
    Err,
}

impl Type {
    fn from_path(path: &Path<'_>, tcx: TyCtxt<'_>) -> Self {
        let Path { segments, .. } = path;
        let PathSegment { ident, args, .. } = segments.iter().last().unwrap();
        let id = ident.name.to_ident_string();
        let args = if let Some(args) = args {
            args.args
                .iter()
                .filter_map(|arg| {
                    if let GenericArg::Type(ty) = arg {
                        Some(Self::from_ty(ty, tcx))
                    } else {
                        None
                    }
                })
                .collect()
        } else {
            vec![]
        };
        Self::Path(id, args)
    }

    fn from_ty(ty: &Ty<'_>, tcx: TyCtxt<'_>) -> Self {
        match &ty.kind {
            TyKind::Slice(ty) => Self::Slice(Box::new(Self::from_ty(ty, tcx))),
            TyKind::Array(ty, len) => {
                let span = tcx.hir().span(len.hir_id());
                let len = tcx.sess().source_map().span_to_snippet(span).unwrap();
                Self::Array(Box::new(Self::from_ty(ty, tcx)), len)
            }
            TyKind::Ptr(MutTy { ty, mutbl }) => Self::Ptr(
                Box::new(Self::from_ty(ty, tcx)),
                matches!(mutbl, Mutability::Mut),
            ),
            TyKind::Ref(_, MutTy { ty, mutbl }) => Self::Ref(
                Box::new(Self::from_ty(ty, tcx)),
                matches!(mutbl, Mutability::Mut),
            ),
            TyKind::Tup(tys) => Self::Tup(tys.iter().map(|ty| Self::from_ty(ty, tcx)).collect()),
            TyKind::Path(QPath::Resolved(_, path)) => Self::from_path(path, tcx),
            TyKind::TraitObject(ts, _, _) => Self::TraitObject(
                ts.iter()
                    .map(|t| Self::from_path(t.trait_ref.path, tcx))
                    .collect(),
            ),
            TyKind::BareFn(t) => Self::BareFn(Box::new(FunTySig::from_fn_decl(t.decl, tcx))),
            TyKind::Never => Self::Never,
            TyKind::OpaqueDef(i, _, _) => {
                if let ItemKind::OpaqueTy(t) = &tcx.hir().item(*i).kind {
                    let ts = t
                        .bounds
                        .iter()
                        .filter_map(|b| match b {
                            GenericBound::Trait(t, _) => {
                                Some(Self::from_path(t.trait_ref.path, tcx))
                            }
                            _ => None,
                        })
                        .collect();
                    Self::Impl(ts)
                } else {
                    panic!()
                }
            }
            TyKind::Err(_) => Self::Err,
            t => panic!("{:?}", t),
        }
    }

    fn normalize_result(self) -> Self {
        match self {
            Self::Slice(ty) => Self::Slice(Box::new(ty.normalize_result())),
            Self::Array(ty, len) => Self::Array(Box::new(ty.normalize_result()), len),
            Self::Ptr(ty, is_mut) => Self::Ptr(Box::new(ty.normalize_result()), is_mut),
            Self::Ref(ty, is_mut) => Self::Ref(Box::new(ty.normalize_result()), is_mut),
            Self::Tup(tys) => Self::Tup(tys.into_iter().map(|ty| ty.normalize_result()).collect()),
            Self::Path(id, mut tys) => {
                if id == "Result" && tys.len() == 2 {
                    tys.pop();
                    tys.push(Self::Tup(vec![]));
                }
                let tys = tys.into_iter().map(|ty| ty.normalize_result()).collect();
                Self::Path(id, tys)
            }
            Self::TraitObject(tys) => {
                Self::TraitObject(tys.into_iter().map(|ty| ty.normalize_result()).collect())
            }
            Self::BareFn(sig) => Self::BareFn(Box::new(sig.normalize_result())),
            Self::Never => Self::Never,
            Self::Impl(tys) => {
                Self::Impl(tys.into_iter().map(|ty| ty.normalize_result()).collect())
            }
            Self::Err => Self::Err,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Slice(t) => write!(f, "[{}]", t),
            Self::Array(t, l) => write!(f, "[{}; {}]", t, l),
            Self::Ptr(t, m) => write!(f, "*{} {}", if *m { "mut" } else { "const" }, t),
            Self::Ref(t, m) => write!(f, "&{}{}", if *m { "mut " } else { "" }, t),
            Self::Tup(ts) => fmt_list(f, ts.iter(), "(", ", ", ")"),
            Self::Path(x, ts) => {
                write!(f, "{}", x)?;
                if !ts.is_empty() {
                    fmt_list(f, ts.iter(), "<", ", ", ">")?;
                }
                Ok(())
            }
            Self::TraitObject(ts) => fmt_list(f, ts.iter(), "dyn ", " + ", ""),
            Self::BareFn(sig) => write!(f, "{}", sig),
            Self::Never => write!(f, "!"),
            Self::Impl(ts) => fmt_list(f, ts.iter(), "impl ", " + ", ""),
            Self::Err => write!(f, "Err"),
        }
    }
}

fn fmt_list<T: fmt::Display, I: Iterator<Item = T>>(
    f: &mut fmt::Formatter<'_>,
    iter: I,
    begin: &str,
    sep: &str,
    end: &str,
) -> fmt::Result {
    write!(f, "{}", begin)?;
    for (i, item) in iter.enumerate() {
        if i == 0 {
            write!(f, "{}", item)?;
        } else {
            write!(f, "{}{}", sep, item)?;
        }
    }
    write!(f, "{}", end)
}

fn result_targs_in_path(path: &Path<'_>) -> Vec<Span> {
    let Path { segments, .. } = path;
    let PathSegment { ident, args, .. } = segments.iter().last().unwrap();
    let id = ident.name.to_ident_string();
    if id != "Result" || args.is_none() {
        return vec![];
    }
    let args = args.unwrap().args;
    if args.len() != 2 {
        return vec![];
    }
    vec![args[1].span()]
}

fn result_targs_in_ty(ty: &Ty<'_>) -> Vec<Span> {
    match &ty.kind {
        TyKind::Slice(ty) => result_targs_in_ty(ty),
        TyKind::Array(ty, _) => result_targs_in_ty(ty),
        TyKind::Ptr(MutTy { ty, .. }) => result_targs_in_ty(ty),
        TyKind::Ref(_, MutTy { ty, .. }) => result_targs_in_ty(ty),
        TyKind::Tup(tys) => tys.iter().flat_map(result_targs_in_ty).collect(),
        TyKind::Path(QPath::Resolved(_, path)) => result_targs_in_path(path),
        TyKind::TraitObject(_, _, _) => vec![],
        TyKind::BareFn(t) => result_targs_in_fn_decl(t.decl),
        TyKind::Never => vec![],
        TyKind::OpaqueDef(_, _, _) => vec![],
        TyKind::Err(_) => vec![],
        t => panic!("{:?}", t),
    }
}

fn result_targs_in_fn_decl(decl: &FnDecl<'_>) -> Vec<Span> {
    let mut spans: Vec<_> = decl.inputs.iter().flat_map(result_targs_in_ty).collect();
    if let FnRetTy::Return(ty) = decl.output {
        spans.append(&mut result_targs_in_ty(ty));
    }
    spans
}

#[derive(Debug, Clone)]
pub struct ParsedType {
    pub name: String,
    pub code: String,
    pub sort: TypeSort,
}

#[derive(Debug, Clone)]
pub struct ParsedItem {
    pub name: String,
    pub code: String,
    pub sort: ItemSort,
}

impl ParsedItem {
    pub fn get_code(&self) -> String {
        if let ItemSort::Type(t) = &self.sort {
            if !t.derives.is_empty() {
                let s = t
                    .derives
                    .iter()
                    .map(|s| s.as_str())
                    .collect::<Vec<_>>()
                    .join(", ");
                return format!("#[derive({})]\n{}", s, self.code);
            }
        }
        self.code.clone()
    }

    pub fn get_simple_code(&self) -> String {
        match &self.sort {
            ItemSort::Type(_) => self.code.clone(),
            ItemSort::Variable(v) => {
                format!(
                    "{} {}{}: {};",
                    if v.is_const { "const" } else { "static" },
                    if v.is_mutable { "mut " } else { "" },
                    self.name,
                    v.ty_str,
                )
            }
            ItemSort::Function(f) => format!("{};", f.signature),
            ItemSort::Use => panic!(),
        }
    }

    pub fn get_checking_code(&self) -> String {
        match &self.sort {
            ItemSort::Type(_) => self.get_code(),
            ItemSort::Variable(v) => {
                let transmute_init = || {
                    format!(
                        "unsafe {{ std::mem::transmute([0u8; std::mem::size_of::<{}>()]) }}",
                        v.ty_str
                    )
                };
                let init = match &v.ty {
                    Type::Array(t, l) => match &**t {
                        Type::Ref(t, false) => {
                            if **t == Type::Path("str".to_string(), vec![]) {
                                format!("[\"\"; {}]", l)
                            } else {
                                transmute_init()
                            }
                        }
                        _ => transmute_init(),
                    },
                    Type::BareFn(sig) => {
                        let p = sig
                            .params
                            .iter()
                            .map(|_| "_")
                            .collect::<Vec<_>>()
                            .join(", ");
                        format!("|{}| todo!()", p)
                    }
                    _ => transmute_init(),
                };
                format!(
                    "{} {}{}: {} = {};",
                    if v.is_const { "const" } else { "static" },
                    if v.is_mutable { "mut " } else { "" },
                    self.name,
                    v.ty_str,
                    init,
                )
            }
            ItemSort::Function(f) => format!("{} {{ todo!() }}", f.signature),
            ItemSort::Use => panic!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ItemSort {
    Type(TypeInfo),
    Variable(VariableInfo),
    Function(FunctionInfo),
    Use,
}

#[derive(Debug, Clone)]
pub struct TypeInfo {
    pub sort: TypeSort,
    pub derives: BTreeSet<String>,
}

#[derive(Debug, Clone)]
pub struct VariableInfo {
    pub is_const: bool,
    pub is_mutable: bool,
    pub ty: Type,
    pub ty_str: String,
}

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub signature: String,
    pub signature_ty: FunTySig,
    pub normalized_signature: String,
    pub normalized_signature_ty: FunTySig,
}

pub fn parse(code: &str) -> Option<Vec<ParsedItem>> {
    let config = make_config(code);
    rustc_interface::run_compiler(config, |compiler| {
        let sess = compiler.session();
        rustc_parse::maybe_new_parser_from_source_str(
            &sess.parse_sess,
            FileName::Custom("main.rs".to_string()),
            code.to_string(),
        )
        .ok()?;
        compiler.enter(|queries| {
            queries.global_ctxt().ok()?.enter(|tcx| {
                let source_map = compiler.session().source_map();
                let hir = tcx.hir();
                let mut items = vec![];
                let mut derives: BTreeMap<_, BTreeSet<_>> = BTreeMap::new();
                for id in hir.items() {
                    if tcx.def_path(id.owner_id.to_def_id()).data.len() > 1 {
                        continue;
                    }
                    let item = hir.item(id);
                    let name = item.ident.name.to_ident_string();
                    let code = source_map.span_to_snippet(item.span).unwrap();
                    let sort = match &item.kind {
                        ItemKind::TyAlias(_, _) => ItemSort::Type(TypeInfo {
                            sort: TypeSort::Typedef,
                            derives: BTreeSet::new(),
                        }),
                        ItemKind::Enum(_, _) => ItemSort::Type(TypeInfo {
                            sort: TypeSort::Enum,
                            derives: BTreeSet::new(),
                        }),
                        ItemKind::Struct(_, _) => ItemSort::Type(TypeInfo {
                            sort: TypeSort::Struct,
                            derives: BTreeSet::new(),
                        }),
                        ItemKind::Union(_, _) => ItemSort::Type(TypeInfo {
                            sort: TypeSort::Union,
                            derives: BTreeSet::new(),
                        }),
                        ItemKind::Impl(i) => {
                            if let Type::Path(ty, v) = Type::from_ty(i.self_ty, tcx) {
                                assert!(v.is_empty());
                                derives.entry(ty).or_default().insert(code);
                            } else {
                                panic!();
                            }
                            continue;
                        }

                        ItemKind::Static(ty, m, _) => {
                            let is_const = false;
                            let is_mutable = matches!(m, Mutability::Mut);
                            let ty_str = source_map.span_to_snippet(ty.span).unwrap();
                            let ty = Type::from_ty(ty, tcx);
                            ItemSort::Variable(VariableInfo {
                                is_const,
                                is_mutable,
                                ty,
                                ty_str,
                            })
                        }
                        ItemKind::Const(ty, _) => {
                            let is_const = true;
                            let is_mutable = false;
                            let ty_str = source_map.span_to_snippet(ty.span).unwrap();
                            let ty = Type::from_ty(ty, tcx);
                            ItemSort::Variable(VariableInfo {
                                is_const,
                                is_mutable,
                                ty,
                                ty_str,
                            })
                        }

                        ItemKind::Fn(sig, _, _) => {
                            let signature = source_map.span_to_snippet(sig.span).unwrap();

                            let prefix_len = code.find(&signature).unwrap();
                            let suffix_len = code.len() - prefix_len - signature.len();

                            let targs = result_targs_in_fn_decl(sig.decl);
                            let suggestions: Vec<_> = targs
                                .into_iter()
                                .map(|span| {
                                    make_suggestion(span_to_snippet(span, source_map), "()")
                                })
                                .collect();
                            let nsig = rustfix::apply_suggestions(&code, &suggestions).unwrap();
                            let normalized_signature =
                                nsig[prefix_len..nsig.len() - suffix_len].to_string();

                            let signature_ty = FunTySig::from_fn_decl(sig.decl, tcx);
                            let normalized_signature_ty = signature_ty.clone().normalize_result();

                            ItemSort::Function(FunctionInfo {
                                signature,
                                signature_ty,
                                normalized_signature,
                                normalized_signature_ty,
                            })
                        }

                        ItemKind::Use(_, _) => {
                            if code.is_empty() {
                                continue;
                            }
                            ItemSort::Use
                        }
                        ItemKind::ExternCrate(_) => {
                            assert_eq!(&code, "");
                            continue;
                        }
                        ItemKind::OpaqueTy(_) => continue,
                        i => panic!("{:?}", i),
                    };
                    items.push(ParsedItem { name, code, sort });
                }
                for item in &mut items {
                    if let ItemSort::Type(t) = &mut item.sort {
                        if let Some(ds) = derives.remove(&item.name) {
                            t.derives = ds;
                        }
                    }
                }
                Some(items)
            })
        })
    })
}

pub fn check_derive(code: &str) -> BTreeMap<String, BTreeSet<String>> {
    let inner = EmitterInner::default();
    let inner = Arc::new(Mutex::new(inner));
    let cloned_inner = inner.clone();

    let mut config = make_config(code);
    config.parse_sess_created = Some(Box::new(|ps: &mut ParseSess| {
        ps.span_diagnostic = Handler::with_emitter(
            true,
            None,
            Box::new(CollectingEmitter::new(cloned_inner, ps.clone_source_map())),
        );
    }));
    rustc_interface::run_compiler(config, |compiler| {
        let sess = compiler.session();
        rustc_parse::maybe_new_parser_from_source_str(
            &sess.parse_sess,
            FileName::Custom("main.rs".to_string()),
            code.to_string(),
        )
        .unwrap();
        let items = compiler.enter(|queries| {
            queries.global_ctxt().unwrap().enter(|tcx| {
                let hir = tcx.hir();
                let mut pos = BytePos::from_usize(0);
                let mut items = vec![];
                for id in hir.items() {
                    let item = hir.item(id);
                    if matches!(
                        item.kind,
                        ItemKind::Enum(_, _) | ItemKind::Struct(_, _) | ItemKind::Union(_, _)
                    ) {
                        let name = item.ident.name.to_ident_string();
                        let hi = item.span.hi();
                        items.push((name, pos, hi));
                        pos = hi;
                    }
                }
                let _ = tcx.analysis(());
                items
            })
        });
        let macros: Vec<_> =
            inner
                .lock()
                .unwrap()
                .diagnostics
                .iter()
                .flat_map(|diag| {
                    std::iter::once(&diag.span)
                        .chain(diag.children.iter().map(|child| &child.span))
                        .flat_map(|span| &span.span_labels)
                        .filter(|label| label.primary)
                        .flat_map(|label| {
                            label.span.span().macro_backtrace().filter_map(|expn_data| {
                                match expn_data.kind {
                                    ExpnKind::Macro(MacroKind::Derive, name) => {
                                        Some((name.to_ident_string(), label.span.lo))
                                    }
                                    _ => None,
                                }
                            })
                        })
                })
                .collect();
        let mut errors: BTreeMap<_, BTreeSet<_>> = BTreeMap::new();
        for (name, lo) in macros {
            let i_name = items
                .iter()
                .rev()
                .find(|(_, i_lo, i_hi)| i_lo <= &lo && &lo < i_hi)
                .unwrap()
                .0
                .clone();
            errors.entry(i_name).or_default().insert(name);
        }
        errors
    })
}

#[derive(Debug, Clone)]
pub struct TypeCheckingResult {
    pub errors: Vec<TypeError>,
    pub suggestions: Vec<Suggestion>,
    pub warnings: usize,
    pub uses: BTreeSet<String>,
}

impl TypeCheckingResult {
    pub fn passed(&self) -> bool {
        self.errors.is_empty() && self.suggestions.is_empty() && self.uses.is_empty()
    }
}

#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: String,
    pub line: usize,
    pub snippet: Snippet,
}

impl TypeError {
    pub fn code(&self) -> &str {
        &self.snippet.text.1
    }
}

const LENGTH_MSG: &str = "consider specifying the actual array length";
const IMPORT_TRAIT_MSG: &str = "implemented but not in scope; perhaps add a `use` for";
const IMPORT_MSG: &str = "consider importing one of these items";
const IMPORT_STRUCT_MSG: &str = "consider importing this struct";
const IMPORT_FUNCTION_MSG: &str = "consider importing this function";
const RET_IMPL_MSG: &str = "as the return type if all return paths have the same type but you want to expose only the trait in the signature";
const SIMILAR_MSG: &str = "a similar name";
const MAX_VAL_MSG: &str = "you may have meant the maximum value of";
const FORMAT_MSG: &str = "use the `Display` trait";
const CHANGE_IMPORT_MSG: &str = "you can use `as` to change the binding name of the import";

pub fn type_check(code: &str) -> Option<TypeCheckingResult> {
    let inner = EmitterInner::default();
    let inner = Arc::new(Mutex::new(inner));
    let cloned_inner = inner.clone();

    let mut config = make_config(code);
    config.parse_sess_created = Some(Box::new(|ps: &mut ParseSess| {
        ps.span_diagnostic = Handler::with_emitter(
            true,
            None,
            Box::new(CollectingEmitter::new(cloned_inner, ps.clone_source_map())),
        );
    }));
    rustc_interface::run_compiler(config, |compiler| {
        let sess = compiler.session();
        let source_map = sess.source_map();
        rustc_parse::maybe_new_parser_from_source_str(
            &sess.parse_sess,
            FileName::Custom("main.rs".to_string()),
            code.to_string(),
        )
        .ok()?;
        compiler.enter(|queries| {
            queries.global_ctxt().ok()?.enter(|tcx| {
                let mut visitor = TraitRefVisitor::new(tcx);
                tcx.hir().visit_all_item_likes_in_crate(&mut visitor);
                if visitor.has_undefined {
                    return None;
                }
                let _ = tcx.analysis(());
                Some(())
            })
        })?;
        let mut errors = vec![];
        let mut suggestions = vec![];
        let mut uses = BTreeSet::new();
        for diag in inner.lock().unwrap().diagnostics.iter() {
            let mut has_suggestion = false;
            for suggestion in &diag.suggestions {
                for subst in &suggestion.substitutions {
                    let mut follow_suggestion = || {
                        for (span, replacement) in &subst.parts {
                            let snippet = span_to_snippet(span.span(), source_map);
                            let suggestion = make_suggestion(snippet, replacement);
                            suggestions.push(suggestion);
                        }
                    };
                    match &suggestion.applicability {
                        Applicability::MachineApplicable => {
                            follow_suggestion();
                            has_suggestion = true;
                        }
                        Applicability::MaybeIncorrect => {
                            let msg = &suggestion.msg;
                            if msg.contains(LENGTH_MSG)
                                || msg.contains(SIMILAR_MSG)
                                || msg.contains(RET_IMPL_MSG)
                                || msg.contains(MAX_VAL_MSG)
                                || msg.contains(FORMAT_MSG)
                                || msg.contains(CHANGE_IMPORT_MSG)
                            {
                                follow_suggestion();
                                has_suggestion = true;
                            } else if msg.contains(IMPORT_TRAIT_MSG)
                                || msg.contains(IMPORT_STRUCT_MSG)
                                || msg.contains(IMPORT_MSG)
                            {
                                assert_eq!(subst.parts.len(), 1);
                                uses.insert(subst.parts[0].1.trim().to_string());
                                has_suggestion = true;
                            } else if msg.contains(IMPORT_FUNCTION_MSG) {
                            } else {
                                panic!("{}\n{:?}\n{:?}", code, diag, suggestion);
                            }
                        }
                        _ => (),
                    }
                    if has_suggestion {
                        break;
                    }
                }
                if has_suggestion {
                    break;
                }
            }
            if !has_suggestion {
                let message = format!("{}", WithSourceMap::new(source_map, diag));
                let line = diag.span.primary_line(source_map);
                let span = diag.span.entire_span(source_map).unwrap();
                let snippet = span_to_snippet(span, source_map);
                let error = TypeError {
                    message,
                    line,
                    snippet,
                };
                errors.push(error);
            }
        }
        let warnings = inner.lock().unwrap().warning_counter;
        Some(TypeCheckingResult {
            errors,
            suggestions,
            warnings,
            uses,
        })
    })
}

struct TraitRefVisitor<'tcx> {
    tcx: TyCtxt<'tcx>,
    has_undefined: bool,
}

impl<'tcx> TraitRefVisitor<'tcx> {
    fn new(tcx: TyCtxt<'tcx>) -> Self {
        Self {
            tcx,
            has_undefined: false,
        }
    }
}

impl<'tcx> Visitor<'tcx> for TraitRefVisitor<'tcx> {
    type NestedFilter = nested_filter::OnlyBodies;

    fn nested_visit_map(&mut self) -> Self::Map {
        self.tcx.hir()
    }

    fn visit_trait_ref(&mut self, t: &'tcx TraitRef<'tcx>) {
        if t.trait_def_id().is_none() {
            self.has_undefined = true;
        }
        intravisit::walk_trait_ref(self, t)
    }
}

pub fn make_suggestion(snippet: Snippet, replacement: &str) -> Suggestion {
    let replacement = Replacement {
        snippet: snippet.clone(),
        replacement: replacement.to_string(),
    };
    let solution = Solution {
        message: "".into(),
        replacements: vec![replacement],
    };
    Suggestion {
        message: "".into(),
        snippets: vec![snippet],
        solutions: vec![solution],
    }
}

fn pos_of_span(span: Span, source_map: &SourceMap) -> (usize, usize) {
    let fname = source_map.span_to_filename(span);
    let file = source_map.get_source_file(&fname).unwrap();
    let lo = file.lookup_file_pos_with_col_display(span.lo());
    (lo.0, lo.2)
}

fn span_to_snippet(span: Span, source_map: &SourceMap) -> Snippet {
    let fname = source_map.span_to_filename(span);
    let file = source_map.get_source_file(&fname).unwrap();
    let lo = file.lookup_file_pos_with_col_display(span.lo());
    let hi = file.lookup_file_pos_with_col_display(span.hi());
    let line_range = LineRange {
        start: LinePosition {
            line: lo.0,
            column: lo.2,
        },
        end: LinePosition {
            line: hi.0,
            column: hi.2,
        },
    };
    let lo_offset = file.original_relative_byte_pos(span.lo()).0;
    let hi_offset = file.original_relative_byte_pos(span.hi()).0;
    Snippet {
        file_name: fname.prefer_remapped().to_string(),
        line_range,
        range: (lo_offset as usize)..(hi_offset as usize),
        text: (
            "".into(),
            source_map.span_to_snippet(span).unwrap(),
            "".into(),
        ),
    }
}

fn get_bundle() -> Option<Lrc<FluentBundle>> {
    let opts = Options::default();
    rustc_errors::fluent_bundle(
        Some(PathBuf::from(sys_root())),
        vec![],
        opts.unstable_opts.translate_lang.clone(),
        opts.unstable_opts.translate_additional_ftl.as_deref(),
        opts.unstable_opts.translate_directionality_markers,
    )
    .unwrap()
}

fn get_fallback_bundle() -> LazyFallbackBundle {
    let opts = Options::default();
    let fluent_resources = Vec::from(rustc_driver_impl::DEFAULT_LOCALE_RESOURCES);
    rustc_errors::fallback_fluent_bundle(
        fluent_resources,
        opts.unstable_opts.translate_directionality_markers,
    )
}

fn sys_root() -> String {
    std::env::var("SYSROOT")
        .ok()
        .map(PathBuf::from)
        .or_else(|| {
            let home = std::env::var("RUSTUP_HOME")
                .or_else(|_| std::env::var("MULTIRUST_HOME"))
                .ok();
            let toolchain = std::env::var("RUSTUP_TOOLCHAIN")
                .or_else(|_| std::env::var("MULTIRUST_TOOLCHAIN"))
                .ok();
            toolchain_path(home, toolchain)
        })
        .or_else(|| {
            Command::new("rustc")
                .arg("--print")
                .arg("sysroot")
                .output()
                .ok()
                .and_then(|out| String::from_utf8(out.stdout).ok())
                .map(|s| PathBuf::from(s.trim()))
        })
        .or_else(|| option_env!("SYSROOT").map(PathBuf::from))
        .or_else(|| {
            let home = option_env!("RUSTUP_HOME")
                .or(option_env!("MULTIRUST_HOME"))
                .map(ToString::to_string);
            let toolchain = option_env!("RUSTUP_TOOLCHAIN")
                .or(option_env!("MULTIRUST_TOOLCHAIN"))
                .map(ToString::to_string);
            toolchain_path(home, toolchain)
        })
        .map(|pb| pb.to_string_lossy().to_string())
        .unwrap()
}

fn toolchain_path(home: Option<String>, toolchain: Option<String>) -> Option<PathBuf> {
    home.and_then(|home| {
        toolchain.map(|toolchain| {
            let mut path = PathBuf::from(home);
            path.push("toolchains");
            path.push(toolchain);
            path
        })
    })
}
