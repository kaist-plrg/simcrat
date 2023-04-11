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
    Expr, ExprKind, FnRetTy, FnSig, GenericArg, ItemKind, MatchSource, MutTy, Mutability, Path,
    PathSegment, QPath, Ty, TyKind,
};
use rustc_interface::Config;
use rustc_middle::{hir::nested_filter, ty::TyCtxt};
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
}

impl fmt::Display for WithSourceMap<'_, &MultiSpan> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let span = if let Some(span) = self.inner.entire_span(self.source_map) {
            span
        } else {
            return Ok(());
        };

        let mut labels: Vec<_> = self
            .inner
            .internal_labels(self.source_map)
            .iter()
            .filter_map(|SpanLabel { span, msg, .. }| msg.clone().map(|msg| (span.span(), msg)))
            .collect();
        labels.sort_by_key(|(span, _)| *span);
        labels.reverse();

        let mut line_span = self.source_map.span_extend_to_line(span.shrink_to_lo());
        loop {
            let line = self.source_map.span_to_snippet(line_span).unwrap();
            write!(f, "{}", line)?;

            let mut b = true;
            while let Some((span, _)) = labels.last() {
                if !line_span.contains(span.shrink_to_lo()) {
                    break;
                }
                let (span, msg) = labels.pop().unwrap();
                if !msg.is_empty() {
                    if b {
                        write!(f, " //")?;
                        b = false;
                    }
                    if !span.is_empty() && !self.source_map.is_multiline(span) {
                        write!(f, " {}: ", self.source_map.span_to_snippet(span).unwrap())?;
                    };
                    write!(f, "{}.", msg)?;
                }
            }

            let pos = line_span.hi() + BytePos::from_usize(1);
            line_span = self
                .source_map
                .span_extend_to_line(line_span.with_hi(pos).with_lo(pos));

            if !span.contains(line_span) {
                break;
            }

            writeln!(f)?;
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
        tracing::info!("{:?}", diag);
        match diag.level() {
            Level::Error { .. } => {
                let diag = self.diagnostic(diag);
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
    for d in &["once_cell", "lazy_static"] {
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
    fn merge_num(self) -> Self {
        Self {
            params: self.params.into_iter().map(Type::merge_num).collect(),
            ret: self.ret.merge_num(),
        }
    }

    fn normalize_result(self) -> Self {
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
        fmt_list(f, self.params.iter(), "(", ", ", ")")?;
        write!(f, " -> {}", self.ret)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Slice(Box<Type>),
    Array(Box<Type>),
    Ptr(Box<Type>, bool),
    Ref(Box<Type>, bool),
    Tup(Vec<Type>),
    Path(String, Vec<Type>),
    TraitObject(Vec<Type>),
}

impl Type {
    fn merge_num(self) -> Self {
        match self {
            Self::Slice(ty) => Self::Slice(Box::new(ty.merge_num())),
            Self::Array(ty) => Self::Array(Box::new(ty.merge_num())),
            Self::Ptr(ty, is_mut) => Self::Ptr(Box::new(ty.merge_num()), is_mut),
            Self::Ref(ty, is_mut) => Self::Ref(Box::new(ty.merge_num()), is_mut),
            Self::Tup(tys) => Self::Tup(tys.into_iter().map(|ty| ty.merge_num()).collect()),
            Self::Path(id, tys) => {
                let id = match id.as_str() {
                    "i8" | "i16" | "i32" | "i64" | "i128" | "isize" | "u8" | "u16" | "u32"
                    | "u64" | "u128" | "usize" => "int".to_string(),
                    "f32" | "f64" => "float".to_string(),
                    _ => id,
                };
                let tys = tys.into_iter().map(|ty| ty.merge_num()).collect();
                Self::Path(id, tys)
            }
            Self::TraitObject(tys) => {
                Self::TraitObject(tys.into_iter().map(|ty| ty.merge_num()).collect())
            }
        }
    }

    fn normalize_result(self) -> Self {
        match self {
            Self::Slice(ty) => Self::Slice(Box::new(ty.normalize_result())),
            Self::Array(ty) => Self::Array(Box::new(ty.normalize_result())),
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
        }
    }

    fn from_path(path: &Path<'_>) -> Self {
        let Path { segments, .. } = path;
        let PathSegment { ident, args, .. } = segments.iter().last().unwrap();
        let id = ident.name.to_ident_string();
        let args = if let Some(args) = args {
            args.args
                .iter()
                .filter_map(|arg| {
                    if let GenericArg::Type(ty) = arg {
                        Some(Self::from_ty(ty))
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

    fn from_ty(ty: &Ty<'_>) -> Self {
        match &ty.kind {
            TyKind::Slice(ty) => Self::Slice(Box::new(Self::from_ty(ty))),
            TyKind::Array(ty, _) => Self::Array(Box::new(Self::from_ty(ty))),
            TyKind::Ptr(MutTy { ty, mutbl }) => Self::Ptr(
                Box::new(Self::from_ty(ty)),
                matches!(mutbl, Mutability::Mut),
            ),
            TyKind::Ref(_, MutTy { ty, mutbl }) => Self::Ref(
                Box::new(Self::from_ty(ty)),
                matches!(mutbl, Mutability::Mut),
            ),
            TyKind::Tup(tys) => Self::Tup(tys.iter().map(Self::from_ty).collect()),
            TyKind::Path(QPath::Resolved(_, path)) => Self::from_path(path),
            TyKind::TraitObject(ts, _, _) => Self::TraitObject(
                ts.iter()
                    .map(|t| Self::from_path(t.trait_ref.path))
                    .collect(),
            ),
            t => panic!("{:?}", t),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Slice(t) => write!(f, "[{}]", t),
            Self::Array(t) => write!(f, "[{}; _]", t),
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

fn result_targ_spans_in_path(path: &Path<'_>) -> Vec<Span> {
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

fn result_targ_spans(ty: &Ty<'_>) -> Vec<Span> {
    match &ty.kind {
        TyKind::Slice(ty) => result_targ_spans(ty),
        TyKind::Array(ty, _) => result_targ_spans(ty),
        TyKind::Ptr(MutTy { ty, .. }) => result_targ_spans(ty),
        TyKind::Ref(_, MutTy { ty, .. }) => result_targ_spans(ty),
        TyKind::Tup(tys) => tys.iter().flat_map(result_targ_spans).collect(),
        TyKind::Path(QPath::Resolved(_, path)) => result_targ_spans_in_path(path),
        TyKind::TraitObject(ts, _, _) => ts
            .iter()
            .flat_map(|t| result_targ_spans_in_path(t.trait_ref.path))
            .collect(),
        t => panic!("{:?}", t),
    }
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
                    v.ty,
                )
            }
            ItemSort::Function(f) => format!("{};", f.signature),
        }
    }

    pub fn get_checking_code(&self) -> String {
        match &self.sort {
            ItemSort::Type(_) => self.get_code(),
            ItemSort::Variable(v) => {
                let init = if v.ty.starts_with("std::mem::MaybeUninit") {
                    "std::mem::MaybeUninit::uninit()".to_string()
                } else {
                    format!(
                        "unsafe {{ std::mem::transmute([0u8; std::mem::size_of::<{}>()]) }}",
                        v.ty
                    )
                };
                format!(
                    "{} {}{}: {} = {};",
                    if v.is_const { "const" } else { "static" },
                    if v.is_mutable { "mut " } else { "" },
                    self.name,
                    v.ty,
                    init,
                )
            }
            ItemSort::Function(f) => format!("{} {{ todo!() }}", f.signature),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ItemSort {
    Type(TypeInfo),
    Variable(VariableInfo),
    Function(FunctionInfo),
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
    pub ty: String,
}

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub signature: String,
    pub signature_ty: FunTySig,
}

pub fn parse(code: &str) -> Option<Vec<ParsedItem>> {
    let config = make_config(code);
    rustc_interface::run_compiler(config, |compiler| {
        let sess = compiler.session();
        let parsed = rustc_parse::maybe_new_parser_from_source_str(
            &sess.parse_sess,
            FileName::Custom("main.rs".to_string()),
            code.to_string(),
        );
        if parsed.is_err() {
            return None;
        }
        compiler.enter(|queries| {
            queries.global_ctxt().unwrap().enter(|tcx| {
                let source_map = compiler.session().source_map();
                let hir = tcx.hir();
                let mut items = vec![];
                let mut derives: BTreeMap<_, BTreeSet<_>> = BTreeMap::new();
                for id in hir.items() {
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
                            if let Type::Path(ty, v) = Type::from_ty(i.self_ty) {
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
                            let ty = source_map.span_to_snippet(ty.span).unwrap();
                            ItemSort::Variable(VariableInfo {
                                is_const,
                                is_mutable,
                                ty,
                            })
                        }
                        ItemKind::Const(ty, _) => {
                            let is_const = true;
                            let is_mutable = false;
                            let ty = source_map.span_to_snippet(ty.span).unwrap();
                            ItemSort::Variable(VariableInfo {
                                is_const,
                                is_mutable,
                                ty,
                            })
                        }

                        ItemKind::Fn(sig, _, _) => {
                            let signature = source_map.span_to_snippet(sig.span).unwrap();
                            let params: Vec<_> =
                                sig.decl.inputs.iter().map(Type::from_ty).collect();
                            let ret = if let FnRetTy::Return(ty) = sig.decl.output {
                                Type::from_ty(ty)
                            } else {
                                Type::Tup(vec![])
                            };
                            let signature_ty = FunTySig { params, ret };
                            ItemSort::Function(FunctionInfo {
                                signature,
                                signature_ty,
                            })
                        }

                        ItemKind::Use(_, _) => {
                            assert_eq!(&code, "");
                            continue;
                        }
                        ItemKind::ExternCrate(_) => {
                            assert_eq!(&code, "");
                            continue;
                        }
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

pub fn parse_types(code: &str) -> Option<Vec<ParsedType>> {
    let config = make_config(code);
    rustc_interface::run_compiler(config, |compiler| {
        let sess = compiler.session();
        let parsed = rustc_parse::maybe_new_parser_from_source_str(
            &sess.parse_sess,
            FileName::Custom("main.rs".to_string()),
            code.to_string(),
        );
        if parsed.is_err() {
            return None;
        }
        compiler.enter(|queries| {
            queries.global_ctxt().unwrap().enter(|tcx| {
                let source_map = compiler.session().source_map();
                let hir = tcx.hir();
                let items: Vec<_> = hir
                    .items()
                    .filter_map(|id| {
                        let item = hir.item(id);
                        let sort = match &item.kind {
                            ItemKind::TyAlias(_, _) => TypeSort::Typedef,
                            ItemKind::Enum(_, _) => TypeSort::Enum,
                            ItemKind::Struct(_, _) => TypeSort::Struct,
                            ItemKind::Union(_, _) => TypeSort::Union,
                            _ => return None,
                        };
                        let name = item.ident.name.to_ident_string();
                        let code = source_map.span_to_snippet(item.span).unwrap();
                        Some(ParsedType { name, code, sort })
                    })
                    .collect();
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

pub fn parse_global_variable(code: &str) -> Vec<(String, String)> {
    let config = make_config(code);
    rustc_interface::run_compiler(config, |compiler| {
        let sess = compiler.session();
        let parsed = rustc_parse::maybe_new_parser_from_source_str(
            &sess.parse_sess,
            FileName::Custom("main.rs".to_string()),
            code.to_string(),
        );
        if parsed.is_err() {
            return vec![];
        }
        compiler.enter(|queries| {
            queries.global_ctxt().unwrap().enter(|tcx| {
                let source_map = compiler.session().source_map();
                let hir = tcx.hir();
                hir.items()
                    .filter_map(|id| {
                        let item = hir.item(id);
                        let ty = match &item.kind {
                            ItemKind::Static(ty, _, _) => ty,
                            ItemKind::Const(ty, _) => ty,
                            _ => return None,
                        };
                        let name = item.ident.name.to_ident_string();
                        let ty = source_map.span_to_snippet(ty.span).unwrap();
                        Some((name, ty))
                    })
                    .collect()
            })
        })
    })
}

pub fn parse_signature(code: &str, merge_num: bool, normalize_result: bool) -> (FunTySig, String) {
    let config = make_config(code);
    rustc_interface::run_compiler(config, |compiler| {
        let sess = compiler.session();
        let _ = rustc_parse::maybe_new_parser_from_source_str(
            &sess.parse_sess,
            FileName::Custom("main.rs".to_string()),
            code.to_string(),
        )
        .unwrap();
        compiler.enter(|queries| {
            queries.global_ctxt().unwrap().enter(|tcx| {
                let source_map = compiler.session().source_map();
                let hir = tcx.hir();
                let decl = hir
                    .items()
                    .filter_map(|id| {
                        if let ItemKind::Fn(FnSig { decl, .. }, _, _) = hir.item(id).kind {
                            Some(decl)
                        } else {
                            None
                        }
                    })
                    .next()
                    .unwrap();
                let params: Vec<_> = decl.inputs.iter().map(Type::from_ty).collect();
                let mut spans: Vec<_> = decl.inputs.iter().flat_map(result_targ_spans).collect();
                let (ret, mut ret_spans) = if let FnRetTy::Return(ty) = decl.output {
                    (Type::from_ty(ty), result_targ_spans(ty))
                } else {
                    (Type::Tup(vec![]), vec![])
                };
                spans.append(&mut ret_spans);

                let mut sig = FunTySig { params, ret };
                if merge_num {
                    sig = sig.merge_num();
                }
                if normalize_result {
                    sig = sig.normalize_result();
                }
                let code = if normalize_result {
                    let suggestions: Vec<_> = spans
                        .iter()
                        .map(|span| make_suggestion(span_to_snippet(*span, source_map), "()"))
                        .collect();
                    rustfix::apply_suggestions(code, &suggestions).unwrap()
                } else {
                    code.to_string()
                };
                (sig, code)
            })
        })
    })
}

#[derive(Debug, Clone)]
pub struct TypeCheckingResult {
    pub errors: Vec<TypeError>,
    pub suggestions: Vec<Suggestion>,
    pub warnings: usize,
    pub uses: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: String,
    pub snippet: Snippet,
}

impl TypeError {
    pub fn code(&self) -> &str {
        &self.snippet.text.1
    }

    pub fn line(&self) -> usize {
        self.snippet.line_range.start.line
    }
}

const LENGTH_MSG: &str = "consider specifying the actual array length";
const ADD_USE_MSG: &str = "implemented but not in scope; perhaps add a `use` for";

pub fn type_check(code: &str) -> TypeCheckingResult {
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
        .unwrap();
        compiler.enter(|queries| {
            queries.global_ctxt().unwrap().enter(|tcx| {
                let _ = tcx.analysis(());
            })
        });
        let mut errors = vec![];
        let mut suggestions = vec![];
        let mut uses = BTreeSet::new();
        for diag in inner.lock().unwrap().diagnostics.iter() {
            let mut has_suggestion = false;
            for suggestion in &diag.suggestions {
                assert_eq!(suggestion.substitutions.len(), 1);
                let subst = &suggestion.substitutions[0];
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
                        if msg.contains(LENGTH_MSG) {
                            follow_suggestion();
                            has_suggestion = true;
                        } else if msg.contains(ADD_USE_MSG) {
                            assert_eq!(subst.parts.len(), 1);
                            uses.insert(subst.parts[0].1.clone());
                            has_suggestion = true;
                        } else {
                            panic!("{:?}", suggestion);
                        }
                    }
                    _ => (),
                }
            }
            if !has_suggestion {
                let message = format!("{}", WithSourceMap::new(source_map, diag));
                let span = diag.span.entire_span(source_map).unwrap();
                let snippet = span_to_snippet(span, source_map);
                let error = TypeError { message, snippet };
                errors.push(error);
            }
        }
        let warnings = inner.lock().unwrap().warning_counter;
        let uses = uses.into_iter().collect();
        TypeCheckingResult {
            errors,
            suggestions,
            warnings,
            uses,
        }
    })
}

struct SemipredicateVisitor<'tcx> {
    tcx: TyCtxt<'tcx>,
    some: bool,
    none: bool,
    ok: bool,
    err: bool,
}

impl<'tcx> SemipredicateVisitor<'tcx> {
    fn new(tcx: TyCtxt<'tcx>) -> Self {
        Self {
            tcx,
            some: false,
            none: false,
            ok: false,
            err: false,
        }
    }
}

impl<'tcx> Visitor<'tcx> for SemipredicateVisitor<'tcx> {
    type NestedFilter = nested_filter::OnlyBodies;

    fn nested_visit_map(&mut self) -> Self::Map {
        self.tcx.hir()
    }

    fn visit_expr(&mut self, expr: &'tcx Expr<'tcx>) {
        match &expr.kind {
            ExprKind::Path(QPath::Resolved(_, Path { segments, .. })) => {
                let ident = segments.iter().last().unwrap().ident;
                match ident.name.to_ident_string().as_ref() {
                    "Some" => self.some = true,
                    "None" => self.none = true,
                    "Ok" => self.ok = true,
                    "Err" => self.err = true,
                    _ => (),
                }
            }
            ExprKind::Match(_, _, MatchSource::TryDesugar) => {
                self.none = true;
                self.err = true;
            }
            _ => (),
        }
        intravisit::walk_expr(self, expr);
    }
}

pub fn is_proper_semipredicate(code: &str, option: bool) -> bool {
    let config = make_config(code);
    rustc_interface::run_compiler(config, |compiler| {
        let sess = compiler.session();
        if rustc_parse::maybe_new_parser_from_source_str(
            &sess.parse_sess,
            FileName::Custom("main.rs".to_string()),
            code.to_string(),
        )
        .is_err()
        {
            return false;
        }
        compiler.enter(|queries| {
            queries.global_ctxt().unwrap().enter(|tcx| {
                let mut visitor = SemipredicateVisitor::new(tcx);
                tcx.hir().visit_all_item_likes_in_crate(&mut visitor);
                if option {
                    !(visitor.some ^ visitor.none)
                } else {
                    !(visitor.ok ^ visitor.err)
                }
            })
        })
    })
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let code = "type T = usize;";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed[0].name, "T");
        assert_eq!(parsed[0].code, code);
        if let ItemSort::Type(t) = &parsed[0].sort {
            assert_eq!(t.sort, TypeSort::Typedef);
            assert!(t.derives.is_empty());
        } else {
            panic!("unexpected sort");
        }

        let code = "#[derive(Clone, Copy)] struct T; struct S {}";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.len(), 2);
        assert_eq!(parsed[0].name, "T");
        assert_eq!(parsed[0].code, "struct T;");
        if let ItemSort::Type(t) = &parsed[0].sort {
            assert_eq!(t.sort, TypeSort::Struct);
            assert_eq!(t.derives.len(), 2);
            assert!(t.derives.contains(&"Clone".to_string()));
            assert!(t.derives.contains(&"Copy".to_string()));
        } else {
            panic!("unexpected sort");
        }
        assert_eq!(parsed[1].name, "S");
        assert_eq!(parsed[1].code, "struct S {}");
        if let ItemSort::Type(t) = &parsed[1].sort {
            assert_eq!(t.sort, TypeSort::Struct);
            assert!(t.derives.is_empty());
        } else {
            panic!("unexpected sort");
        }

        let code = "union U { x: u32, y: i32 }";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed[0].name, "U");
        assert_eq!(parsed[0].code, code);
        if let ItemSort::Type(t) = &parsed[0].sort {
            assert_eq!(t.sort, TypeSort::Union);
            assert!(t.derives.is_empty());
        } else {
            panic!("unexpected sort");
        }

        let code = "enum E { A, B }";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed[0].name, "E");
        assert_eq!(parsed[0].code, code);
        if let ItemSort::Type(t) = &parsed[0].sort {
            assert_eq!(t.sort, TypeSort::Enum);
            assert!(t.derives.is_empty());
        } else {
            panic!("unexpected sort");
        }

        let code = "static X: usize = 1;";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed[0].name, "X");
        assert_eq!(parsed[0].code, code);
        if let ItemSort::Variable(v) = &parsed[0].sort {
            assert!(!v.is_const);
            assert!(!v.is_mutable);
            assert_eq!(v.ty, "usize");
        } else {
            panic!("unexpected sort");
        }

        let code = "static mut Y: isize = 1;";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed[0].name, "Y");
        assert_eq!(parsed[0].code, code);
        if let ItemSort::Variable(v) = &parsed[0].sort {
            assert!(!v.is_const);
            assert!(v.is_mutable);
            assert_eq!(v.ty, "isize");
        } else {
            panic!("unexpected sort");
        }

        let code = "const Z: f32 = 1.0;";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed[0].name, "Z");
        assert_eq!(parsed[0].code, code);
        if let ItemSort::Variable(v) = &parsed[0].sort {
            assert!(v.is_const);
            assert!(!v.is_mutable);
            assert_eq!(v.ty, "f32");
        } else {
            panic!("unexpected sort");
        }

        let code = "fn foo(x: u64) -> i64 { x as _ }";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed[0].name, "foo");
        assert_eq!(parsed[0].code, code);
        if let ItemSort::Function(f) = &parsed[0].sort {
            assert_eq!(f.signature, "fn foo(x: u64) -> i64");
            assert_eq!(f.signature_ty.params.len(), 1);
            assert_eq!(
                f.signature_ty.params[0],
                Type::Path("u64".to_string(), vec![])
            );
            assert_eq!(f.signature_ty.ret, Type::Path("i64".to_string(), vec![]));
        } else {
            panic!("unexpected sort");
        }
    }

    #[test]
    fn test_parse_global_variable() {
        let mut v = vec![("X".to_string(), "usize".to_string())];

        let code = "static X: usize = 1;";
        assert_eq!(parse_global_variable(code), v);

        let code = "static mut X: usize = 1;";
        assert_eq!(parse_global_variable(code), v);

        let code = "const X: usize = 1;";
        assert_eq!(parse_global_variable(code), v);

        v.push(("Y".to_string(), "isize".to_string()));
        let code = "static X: usize = 1; const Y: isize = 2;";
        assert_eq!(parse_global_variable(code), v);
    }

    #[test]
    fn test_parse_signature() {
        let parse = |code| parse_signature(code, true, true);
        let int = Type::Path("int".to_string(), vec![]);
        let float = Type::Path("float".to_string(), vec![]);
        let unit = Type::Tup(vec![]);
        let option = |t| Type::Path("Option".to_string(), vec![t]);
        let result = |t| Type::Path("Result".to_string(), vec![t, unit.clone()]);

        let code = "fn f(a: i8, b: i16, c: i32, d: i64, e: i128) -> isize {}";
        let (FunTySig { params, ret }, new_code) = parse(code);
        assert_eq!(params.len(), 5);
        for param in params {
            assert_eq!(param, int);
        }
        assert_eq!(ret, int);
        assert_eq!(new_code, code);

        let code = "fn f(a: u8, b: u16, c: u32, d: u64, e: u128) -> usize {}";
        let (FunTySig { params, ret }, new_code) = parse(code);
        assert_eq!(params.len(), 5);
        for param in params {
            assert_eq!(param, int);
        }
        assert_eq!(ret, int);
        assert_eq!(new_code, code);

        let code = "fn f(a: f32) -> f64 {}";
        let (FunTySig { params, ret }, new_code) = parse(code);
        assert_eq!(params.len(), 1);
        assert_eq!(params[0], float);
        assert_eq!(ret, float);
        assert_eq!(new_code, code);

        let code = "fn f() -> () {}";
        let (FunTySig { params, ret }, new_code) = parse(code);
        assert_eq!(params.len(), 0);
        assert_eq!(ret, unit);
        assert_eq!(new_code, code);

        let code = "fn f() {}";
        let (FunTySig { params, ret }, new_code) = parse(code);
        assert_eq!(params.len(), 0);
        assert_eq!(ret, unit);
        assert_eq!(new_code, code);

        let code = "fn f() -> Option<usize> {}";
        let (FunTySig { params, ret }, new_code) = parse(code);
        assert_eq!(params.len(), 0);
        assert_eq!(ret, option(int.clone()));
        assert_eq!(new_code, code);

        let code = "fn f() -> std::option::Option<usize> {}";
        let (FunTySig { params, ret }, new_code) = parse(code);
        assert_eq!(params.len(), 0);
        assert_eq!(ret, option(int.clone()));
        assert_eq!(new_code, code);

        let code = "fn f() -> Result<usize, ()> {}";
        let (FunTySig { params, ret }, new_code) = parse(code);
        assert_eq!(params.len(), 0);
        assert_eq!(ret, result(int.clone()));
        assert_eq!(new_code, code);

        let code2 = "fn f() -> Result<usize, usize> {}";
        let (FunTySig { params, ret }, new_code) = parse(code2);
        assert_eq!(params.len(), 0);
        assert_eq!(ret, result(int.clone()));
        assert_eq!(new_code, code);

        let code = "fn f() -> [usize] {}";
        let (FunTySig { params, ret }, new_code) = parse(code);
        assert_eq!(params.len(), 0);
        assert_eq!(ret, Type::Slice(Box::new(int.clone())));
        assert_eq!(new_code, code);

        let code = "fn f() -> [usize; 3] {}";
        let (FunTySig { params, ret }, new_code) = parse(code);
        assert_eq!(params.len(), 0);
        assert_eq!(ret, Type::Array(Box::new(int.clone())));
        assert_eq!(new_code, code);

        let code = "fn f() -> *const usize {}";
        let (FunTySig { params, ret }, new_code) = parse(code);
        assert_eq!(params.len(), 0);
        assert_eq!(ret, Type::Ptr(Box::new(int.clone()), false));
        assert_eq!(new_code, code);

        let code = "fn f() -> *mut usize {}";
        let (FunTySig { params, ret }, new_code) = parse(code);
        assert_eq!(params.len(), 0);
        assert_eq!(ret, Type::Ptr(Box::new(int.clone()), true));
        assert_eq!(new_code, code);

        let code = "fn f() -> &usize {}";
        let (FunTySig { params, ret }, new_code) = parse(code);
        assert_eq!(params.len(), 0);
        assert_eq!(ret, Type::Ref(Box::new(int.clone()), false));
        assert_eq!(new_code, code);

        let code = "fn f() -> &mut usize {}";
        let (FunTySig { params, ret }, new_code) = parse(code);
        assert_eq!(params.len(), 0);
        assert_eq!(ret, Type::Ref(Box::new(int.clone()), true));
        assert_eq!(new_code, code);

        let code = "fn f() -> (usize, f32) {}";
        let (FunTySig { params, ret }, new_code) = parse(code);
        assert_eq!(params.len(), 0);
        assert_eq!(ret, Type::Tup(vec![int.clone(), float.clone()]));
        assert_eq!(new_code, code);

        let code = "fn f() -> dyn usize + f32 {}";
        let (FunTySig { params, ret }, new_code) = parse(code);
        assert_eq!(params.len(), 0);
        assert_eq!(ret, Type::TraitObject(vec![int.clone(), float.clone()]));
        assert_eq!(new_code, code);
    }

    #[test]
    fn test_type_check() {
        let TypeCheckingResult {
            errors,
            suggestions,
            warnings,
            uses,
        } = type_check(
            "fn main() {
    let mut x = 1;
    let a = &mut x;
    let b = &x;
    let _ = *b;
    *a = 2;
}",
        );
        assert_eq!(errors.len(), 1);
        let msg = &errors[0].message;
        assert!(msg.starts_with(
            "error[E0502]: cannot borrow `x` as immutable because it is also borrowed as mutable"
        ));
        assert!(msg.contains("mutable borrow occurs here"));
        assert!(msg.contains("immutable borrow occurs here"));
        assert!(msg.contains("mutable borrow later used here"));
        assert_eq!(suggestions.len(), 0);
        assert_eq!(warnings, 0);
        assert_eq!(uses.len(), 0);

        let TypeCheckingResult {
            errors,
            suggestions,
            warnings,
            uses,
        } = type_check(
            "fn main() {
    let x = 1;
}",
        );
        assert_eq!(errors.len(), 0);
        assert_eq!(suggestions.len(), 0);
        assert_eq!(warnings, 1);
        assert_eq!(uses.len(), 0);

        let code = "fn main() {
    let x = 1i32;
    let _: u32 = x;
}";
        let TypeCheckingResult {
            errors,
            suggestions,
            warnings,
            uses,
        } = type_check(code);
        assert_eq!(errors.len(), 0);
        assert_eq!(suggestions.len(), 1);
        assert_eq!(warnings, 0);
        assert_eq!(uses.len(), 0);

        let code = rustfix::apply_suggestions(code, &suggestions).unwrap();
        assert!(code.contains(".try_into().unwrap()"));
        let TypeCheckingResult {
            errors,
            suggestions,
            warnings,
            uses,
        } = type_check(&code);
        assert_eq!(errors.len(), 0);
        assert_eq!(suggestions.len(), 0);
        assert_eq!(warnings, 0);
        assert_eq!(uses.len(), 1);

        let code = uses[0].clone() + &code;
        assert!(code.starts_with("use std::convert::TryInto;"));
        let TypeCheckingResult {
            errors,
            suggestions,
            warnings,
            uses,
        } = type_check(&code);
        assert_eq!(errors.len(), 0);
        assert_eq!(suggestions.len(), 0);
        assert_eq!(warnings, 0);
        assert_eq!(uses.len(), 0);

        let TypeCheckingResult {
            errors,
            suggestions,
            warnings,
            uses,
        } = type_check(
            "fn main() {
    foo(1);
}
fn foo() {}",
        );
        assert_eq!(errors.len(), 1);
        let msg = &errors[0].message;
        assert!(msg.starts_with(
            "error[E0061]: this function takes 0 arguments but 1 argument was supplied"
        ));
        assert!(msg.contains("note: function defined here"));
        assert_eq!(suggestions.len(), 0);
        assert_eq!(warnings, 0);
        assert_eq!(uses.len(), 0);

        let TypeCheckingResult {
            errors,
            suggestions,
            warnings,
            uses,
        } = type_check(
            "fn main() {
    1.0 * 1;
}",
        );
        assert_eq!(errors.len(), 1);
        let msg = &errors[0].message;
        assert!(msg.starts_with("error[E0277]: cannot multiply `{float}` by `{integer}`"));
        assert!(msg.contains("no implementation for `{float} * {integer}"));
        assert!(msg.contains(
            "= help: the trait `std::ops::Mul<{integer}>` is not implemented for `{float}`"
        ));
        assert!(
            msg.contains("= help: the following other types implement trait `std::ops::Mul<Rhs>`:")
        );
        assert_eq!(suggestions.len(), 0);
        assert_eq!(warnings, 0);
        assert_eq!(uses.len(), 0);

        let TypeCheckingResult {
            errors,
            suggestions,
            warnings,
            uses,
        } = type_check(
            "fn main() {}
fn foo(x: usize, a: [usize; x]) {}",
        );
        assert_eq!(errors.len(), 1);
        let msg = &errors[0].message;
        assert!(msg.starts_with("error[E0435]: attempt to use a non-constant value in a constant"));
        assert!(msg.contains("this would need to be a `const`"));
        assert_eq!(suggestions.len(), 0);
        assert_eq!(warnings, 0);
        assert_eq!(uses.len(), 0);
    }

    #[test]
    fn test_semipredicate() {
        assert_eq!(is_proper_semipredicate("fn f() { Some(0) }", true), false);
        assert_eq!(is_proper_semipredicate("fn f() { None }", true), false);
        assert_eq!(is_proper_semipredicate("fn f() { bar()?; }", true), false);
        assert_eq!(
            is_proper_semipredicate("fn f() { bar()?; None }", true),
            false
        );
        assert_eq!(
            is_proper_semipredicate(
                "fn f() { match x { Some(_) => {} None => {} }; None }",
                true
            ),
            false
        );
        assert_eq!(is_proper_semipredicate("fn f() { bar() }", true), true);
        assert_eq!(
            is_proper_semipredicate("fn f() { None; Some(0) }", true),
            true
        );
        assert_eq!(
            is_proper_semipredicate("fn f() { bar()?; Some(0) }", true),
            true
        );

        assert_eq!(is_proper_semipredicate("fn f() { Ok(0) }", false), false);
        assert_eq!(is_proper_semipredicate("fn f() { Err(()) }", false), false);
        assert_eq!(is_proper_semipredicate("fn f() { bar()?; }", false), false);
        assert_eq!(
            is_proper_semipredicate("fn f() { bar()?; Err(()) }", false),
            false
        );
        assert_eq!(
            is_proper_semipredicate(
                "fn f() { match x { Ok(_) => {} Err(_) => {} }; Err(()) }",
                false
            ),
            false
        );
        assert_eq!(is_proper_semipredicate("fn f() { bar() }", false), true);
        assert_eq!(
            is_proper_semipredicate("fn f() { Err(()); Ok(0) }", false),
            true
        );
        assert_eq!(
            is_proper_semipredicate("fn f() { bar()?; Ok(0) }", false),
            true
        );
    }
}
