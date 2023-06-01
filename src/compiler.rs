use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
    path::PathBuf,
    process::Command,
    sync::{Arc, Mutex},
};

use etrace::ok_or;
use lazy_static::lazy_static;
use rustc_data_structures::sync::Lrc;
use rustc_errors::{
    emitter::Emitter, registry::Registry, translation::Translate, Applicability, DiagnosticId,
    DiagnosticMessage, FluentBundle, Handler, LazyFallbackBundle, Level, SubstitutionPart,
};
use rustc_hash::{FxHashMap, FxHashSet};
use rustc_hir::{
    def::Res,
    hir_id::HirId,
    intravisit::{self, Visitor},
    Expr, ExprKind, FnDecl, FnRetTy, GenericArg, GenericBound, GenericParam, GenericParamKind,
    ItemKind, MutTy, Mutability, Path, PathSegment, QPath, TraitRef, Ty, TyKind,
};
use rustc_interface::{interface::Compiler, Config};
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
            .filter(|l| internal_span(l.span.span(), source_map))
            .collect()
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
            WithSourceMap::new(self.source_map, &self.inner.span)
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
            WithSourceMap::new(self.source_map, &self.inner.span)
        )?;
        for child in &self.inner.children {
            writeln!(f, "{}", WithSourceMap::new(self.source_map, child))?;
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
        let span = label.span;
        let span = if internal_span(span, &self.source_map) {
            span
        } else {
            span.macro_backtrace()
                .find_map(|expn| {
                    if internal_span(expn.call_site, &self.source_map) {
                        Some(expn.call_site)
                    } else {
                        None
                    }
                })
                .unwrap_or(span)
        };
        let span = span.data();
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
        match diag.level() {
            Level::Error { .. } => {
                let diag = self.diagnostic(diag);
                tracing::info!("{:?}", diag);
                self.inner.lock().unwrap().diagnostics.push(diag);
            }
            Level::Warning(_) => self.inner.lock().unwrap().warning_counter += 1,
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

fn run_compiler<R: Send, F: FnOnce(&Compiler) -> R + Send>(config: Config, f: F) -> Option<R> {
    rustc_driver::catch_fatal_errors(|| rustc_interface::run_compiler(config, f)).ok()
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
    for d in &["once_cell", "libc"] {
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
    pub generic: bool,
}

impl FunTySig {
    fn from_fn_decl(decl: &FnDecl<'_>, generics: &[GenericParam<'_>], tcx: TyCtxt<'_>) -> Self {
        let params: Vec<_> = decl
            .inputs
            .iter()
            .map(|ty| Type::from_ty(ty, tcx))
            .collect();
        let ret = if let FnRetTy::Return(ty) = decl.output {
            Type::from_ty(ty, tcx)
        } else {
            UNIT
        };
        let generic = generics.iter().any(|p| {
            matches!(
                p.kind,
                GenericParamKind::Type { .. } | GenericParamKind::Const { .. }
            )
        });
        Self {
            params,
            ret,
            generic,
        }
    }

    pub fn into_c(self, map: &BTreeMap<&str, &str>) -> Self {
        Self {
            params: self.params.into_iter().map(|t| t.into_c(map)).collect(),
            ret: self.ret.into_c(map),
            generic: self.generic,
        }
    }

    fn contains(&self, s: &str) -> bool {
        self.params.iter().any(|t| t.contains(s)) || self.ret.contains(s)
    }

    fn contains_slice(&self) -> bool {
        self.params.iter().any(|t| t.contains_slice()) || self.ret.contains_slice()
    }

    fn contains_array(&self) -> bool {
        self.params.iter().any(|t| t.contains_array()) || self.ret.contains_array()
    }

    fn contains_tuple(&self) -> bool {
        self.params.iter().any(|t| t.contains_tuple()) || self.ret.contains_tuple()
    }
}

impl fmt::Display for FunTySig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_list(f, self.params.iter(), "fn(", ", ", ")")?;
        write!(f, " -> {}", self.ret)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PathSeg {
    ident: String,
    args: Vec<Type>,
}

impl PathSeg {
    fn from_path_segment(seg: &PathSegment<'_>, tcx: TyCtxt<'_>) -> Self {
        let ident = seg.ident.name.to_ident_string();
        let args = if let Some(args) = seg.args {
            args.args
                .iter()
                .filter_map(|arg| {
                    if let GenericArg::Type(ty) = arg {
                        Some(Type::from_ty(ty, tcx))
                    } else {
                        None
                    }
                })
                .collect()
        } else {
            vec![]
        };
        Self { ident, args }
    }

    fn into_c(self, map: &BTreeMap<&str, &str>) -> Self {
        Self {
            ident: self.ident,
            args: self.args.into_iter().map(|t| t.into_c(map)).collect(),
        }
    }

    fn contains(&self, s: &str) -> bool {
        self.ident == s
            || self
                .ident
                .strip_prefix("impl ")
                .map(|i| i.contains(s))
                .unwrap_or(false)
            || self.args.iter().any(|t| t.contains(s))
    }

    fn contains_slice(&self) -> bool {
        self.args.iter().any(|t| t.contains_slice())
    }

    fn contains_array(&self) -> bool {
        self.args.iter().any(|t| t.contains_array())
    }

    fn contains_fn(&self) -> bool {
        self.args.iter().any(|t| t.contains_fn())
    }

    fn contains_tuple(&self) -> bool {
        self.args.iter().any(|t| t.contains_tuple())
    }
}

impl fmt::Display for PathSeg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.ident)?;
        if !self.args.is_empty() {
            fmt_list(f, self.args.iter(), "<", ", ", ">")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Slice(Box<Type>),
    Array(Box<Type>, String),
    Ptr(Box<Type>, bool),
    Ref(Box<Type>, bool),
    Tup(Vec<Type>),
    Path(Vec<PathSeg>),
    TypeRelative(Box<Type>, PathSeg),
    TraitObject(Vec<Type>),
    BareFn(Box<FunTySig>),
    Never,
    Impl(Vec<Type>),
    Infer,
    Err,
}

pub const UNIT: Type = Type::Tup(vec![]);

impl Type {
    fn from_path(path: &Path<'_>, tcx: TyCtxt<'_>) -> Self {
        let Path { segments, .. } = path;
        let segs = segments
            .iter()
            .map(|seg| PathSeg::from_path_segment(seg, tcx))
            .collect();
        Self::Path(segs)
    }

    pub fn from_name(s: String) -> Self {
        Type::Path(vec![PathSeg {
            ident: s,
            args: vec![],
        }])
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
            TyKind::Path(path) => match path {
                QPath::Resolved(_, path) => Self::from_path(path, tcx),
                QPath::TypeRelative(ty, seg) => Self::TypeRelative(
                    Box::new(Self::from_ty(ty, tcx)),
                    PathSeg::from_path_segment(seg, tcx),
                ),
                _ => panic!(
                    "{:?} {}",
                    ty,
                    tcx.sess.source_map().span_to_snippet(ty.span).unwrap()
                ),
            },
            TyKind::TraitObject(ts, _, _) => Self::TraitObject(
                ts.iter()
                    .map(|t| Self::from_path(t.trait_ref.path, tcx))
                    .collect(),
            ),
            TyKind::BareFn(t) => Self::BareFn(Box::new(FunTySig::from_fn_decl(
                t.decl,
                t.generic_params,
                tcx,
            ))),
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
            TyKind::Infer => Self::Infer,
            t => panic!(
                "{:?} {}",
                t,
                tcx.sess.source_map().span_to_snippet(ty.span).unwrap()
            ),
        }
    }

    fn into_c(self, map: &BTreeMap<&str, &str>) -> Self {
        match self {
            Self::Slice(t) => Self::Slice(Box::new(t.into_c(map))),
            Self::Array(t, _) => Self::Array(Box::new(t.into_c(map)), "_".to_string()),
            Self::Ptr(t, _) => Self::Ptr(Box::new(t.into_c(map)), true),
            Self::Ref(t, _) => Self::Ptr(Box::new(t.into_c(map)), true),
            Self::Tup(ts) => Self::Tup(ts.into_iter().map(|t| t.into_c(map)).collect()),
            Self::Path(ss) => {
                let ss: Vec<_> = ss.into_iter().map(|s| s.into_c(map)).collect();
                let last = ss.last().unwrap();
                let ty = last.ident.as_str();
                if INT_TYPES.contains(&ty)
                    || ty == "RawFd"
                    || ty == "pid_t"
                    || ty == "bool"
                    || ty.to_lowercase().contains("int")
                {
                    Type::from_name("int".to_string())
                } else if ty == "f32" || ty == "f64" || ty == "c_double" || ty == "c_float" {
                    Type::from_name("float".to_string())
                } else if ty == "c_void" {
                    UNIT
                } else if ss[0].ident == "libc" {
                    Type::from_name(ss[1].ident.clone())
                } else if ty == "stat" {
                    Type::from_name("stat".to_string())
                } else if ty == "time_t" {
                    Type::from_name("time_t".to_string())
                } else if ty == "Option" {
                    let arg = &last.args[0];
                    if matches!(arg, Self::Ptr(_, _)) {
                        arg.clone()
                    } else {
                        Self::Path(ss)
                    }
                } else if ty == "Box" {
                    Self::Ptr(Box::new(last.args[0].clone()), true)
                } else if let Some(name) = map.get(ty) {
                    Type::from_name(name.to_string())
                } else {
                    Self::Path(ss)
                }
            }
            Self::TraitObject(ts) => {
                Self::TraitObject(ts.into_iter().map(|t| t.into_c(map)).collect())
            }
            Self::TypeRelative(t, s) => Self::TypeRelative(Box::new(t.into_c(map)), s),
            Self::BareFn(f) => Self::BareFn(Box::new(f.into_c(map))),
            Self::Impl(ts) => Self::Impl(ts.into_iter().map(|t| t.into_c(map)).collect()),
            Self::Never | Self::Infer | Self::Err => self,
        }
    }

    pub fn contains(&self, s: &str) -> bool {
        match self {
            Self::Slice(t) | Self::Array(t, _) | Self::Ptr(t, _) | Self::Ref(t, _) => t.contains(s),
            Self::Tup(ts) | Self::TraitObject(ts) | Self::Impl(ts) => {
                ts.iter().any(|t| t.contains(s))
            }
            Self::Path(ss) => ss.iter().any(|seg| seg.contains(s)),
            Self::TypeRelative(t, seg) => t.contains(s) || seg.contains(s),
            Self::BareFn(f) => f.contains(s),
            Self::Never | Self::Infer | Self::Err => false,
        }
    }

    pub fn contains_slice(&self) -> bool {
        match self {
            Self::Slice(_) => true,
            Self::Array(t, _) | Self::Ptr(t, _) | Self::Ref(t, _) => t.contains_slice(),
            Self::Tup(ts) | Self::TraitObject(ts) | Self::Impl(ts) => {
                ts.iter().any(|t| t.contains_slice())
            }
            Self::Path(ss) => ss.iter().any(|seg| seg.contains_slice()),
            Self::TypeRelative(t, seg) => t.contains_slice() || seg.contains_slice(),
            Self::BareFn(f) => f.contains_slice(),
            Self::Never | Self::Infer | Self::Err => false,
        }
    }

    pub fn contains_array(&self) -> bool {
        match self {
            Self::Array(_, _) => true,
            Self::Slice(t) | Self::Ptr(t, _) | Self::Ref(t, _) => t.contains_array(),
            Self::Tup(ts) | Self::TraitObject(ts) | Self::Impl(ts) => {
                ts.iter().any(|t| t.contains_array())
            }
            Self::Path(ss) => ss.iter().any(|seg| seg.contains_array()),
            Self::TypeRelative(t, seg) => t.contains_array() || seg.contains_array(),
            Self::BareFn(f) => f.contains_array(),
            Self::Never | Self::Infer | Self::Err => false,
        }
    }

    pub fn contains_fn(&self) -> bool {
        match self {
            Self::BareFn(_) => true,
            Self::Slice(t) | Self::Array(t, _) | Self::Ptr(t, _) | Self::Ref(t, _) => {
                t.contains_fn()
            }
            Self::Tup(ts) | Self::TraitObject(ts) | Self::Impl(ts) => {
                ts.iter().any(|t| t.contains_fn())
            }
            Self::Path(ss) => ss.iter().any(|seg| seg.contains_fn()),
            Self::TypeRelative(t, seg) => t.contains_fn() || seg.contains_fn(),
            Self::Never | Self::Infer | Self::Err => false,
        }
    }

    pub fn contains_tuple(&self) -> bool {
        match self {
            Self::Slice(t) | Self::Array(t, _) | Self::Ptr(t, _) | Self::Ref(t, _) => {
                t.contains_tuple()
            }
            Self::Tup(ts) => ts.len() >= 2,
            Self::TraitObject(ts) | Self::Impl(ts) => ts.iter().any(|t| t.contains_tuple()),
            Self::Path(ss) => ss.iter().any(|seg| seg.contains_tuple()),
            Self::TypeRelative(t, seg) => t.contains_tuple() || seg.contains_tuple(),
            Self::BareFn(f) => f.contains_tuple(),
            Self::Never | Self::Infer | Self::Err => false,
        }
    }

    pub fn is_void_ptr(&self) -> bool {
        if let Self::Ptr(t, _) = self {
            t.as_ref() == &UNIT
        } else {
            false
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
            Self::Path(ss) => fmt_list(f, ss.iter(), "", "::", ""),
            Self::TypeRelative(t, s) => write!(f, "{}::{}", t, s),
            Self::TraitObject(ts) => fmt_list(f, ts.iter(), "dyn ", " + ", ""),
            Self::BareFn(sig) => write!(f, "{}", sig),
            Self::Never => write!(f, "!"),
            Self::Impl(ts) => fmt_list(f, ts.iter(), "impl ", " + ", ""),
            Self::Infer => write!(f, "_"),
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
                let init = format!(
                    "unsafe {{ std::mem::transmute([1u8; std::mem::size_of::<{}>()]) }}",
                    v.ty_str
                );
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

    pub fn as_function(&self) -> Option<&FunctionInfo> {
        if let ItemSort::Function(f) = &self.sort {
            Some(f)
        } else {
            None
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
}

pub fn parse(code: &str) -> Option<Vec<ParsedItem>> {
    let config = make_config(code);
    run_compiler(config, |compiler| {
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
                    let item_code = ok_or!(source_map.span_to_snippet(item.span), continue);
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
                            if !DERIVES.contains(&item_code.as_str()) {
                                continue;
                            }
                            if let Type::Path(mut ss) = Type::from_ty(i.self_ty, tcx) {
                                assert_eq!(ss.len(), 1);
                                let seg = ss.pop().unwrap();
                                derives.entry(seg.ident).or_default().insert(item_code);
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
                        ItemKind::Fn(sig, generics, _) => {
                            let signature = source_map.span_to_snippet(sig.span).unwrap();
                            let signature_ty =
                                FunTySig::from_fn_decl(sig.decl, generics.params, tcx);
                            ItemSort::Function(FunctionInfo {
                                signature,
                                signature_ty,
                            })
                        }
                        ItemKind::Use(_, _) => {
                            if item_code.is_empty() {
                                continue;
                            }
                            ItemSort::Use
                        }
                        _ => continue,
                    };
                    items.push(ParsedItem {
                        name,
                        code: item_code,
                        sort,
                    });
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
    })?
}

pub static DERIVES: [&str; 9] = [
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

pub fn parse_one(code: &str) -> Option<ParsedItem> {
    let mut parsed = parse(code)?;
    assert_eq!(parsed.len(), 1, "{}", code);
    Some(parsed.pop().unwrap())
}

pub fn parse_signature(code: &str) -> Option<(String, FunctionInfo)> {
    let parsed = parse_one(code)?;
    if let ParsedItem {
        name,
        sort: ItemSort::Function(f),
        ..
    } = parsed
    {
        Some((name, f))
    } else {
        panic!()
    }
}

pub fn normalize_result(code: &str) -> Option<String> {
    let config = make_config(code);
    let suggestions: Vec<_> = run_compiler(config, |compiler| {
        compiler.enter(|queries| {
            queries.global_ctxt().ok()?.enter(|tcx| {
                let mut visitor = ResultVisitor::new(tcx);
                tcx.hir().visit_all_item_likes_in_crate(&mut visitor);
                Some(visitor.suggestions)
            })
        })
    })??;
    Some(rustfix::apply_suggestions(code, &suggestions).expect(code))
}

pub fn resolve_free_consts(code: &str, quiet: bool) -> Option<String> {
    let config = make_config(code);
    let suggestions: Vec<_> = run_compiler(config, |compiler| {
        compiler.enter(|queries| {
            queries.global_ctxt().ok()?.enter(|tcx| {
                let mut visitor = FreeConstantVisitor::new(tcx);
                tcx.hir().visit_all_item_likes_in_crate(&mut visitor);
                let source_map = compiler.session().source_map();
                let suggestions = visitor
                    .undefined_constants
                    .into_iter()
                    .map(|span| {
                        let s = source_map.span_to_snippet(span).unwrap();
                        if !quiet {
                            println!("free const: {}", s);
                        }
                        let snippet = span_to_snippet(span, source_map);
                        make_suggestion(snippet, "1")
                    })
                    .collect();
                Some(suggestions)
            })
        })
    })??;
    Some(rustfix::apply_suggestions(code, &suggestions).unwrap())
}

pub fn resolve_free_types(code: &str, prefix: &str, quiet: bool) -> Option<String> {
    let full_code = format!("{}{}", prefix, code);
    let config = make_config(&full_code);
    let suggestions: Vec<_> = run_compiler(config, |compiler| {
        compiler.enter(|queries| {
            queries.global_ctxt().ok()?.enter(|tcx| {
                let mut visitor = FreeTypeVisitor::new(tcx);
                tcx.hir().visit_all_item_likes_in_crate(&mut visitor);
                let source_map = compiler.session().source_map();
                let types = visitor.undefined_types.into_iter().map(|(span, args)| {
                    let s = source_map.span_to_snippet(span).unwrap();
                    let replacement = if let Some(t) = LIBC_TYPE_ALIASES.get(s.as_str()) {
                        t.to_string()
                    } else if let Some(t) = STD_TYPES.get(s.as_str()) {
                        t.to_string()
                    } else if LIBC_TYPES.contains(s.as_str()) {
                        format!("libc::{}", s)
                    } else {
                        match s.as_str() {
                            "int" => "i32",
                            "std::os::unix::raw::c_void" | "void" | "Void" => "libc::c_void",
                            "std::os::unix::prelude::Pid" | "Pid" => "libc::pid_t",
                            "TimeVal" | "time::TimeVal" => "libc::timeval",
                            "__sighandler_t" | "libc::__sighandler_t" => "libc::sighandler_t",
                            "SockaddrStorage" => "libc::sockaddr_storage",
                            "AddrInfo" => "libc::addrinfo",
                            "Passwd" | "libc::Passwd" => "libc::passwd",
                            "std::os::raw::ptrdiff_t" => "libc::ptrdiff_t",
                            _ => {
                                if !quiet {
                                    println!("free type: {}", s);
                                }
                                match args {
                                    0 => "usize",
                                    1 => "Box",
                                    _ => panic!("{}", s),
                                }
                            }
                        }
                        .to_string()
                    };
                    tracing::info!("free type: {} -> {}", s, replacement);
                    let snippet = span_to_snippet(span, source_map);
                    make_suggestion(snippet, &replacement)
                });
                let traits = visitor.undefined_traits.into_iter().map(|(span, _)| {
                    let s = source_map.span_to_snippet(span).unwrap();
                    let replacement = if let Some(t) = STD_TRAITS.get(s.as_str()) {
                        t
                    } else {
                        if !quiet {
                            println!("free trait: {}", s);
                        }
                        "Clone"
                    };
                    tracing::info!("free trait: {} -> {}", s, replacement);
                    let snippet = span_to_snippet(span, source_map);
                    make_suggestion(snippet, replacement)
                });
                Some(types.chain(traits).collect())
            })
        })
    })??;
    let full_code = rustfix::apply_suggestions(&full_code, &suggestions).expect(&full_code);
    Some(full_code.strip_prefix(prefix).unwrap().to_string())
}

pub fn resolve_imports(code: &str, prefix: &str) -> Option<String> {
    let full_code = format!("{}{}", prefix, code);
    let config = make_config(&full_code);
    let suggestions: Vec<_> = run_compiler(config, |compiler| {
        compiler.enter(|queries| {
            queries.global_ctxt().ok()?.enter(|tcx| {
                let hir = tcx.hir();
                let mut visitor = PathVisitor::new(tcx);
                for id in hir.items() {
                    visitor.visit_item(hir.item(id));
                }
                Some(visitor.suggestions)
            })
        })
    })??;
    let full_code = rustfix::apply_suggestions(&full_code, &suggestions).unwrap();
    Some(full_code.strip_prefix(prefix).unwrap().to_string())
}

pub fn rename_params(code: &str) -> Option<String> {
    let config = make_config(code);
    let suggestions: Vec<_> = run_compiler(config, |compiler| {
        compiler.enter(|queries| {
            queries.global_ctxt().ok()?.enter(|tcx| {
                let hir = tcx.hir();
                let source_map = compiler.session().source_map();
                let mut suggestions = vec![];
                for id in hir.items() {
                    if let ItemKind::Fn(_, _, body_id) = &hir.item(id).kind {
                        for param in hir.body(*body_id).params {
                            let param_str = source_map.span_to_snippet(param.span).unwrap();
                            let pat_str = source_map.span_to_snippet(param.pat.span).unwrap();
                            let replacement = if param_str == "..." {
                                "".to_string()
                            } else if pat_str == "..." {
                                "varargs".to_string()
                            } else if pat_str.to_lowercase() == "self" {
                                "self_".to_string()
                            } else if pat_str.contains(|c: char| c.is_ascii_uppercase()) {
                                pat_str.to_lowercase()
                            } else {
                                continue;
                            };
                            let snippet = span_to_snippet(param.pat.span, source_map);
                            suggestions.push(make_suggestion(snippet, &replacement));
                        }
                    }
                }
                Some(suggestions)
            })
        })
    })??;
    Some(rustfix::apply_suggestions(code, &suggestions).unwrap())
}

pub fn rename_item(code: &str, new_name: &str) -> Option<String> {
    let config = make_config(code);
    let suggestions: Vec<_> = run_compiler(config, |compiler| {
        compiler.enter(|queries| {
            queries.global_ctxt().ok()?.enter(|tcx| {
                let source_map = compiler.session().source_map();
                let hir = tcx.hir();
                let mut suggestions = vec![];
                for id in hir.items() {
                    let item = hir.item(id);
                    if matches!(
                        item.kind,
                        ItemKind::Fn(_, _, _) | ItemKind::Static(_, _, _) | ItemKind::Const(_, _)
                    ) {
                        let snippet = span_to_snippet(item.ident.span, source_map);
                        let suggestion = make_suggestion(snippet, new_name);
                        suggestions.push(suggestion);
                    }
                }
                Some(suggestions)
            })
        })
    })??;
    Some(rustfix::apply_suggestions(code, &suggestions).unwrap())
}

pub fn add_trait_uses<'i, I: IntoIterator<Item = &'i String>>(
    code: &str,
    uses: I,
) -> Option<String> {
    let uses: String = uses.into_iter().map(|s| format!("\n    {}", s)).collect();
    let config = make_config(code);
    let suggestions: Vec<_> = run_compiler(config, |compiler| {
        compiler.enter(|queries| {
            queries.global_ctxt().ok()?.enter(|tcx| {
                let hir = tcx.hir();
                for id in hir.items() {
                    if let ItemKind::Fn(_, _, body_id) = hir.item(id).kind {
                        let body = hir.body(body_id);
                        let span = body.value.span;
                        let span = span
                            .with_lo(span.lo() + BytePos(1))
                            .with_hi(span.lo() + BytePos(1));
                        let snippet = span_to_snippet(span, compiler.session().source_map());
                        let suggestion = make_suggestion(snippet, &uses);
                        return Some(vec![suggestion]);
                    }
                }
                Some(vec![])
            })
        })
    })??;
    Some(rustfix::apply_suggestions(code, &suggestions).unwrap())
}

pub fn resolve_sync(code: &str, prefix: &str) -> Option<String> {
    let inner = EmitterInner::default();
    let inner = Arc::new(Mutex::new(inner));
    let cloned_inner = inner.clone();

    let full_code = format!("{}{}", prefix, code);
    let mut config = make_config(&full_code);
    config.parse_sess_created = Some(Box::new(|ps: &mut ParseSess| {
        ps.span_diagnostic = Handler::with_emitter(
            false,
            None,
            Box::new(CollectingEmitter::new(cloned_inner, ps.clone_source_map())),
        );
    }));
    let not_sync = run_compiler(config, |compiler| {
        compiler.enter(|queries| {
            queries.global_ctxt().ok()?.enter(|tcx| {
                let _ = tcx.analysis(());
                Some(())
            })
        })?;
        let not_sync = inner.lock().unwrap().diagnostics.iter().any(|diag| {
            diag.message
                .iter()
                .any(|msg| msg.contains("cannot be shared between threads safely"))
        });
        Some(not_sync)
    })??;
    if not_sync {
        let code = code
            .strip_prefix("static mut ")
            .unwrap_or_else(|| code.strip_prefix("static ").expect(&full_code));
        Some("const ".to_string() + code)
    } else {
        Some(code.to_string())
    }
}

pub fn check_derive(code: &str) -> BTreeMap<String, BTreeSet<String>> {
    let inner = EmitterInner::default();
    let inner = Arc::new(Mutex::new(inner));
    let cloned_inner = inner.clone();

    let mut config = make_config(code);
    config.parse_sess_created = Some(Box::new(|ps: &mut ParseSess| {
        ps.span_diagnostic = Handler::with_emitter(
            false,
            None,
            Box::new(CollectingEmitter::new(cloned_inner, ps.clone_source_map())),
        );
    }));
    run_compiler(config, |compiler| {
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
    .unwrap()
}

#[derive(Debug, Clone)]
pub struct TypeCheckingResult {
    pub stage: usize,
    pub errors: Vec<TypeError>,
}

pub const MAX_STAGE: usize = 6;

impl TypeCheckingResult {
    pub fn passed(&self) -> bool {
        self.errors.is_empty()
    }
}

#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: String,
    pub line: usize,
    pub fix: Option<PossibleFix>,
}

impl TypeError {
    pub fn suggestion(&self) -> Option<&Vec<Suggestion>> {
        self.fix.as_ref().and_then(|fix| match fix {
            PossibleFix::Suggestion(suggestions) => Some(suggestions),
            _ => None,
        })
    }

    pub fn new_use(&self) -> Option<&str> {
        self.fix.as_ref().and_then(|fix| match fix {
            PossibleFix::Use(u) => Some(u.as_str()),
            _ => None,
        })
    }

    pub fn trait_use(&self) -> Option<&str> {
        self.fix.as_ref().and_then(|fix| match fix {
            PossibleFix::UseTrait(u) => Some(u.as_str()),
            _ => None,
        })
    }
}

#[derive(Debug, Clone)]
pub enum PossibleFix {
    Suggestion(Vec<Suggestion>),
    Use(String),
    UseTrait(String),
}

const IMPORT_TRAIT_MSG: &str = "implemented but not in scope; perhaps add a `use` for";
const IMPORT_TRAIT_MSG2: &str =
    "another candidate was found in the following trait, perhaps add a `use` for it";
const IMPORT_MSG: &str = "consider importing";
const CHANGE_IMPORT_MSG: &str = "you can use `as` to change the binding name of the import";
const BINDING_MSG: &str = "you might have meant to introduce a new binding";
const RELAX_MSG: &str = "consider relaxing the implicit `Sized` restriction";
const DOTS_MSG: &str = "you might have meant to write `.` instead of `..`";
const COMMA_MSG: &str = "missing `,`";
const LET_MSG: &str = "maybe you meant to write an assignment here";
const ANNOTATION_MSG: &str = "consider annotating";
const UB_MSG: &str = "The rules on what exactly is undefined behavior aren't clear, so this check might be overzealous. Please open an issue on the rustc repository if you believe it should not be considered undefined behavior.";
const BRACE_MSG: &str = "try adding braces";
const MACRO_MSG: &str = "use `!` to invoke the macro";
const FIELD_MSG: &str = "you might have meant to use field";
const END_TYPE_PARAM_MSG: &str = "you might have meant to end the type parameters here";
const PATH_SEP_MSG: &str = "maybe write a path separator here";
const FAT_ARROW_MSG: &str = "try using a fat arrow here";

pub fn type_check(code: &str) -> Option<TypeCheckingResult> {
    let inner = EmitterInner::default();
    let inner = Arc::new(Mutex::new(inner));
    let cloned_inner = inner.clone();

    let mut config = make_config(code);
    config.parse_sess_created = Some(Box::new(|ps: &mut ParseSess| {
        ps.span_diagnostic = Handler::with_emitter(
            false,
            None,
            Box::new(CollectingEmitter::new(cloned_inner, ps.clone_source_map())),
        );
    }));
    run_compiler(config, |compiler| {
        let stage = compiler.enter(|queries| {
            queries.global_ctxt().ok()?.enter(|tcx| {
                let mut visitor = FreeTypeVisitor::new(tcx);
                tcx.hir().visit_all_item_likes_in_crate(&mut visitor);
                if !visitor.undefined_traits.is_empty() {
                    return None;
                }

                rustc_passes::hir_id_validator::check_crate(tcx);
                let _ = tcx.entry_fn(());
                tcx.ensure().proc_macro_decls_static(());
                tcx.hir().par_for_each_module(|module| {
                    tcx.ensure().check_mod_loops(module);
                    tcx.ensure().check_mod_attrs(module);
                    tcx.ensure().check_mod_naked_functions(module);
                    tcx.ensure().check_mod_unstable_api_usage(module);
                    tcx.ensure().check_mod_const_bodies(module);
                });
                rustc_passes::stability::check_unused_or_stable_features(tcx);
                tcx.ensure().limits(());
                tcx.ensure().stability_index(());

                if tcx
                    .sess
                    .track_errors(|| {
                        tcx.hir()
                            .for_each_module(|module| tcx.ensure().collect_mod_item_types(module))
                    })
                    .is_err()
                {
                    return Some(0);
                }
                if tcx
                    .sess
                    .track_errors(|| {
                        tcx.hir()
                            .for_each_module(|module| tcx.ensure().check_mod_impl_wf(module))
                    })
                    .is_err()
                {
                    return Some(1);
                }
                #[allow(clippy::blocks_in_if_conditions)]
                if tcx
                    .sess
                    .track_errors(|| {
                        for &trait_def_id in tcx.all_local_trait_impls(()).keys() {
                            tcx.ensure().coherent_trait(trait_def_id);
                        }
                        tcx.ensure().crate_inherent_impls(());
                        tcx.ensure().crate_inherent_impls_overlap_check(());
                    })
                    .is_err()
                {
                    return Some(2);
                }
                if tcx
                    .sess
                    .track_errors(|| {
                        tcx.hir()
                            .par_for_each_module(|module| tcx.ensure().check_mod_type_wf(module))
                    })
                    .is_err()
                {
                    return Some(3);
                }
                if rustc_hir_analysis::check_crate(tcx).is_err() {
                    return Some(4);
                }
                if tcx.analysis(()).is_err() {
                    return Some(5);
                }
                Some(6)
            })
        })?;
        let mut errors = vec![];
        let source_map = compiler.session().source_map();
        for diag in inner.lock().unwrap().diagnostics.iter() {
            let message = format!("{}", WithSourceMap::new(source_map, diag));
            if message.contains(UB_MSG) {
                continue;
            }
            let message: String = message
                .lines()
                .filter(|s| !s.trim().is_empty())
                .intersperse("\n")
                .collect();
            let line = diag.span.primary_line(source_map);
            let fix = diag.suggestions.iter().find_map(|sugg| {
                let msg = &sugg.msg;
                let subst = &sugg.substitutions[0];
                if subst
                    .parts
                    .iter()
                    .any(|(span, _)| !internal_span(span.span(), source_map))
                {
                    return None;
                }
                let (is_suggestion, is_trait) = match &sugg.applicability {
                    Applicability::HasPlaceholders => return None,
                    Applicability::MachineApplicable => {
                        if msg.contains(RELAX_MSG)
                            || message.contains("can't pass `f32` to variadic function")
                        {
                            return None;
                        } else {
                            (true, false)
                        }
                    }
                    _ => {
                        if msg.contains(IMPORT_MSG) {
                            (false, false)
                        } else if msg.contains(IMPORT_TRAIT_MSG) || msg.contains(IMPORT_TRAIT_MSG2)
                        {
                            (false, true)
                        } else if msg.contains(BINDING_MSG)
                            || msg.contains(DOTS_MSG)
                            || msg.contains(COMMA_MSG)
                            || msg.contains(LET_MSG)
                            || msg.contains(ANNOTATION_MSG)
                            || msg.contains(CHANGE_IMPORT_MSG)
                            || msg.contains(BRACE_MSG)
                            || msg.contains(MACRO_MSG)
                            || msg.contains(FIELD_MSG)
                            || msg.contains(END_TYPE_PARAM_MSG)
                            || msg.contains(PATH_SEP_MSG)
                            || msg.contains(FAT_ARROW_MSG)
                        {
                            return None;
                        } else {
                            (true, false)
                        }
                    }
                };
                let mk_suggestion = || {
                    let suggestions = subst
                        .parts
                        .iter()
                        .map(|(span, replacement)| {
                            let snippet = span_to_snippet(span.span(), source_map);
                            make_suggestion(snippet, replacement)
                        })
                        .collect();
                    PossibleFix::Suggestion(suggestions)
                };
                let fix = if is_suggestion {
                    mk_suggestion()
                } else {
                    assert_eq!(subst.parts.len(), 1);
                    let s = subst.parts[0].1.trim().to_string();
                    if s.starts_with("use ") {
                        assert!(s.ends_with(';'), "{}", s);
                        if is_trait {
                            PossibleFix::UseTrait(s)
                        } else {
                            PossibleFix::Use(s)
                        }
                    } else {
                        mk_suggestion()
                    }
                };
                Some(fix)
            });
            let error = TypeError { message, line, fix };
            errors.push(error);
        }
        Some(TypeCheckingResult { stage, errors })
    })?
}

struct FreeTypeVisitor<'tcx> {
    tcx: TyCtxt<'tcx>,
    undefined_traits: Vec<(Span, usize)>,
    undefined_types: Vec<(Span, usize)>,
}

impl<'tcx> FreeTypeVisitor<'tcx> {
    fn new(tcx: TyCtxt<'tcx>) -> Self {
        Self {
            tcx,
            undefined_traits: vec![],
            undefined_types: vec![],
        }
    }
}

fn span_and_args_of_path(path: &Path<'_>) -> Option<(Span, usize)> {
    if path.res == Res::Err {
        let span1 = path.segments.first().unwrap().ident.span;
        let last = path.segments.last().unwrap();
        let span2 = last.ident.span;
        let args = last.args.map(|args| args.args.len()).unwrap_or(0);
        Some((span1.with_hi(span2.hi()), args))
    } else {
        None
    }
}

impl<'tcx> Visitor<'tcx> for FreeTypeVisitor<'tcx> {
    type NestedFilter = nested_filter::OnlyBodies;

    fn nested_visit_map(&mut self) -> Self::Map {
        self.tcx.hir()
    }

    fn visit_trait_ref(&mut self, t: &'tcx TraitRef<'tcx>) {
        if t.trait_def_id().is_none() {
            if let Some(x) = span_and_args_of_path(t.path) {
                self.undefined_traits.push(x);
            }
        }
        intravisit::walk_trait_ref(self, t)
    }

    fn visit_ty(&mut self, ty: &'tcx Ty<'tcx>) {
        if let TyKind::Path(QPath::Resolved(_, path)) = &ty.kind {
            if let Some(x) = span_and_args_of_path(path) {
                self.undefined_types.push(x);
            }
        }
        intravisit::walk_ty(self, ty);
    }
}

struct FreeConstantVisitor<'tcx> {
    tcx: TyCtxt<'tcx>,
    undefined_constants: Vec<Span>,
}

impl<'tcx> FreeConstantVisitor<'tcx> {
    fn new(tcx: TyCtxt<'tcx>) -> Self {
        Self {
            tcx,
            undefined_constants: vec![],
        }
    }
}

impl<'tcx> Visitor<'tcx> for FreeConstantVisitor<'tcx> {
    type NestedFilter = nested_filter::OnlyBodies;

    fn nested_visit_map(&mut self) -> Self::Map {
        self.tcx.hir()
    }

    fn visit_expr(&mut self, e: &'tcx Expr<'tcx>) {
        if let ExprKind::Path(QPath::Resolved(_, p)) = &e.kind {
            if p.res == Res::Err {
                self.undefined_constants.push(e.span);
            }
        }
        intravisit::walk_expr(self, e);
    }
}

struct ResultVisitor<'tcx> {
    tcx: TyCtxt<'tcx>,
    suggestions: Vec<Suggestion>,
}

impl<'tcx> ResultVisitor<'tcx> {
    fn new(tcx: TyCtxt<'tcx>) -> Self {
        Self {
            tcx,
            suggestions: vec![],
        }
    }
}

impl<'tcx> Visitor<'tcx> for ResultVisitor<'tcx> {
    type NestedFilter = nested_filter::OnlyBodies;

    fn nested_visit_map(&mut self) -> Self::Map {
        self.tcx.hir()
    }

    fn visit_path(&mut self, path: &Path<'tcx>, _: HirId) {
        let PathSegment { ident, args, .. } = path.segments.iter().last().unwrap();
        let id = ident.name.to_ident_string();
        if id == "Result" {
            let args = if let Some(args) = args {
                args.args
                    .iter()
                    .filter(|arg| matches!(arg, GenericArg::Type(_)))
                    .collect()
            } else {
                vec![]
            };
            let source_map = self.tcx.sess.source_map();
            let ty = if let Some(arg) = args.get(0) {
                source_map.span_to_snippet(arg.span()).unwrap()
            } else {
                "()".to_string()
            };
            let snippet = span_to_snippet(path.span, source_map);
            let suggestion = make_suggestion(snippet, &format!("Result<{}, ()>", ty));
            self.suggestions.push(suggestion);
        } else {
            intravisit::walk_path(self, path);
        }
    }
}

struct PathVisitor<'tcx> {
    tcx: TyCtxt<'tcx>,
    suggestions: Vec<Suggestion>,
}

impl<'tcx> PathVisitor<'tcx> {
    fn new(tcx: TyCtxt<'tcx>) -> Self {
        Self {
            tcx,
            suggestions: vec![],
        }
    }
}

impl<'tcx> Visitor<'tcx> for PathVisitor<'tcx> {
    type NestedFilter = nested_filter::OnlyBodies;

    fn nested_visit_map(&mut self) -> Self::Map {
        self.tcx.hir()
    }

    fn visit_path(&mut self, path: &Path<'tcx>, _: HirId) {
        let seg = &path.segments[0];
        if let Res::Def(_, def_id) = seg.res {
            if self.tcx.hir().get_if_local(def_id).is_none() {
                let seg_ident = seg.ident.name.to_ident_string();
                let full_path = self.tcx.def_path_str(def_id);
                if full_path != seg_ident
                    && !full_path.starts_with("std::prelude")
                    && !PRELUDES.contains(full_path.as_str())
                {
                    let snippet = span_to_snippet(seg.ident.span, self.tcx.sess.source_map());
                    let suggestion = make_suggestion(snippet, &full_path);
                    self.suggestions.push(suggestion);
                }
            }
        }
        intravisit::walk_path(self, path)
    }
}

pub fn overlap(s1: &Suggestion, s2: &Suggestion) -> bool {
    let r1 = &s1.snippets[0].range;
    let r2 = &s2.snippets[0].range;
    (r1.start <= r2.end) && (r2.start <= r1.end)
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

fn internal_span(span: Span, source_map: &SourceMap) -> bool {
    source_map.span_to_filename(span) == FileName::Custom("main.rs".to_string())
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_result() {
        assert_eq!(
            normalize_result("fn f() -> Result<usize, ()> {}").unwrap(),
            "fn f() -> Result<usize, ()> {}"
        );
        assert_eq!(
            normalize_result("fn f() -> Result<usize, usize> {}").unwrap(),
            "fn f() -> Result<usize, ()> {}"
        );
        assert_eq!(
            normalize_result("fn f() -> std::result::Result<usize, ()> {}").unwrap(),
            "fn f() -> Result<usize, ()> {}"
        );
        assert_eq!(
            normalize_result("fn f() -> std::io::Result<usize> {}").unwrap(),
            "fn f() -> Result<usize, ()> {}"
        );
        assert_eq!(
            normalize_result("fn f() -> std::fmt::Result {}").unwrap(),
            "fn f() -> Result<(), ()> {}"
        );
    }

    #[test]
    fn test_free_types() {
        assert_eq!(
            resolve_free_types("fn foo(x: Foo) {}", "", true).unwrap(),
            "fn foo(x: usize) {}"
        );
        assert_eq!(
            resolve_free_types("fn foo() -> Foo {}", "", true).unwrap(),
            "fn foo() -> usize {}"
        );
        assert_eq!(
            resolve_free_types("fn foo<T: Read>(t: T) {}", "", true).unwrap(),
            "fn foo<T: std::io::Read>(t: T) {}"
        );
        assert_eq!(
            resolve_free_types("fn foo<T: io::Read>(t: T) {}", "", true).unwrap(),
            "fn foo<T: std::io::Read>(t: T) {}"
        );
        assert_eq!(
            resolve_free_types("fn foo<T: Seek>(t: T) {}", "", true).unwrap(),
            "fn foo<T: std::io::Seek>(t: T) {}"
        );
        assert_eq!(
            resolve_free_types("fn foo<T: io::Seek>(t: T) {}", "", true).unwrap(),
            "fn foo<T: std::io::Seek>(t: T) {}"
        );
        assert_eq!(
            resolve_free_types("fn foo<T: Write>(t: T) {}", "", true).unwrap(),
            "fn foo<T: std::io::Write>(t: T) {}"
        );
        assert_eq!(
            resolve_free_types("fn foo<T: io::Write>(t: T) {}", "", true).unwrap(),
            "fn foo<T: std::io::Write>(t: T) {}"
        );
        assert_eq!(
            resolve_free_types("fn foo() -> Foo {}", "type Foo = usize;", true).unwrap(),
            "fn foo() -> Foo {}"
        );
        assert_eq!(
            resolve_free_types("fn foo<T: Foo>(t: T) {}", "trait Foo {}", true).unwrap(),
            "fn foo<T: Foo>(t: T) {}"
        );
        assert_eq!(
            resolve_free_types("fn foo() -> path::Path {}", "", true).unwrap(),
            "fn foo() -> std::path::Path {}"
        );
        assert_eq!(
            resolve_free_types("fn foo() -> Path {}", "use std::path;", true).unwrap(),
            "fn foo() -> std::path::Path {}"
        );
        assert_eq!(
            resolve_free_types("fn foo() -> std::path::Path {}", "", true).unwrap(),
            "fn foo() -> std::path::Path {}"
        );
        assert_eq!(
            resolve_free_types("fn foo() -> path::Path {}", "use std::path;", true).unwrap(),
            "fn foo() -> path::Path {}"
        );
        assert_eq!(
            resolve_free_types("fn foo() -> Path {}", "use std::path::Path;", true).unwrap(),
            "fn foo() -> Path {}"
        );
        assert_eq!(
            resolve_free_types("fn foo() -> Rc<usize> {}", "", true).unwrap(),
            "fn foo() -> std::rc::Rc<usize> {}"
        );
        assert_eq!(
            resolve_free_types("fn foo() -> rc::Rc<usize> {}", "", true).unwrap(),
            "fn foo() -> std::rc::Rc<usize> {}"
        );
        assert_eq!(
            resolve_free_types("fn foo() -> RefCell<usize> {}", "", true).unwrap(),
            "fn foo() -> std::cell::RefCell<usize> {}"
        );
        assert_eq!(
            resolve_free_types("fn foo() -> cell::RefCell<usize> {}", "", true).unwrap(),
            "fn foo() -> std::cell::RefCell<usize> {}"
        );
        assert_eq!(
            resolve_free_types("fn foo() -> Rc<RefCell<usize>> {}", "", true).unwrap(),
            "fn foo() -> std::rc::Rc<std::cell::RefCell<usize>> {}"
        );
        assert_eq!(
            resolve_free_types("fn foo() -> RefCell<Rc<usize>> {}", "", true).unwrap(),
            "fn foo() -> std::cell::RefCell<std::rc::Rc<usize>> {}"
        );
    }

    #[test]
    fn test_import() {
        assert_eq!(
            resolve_imports("fn f() -> Path {}", "use std::path::Path;").unwrap(),
            "fn f() -> std::path::Path {}"
        );
        assert_eq!(
            resolve_imports("fn f() -> path::Path {}", "use std::path;").unwrap(),
            "fn f() -> std::path::Path {}"
        );
        assert_eq!(
            resolve_imports("fn f() -> Rc<usize> {}", "use std::rc::Rc;").unwrap(),
            "fn f() -> std::rc::Rc<usize> {}"
        );
        assert_eq!(
            resolve_imports("fn f() -> rc::Rc<usize> {}", "use std::rc;").unwrap(),
            "fn f() -> std::rc::Rc<usize> {}"
        );
        assert_eq!(
            resolve_imports("fn f() -> Rc<Rc<usize>> {}", "use std::rc::Rc;").unwrap(),
            "fn f() -> std::rc::Rc<std::rc::Rc<usize>> {}"
        );
        assert_eq!(
            resolve_imports("fn f() -> rc::Rc<rc::Rc<usize>> {}", "use std::rc;").unwrap(),
            "fn f() -> std::rc::Rc<std::rc::Rc<usize>> {}"
        );
        assert_eq!(
            resolve_imports("fn f() { read(\"\"); }", "use std::fs::read;").unwrap(),
            "fn f() { std::fs::read(\"\"); }"
        );
        assert_eq!(
            resolve_imports("fn f() { fs::read(\"\"); }", "use std::fs;").unwrap(),
            "fn f() { std::fs::read(\"\"); }"
        );
    }
}

lazy_static! {
    static ref INT_TYPES: BTreeSet<&'static str> = INT_TYPES_RAW.iter().copied().collect();
    static ref PRELUDES: BTreeSet<&'static str> = PRELUDES_RAW.iter().copied().collect();
    static ref LIBC_TYPES: BTreeSet<&'static str> = LIBC_TYPES_RAW.iter().copied().collect();
    static ref LIBC_TYPE_ALIASES: BTreeMap<&'static str, &'static str> =
        LIBC_TYPE_ALIASES_RAW.iter().copied().collect();
    static ref STD_TYPES: BTreeMap<&'static str, &'static str> = raw_to_map(&STD_TYPES_RAW);
    static ref STD_TRAITS: BTreeMap<&'static str, &'static str> = raw_to_map(&STD_TRAITS_RAW);
}

fn raw_to_map(arr: &'static [&'static str]) -> BTreeMap<&'static str, &'static str> {
    let mut map = BTreeMap::new();
    for ty in arr {
        let mut partial_ty = *ty;
        while let Some(i) = partial_ty.find("::") {
            partial_ty = &partial_ty[i + 2..];
            map.insert(partial_ty, *ty);
        }
    }
    map
}

static INT_TYPES_RAW: [&str; 46] = [
    "char",
    "u8",
    "u16",
    "u32",
    "u64",
    "usize",
    "i8",
    "i16",
    "i32",
    "i64",
    "isize",
    "__s16",
    "__s32",
    "__s64",
    "__syscall_ulong_t",
    "__u8",
    "__u16",
    "__u32",
    "__u64",
    "c_char",
    "c_int",
    "c_long",
    "c_longlong",
    "c_schar",
    "c_short",
    "c_uchar",
    "c_uint",
    "c_ulong",
    "c_ulonglong",
    "c_ushort",
    "int8_t",
    "int16_t",
    "int32_t",
    "int64_t",
    "intmax_t",
    "intptr_t",
    "off64_t",
    "off_t",
    "size_t",
    "ssize_t",
    "uint8_t",
    "uint16_t",
    "uint32_t",
    "uint64_t",
    "uintmax_t",
    "uintptr_t",
];

static PRELUDES_RAW: [&str; 36] = [
    "std::marker::Copy",
    "std::marker::Send",
    "std::marker::Sized",
    "std::marker::Sync",
    "std::marker::Unpin",
    "std::ops::Drop",
    "std::ops::Fn",
    "std::ops::FnMut",
    "std::ops::FnOnce",
    "std::mem::drop",
    "std::boxed::Box",
    "std::borrow::ToOwned",
    "std::clone::Clone",
    "std::cmp::PartialEq",
    "std::cmp::PartialOrd",
    "std::cmp::Eq",
    "std::cmp::Ord",
    "std::convert::AsRef",
    "std::convert::AsMut",
    "std::convert::Into",
    "std::convert::From",
    "std::default::Default",
    "std::iter::Iterator",
    "std::iter::Extend",
    "std::iter::IntoIterator",
    "std::iter::DoubleEndedIterator",
    "std::iter::ExactSizeIterator",
    "std::option::Option",
    "std::option::Option::Some",
    "std::option::Option::None",
    "std::result::Result",
    "std::result::Result::Ok",
    "std::result::Result::Err",
    "std::string::String",
    "std::string::ToString",
    "std::vec::Vec",
];

static LIBC_TYPES_RAW: [&str; 278] = [
    "Dl_info",
    "Elf32_Chdr",
    "Elf32_Ehdr",
    "Elf32_Phdr",
    "Elf32_Shdr",
    "Elf32_Sym",
    "Elf64_Chdr",
    "Elf64_Ehdr",
    "Elf64_Phdr",
    "Elf64_Shdr",
    "Elf64_Sym",
    "__c_anonymous_ifru_map",
    "__c_anonymous_ptrace_syscall_info_entry",
    "__c_anonymous_ptrace_syscall_info_exit",
    "__c_anonymous_ptrace_syscall_info_seccomp",
    "__c_anonymous_sockaddr_can_j1939",
    "__c_anonymous_sockaddr_can_tp",
    "__exit_status",
    "__timeval",
    "_libc_fpstate",
    "_libc_fpxreg",
    "_libc_xmmreg",
    "addrinfo",
    "af_alg_iv",
    "aiocb",
    "arpd_request",
    "arphdr",
    "arpreq",
    "arpreq_old",
    "can_filter",
    "can_frame",
    "canfd_frame",
    "clone_args",
    "cmsghdr",
    "cpu_set_t",
    "dirent",
    "dirent64",
    "dl_phdr_info",
    "dqblk",
    "epoll_event",
    "fanotify_event_metadata",
    "fanotify_response",
    "fd_set",
    "ff_condition_effect",
    "ff_constant_effect",
    "ff_effect",
    "ff_envelope",
    "ff_periodic_effect",
    "ff_ramp_effect",
    "ff_replay",
    "ff_rumble_effect",
    "ff_trigger",
    "file_clone_range",
    "flock",
    "flock64",
    "fsid_t",
    "genlmsghdr",
    "glob64_t",
    "glob_t",
    "group",
    "hostent",
    "hwtstamp_config",
    "if_nameindex",
    "ifaddrs",
    "ifreq",
    "in6_addr",
    "in6_ifreq",
    "in6_pktinfo",
    "in6_rtmsg",
    "in_addr",
    "in_pktinfo",
    "inotify_event",
    "input_absinfo",
    "input_event",
    "input_id",
    "input_keymap_entry",
    "input_mask",
    "iovec",
    "ip_mreq",
    "ip_mreq_source",
    "ip_mreqn",
    "ipc_perm",
    "ipv6_mreq",
    "itimerspec",
    "itimerval",
    "j1939_filter",
    "lconv",
    "linger",
    "mallinfo",
    "mallinfo2",
    "max_align_t",
    "mcontext_t",
    "mmsghdr",
    "mntent",
    "mq_attr",
    "msghdr",
    "msginfo",
    "msqid_ds",
    "nl_mmap_hdr",
    "nl_mmap_req",
    "nl_pktinfo",
    "nlattr",
    "nlmsgerr",
    "nlmsghdr",
    "ntptimeval",
    "open_how",
    "option",
    "packet_mreq",
    "passwd",
    "pollfd",
    "posix_spawn_file_actions_t",
    "posix_spawnattr_t",
    "protoent",
    "pthread_attr_t",
    "pthread_barrier_t",
    "pthread_barrierattr_t",
    "pthread_cond_t",
    "pthread_condattr_t",
    "pthread_mutex_t",
    "pthread_mutexattr_t",
    "pthread_rwlock_t",
    "pthread_rwlockattr_t",
    "ptrace_peeksiginfo_args",
    "ptrace_rseq_configuration",
    "ptrace_syscall_info",
    "regex_t",
    "regmatch_t",
    "rlimit",
    "rlimit64",
    "rtentry",
    "rusage",
    "sched_param",
    "sctp_authinfo",
    "sctp_initmsg",
    "sctp_nxtinfo",
    "sctp_prinfo",
    "sctp_rcvinfo",
    "sctp_sndinfo",
    "sctp_sndrcvinfo",
    "seccomp_data",
    "seccomp_notif_sizes",
    "sem_t",
    "sembuf",
    "semid_ds",
    "seminfo",
    "servent",
    "shmid_ds",
    "sigaction",
    "sigevent",
    "siginfo_t",
    "signalfd_siginfo",
    "sigset_t",
    "sigval",
    "sock_extended_err",
    "sock_filter",
    "sock_fprog",
    "sock_txtime",
    "sockaddr",
    "sockaddr_alg",
    "sockaddr_can",
    "sockaddr_in",
    "sockaddr_in6",
    "sockaddr_ll",
    "sockaddr_nl",
    "sockaddr_storage",
    "sockaddr_un",
    "sockaddr_vm",
    "spwd",
    "stack_t",
    "stat",
    "stat64",
    "statfs",
    "statfs64",
    "statvfs",
    "statvfs64",
    "statx",
    "statx_timestamp",
    "sysinfo",
    "termios",
    "termios2",
    "timespec",
    "timeval",
    "timex",
    "tm",
    "tms",
    "ucontext_t",
    "ucred",
    "uinput_abs_setup",
    "uinput_ff_erase",
    "uinput_ff_upload",
    "uinput_setup",
    "uinput_user_dev",
    "user",
    "user_fpregs_struct",
    "user_regs_struct",
    "utimbuf",
    "utmpx",
    "utsname",
    "winsize",
    "DIR",
    "FILE",
    "c_void",
    "fpos64_t",
    "fpos_t",
    "timezone",
    "__c_anonymous_ifr_ifru",
    "__c_anonymous_ptrace_syscall_info_data",
    "__c_anonymous_sockaddr_can_can_addr",
    "Elf32_Addr",
    "Elf32_Half",
    "Elf32_Off",
    "Elf32_Section",
    "Elf32_Word",
    "Elf64_Addr",
    "Elf64_Half",
    "Elf64_Off",
    "Elf64_Section",
    "Elf64_Sxword",
    "Elf64_Word",
    "Elf64_Xword",
    "Lmid_t",
    "__fsword_t",
    "__priority_which_t",
    "__rlimit_resource_t",
    "blkcnt64_t",
    "blkcnt_t",
    "blksize_t",
    "can_err_mask_t",
    "canid_t",
    "cc_t",
    "clock_t",
    "clockid_t",
    "dev_t",
    "fsblkcnt_t",
    "fsfilcnt_t",
    "gid_t",
    "greg_t",
    "iconv_t",
    "id_t",
    "idtype_t",
    "in_addr_t",
    "in_port_t",
    "ino64_t",
    "ino_t",
    "key_t",
    "locale_t",
    "loff_t",
    "mode_t",
    "mqd_t",
    "msglen_t",
    "msgqnum_t",
    "name_t",
    "nfds_t",
    "nl_item",
    "nlink_t",
    "pgn_t",
    "pid_t",
    "priority_t",
    "pthread_key_t",
    "pthread_spinlock_t",
    "pthread_t",
    "ptrdiff_t",
    "regoff_t",
    "rlim64_t",
    "rlim_t",
    "sa_family_t",
    "sctp_assoc_t",
    "shmatt_t",
    "sighandler_t",
    "socklen_t",
    "speed_t",
    "suseconds_t",
    "tcflag_t",
    "time_t",
    "timer_t",
    "uid_t",
    "useconds_t",
    "wchar_t",
];

static LIBC_TYPE_ALIASES_RAW: [(&str, &str); 37] = [
    ("__s16", "i16"),
    ("__s32", "i32"),
    ("__s64", "i64"),
    ("__syscall_ulong_t", "u64"),
    ("__u8", "u8"),
    ("__u16", "u16"),
    ("__u32", "u32"),
    ("__u64", "u64"),
    ("c_char", "u8"),
    ("c_double", "double"),
    ("c_float", "float"),
    ("c_int", "i32"),
    ("c_long", "i64"),
    ("c_longlong", "i64"),
    ("c_schar", "i8"),
    ("c_short", "i16"),
    ("c_uchar", "u8"),
    ("c_uint", "u32"),
    ("c_ulong", "u64"),
    ("c_ulonglong", "u64"),
    ("c_ushort", "u16"),
    ("int8_t", "i8"),
    ("int16_t", "i16"),
    ("int32_t", "i32"),
    ("int64_t", "i64"),
    ("intmax_t", "i64"),
    ("intptr_t", "isize"),
    ("off64_t", "i64"),
    ("off_t", "i64"),
    ("size_t", "usize"),
    ("ssize_t", "isize"),
    ("uint8_t", "u8"),
    ("uint16_t", "u16"),
    ("uint32_t", "u32"),
    ("uint64_t", "u64"),
    ("uintmax_t", "u64"),
    ("uintptr_t", "usize"),
];

static STD_TYPES_RAW: [&str; 605] = [
    "std::alloc::AllocError",
    "std::alloc::Global",
    "std::alloc::Layout",
    "std::alloc::LayoutError",
    "std::alloc::System",
    "std::any::Demand",
    "std::any::TypeId",
    "std::array::IntoIter",
    "std::array::TryFromSliceError",
    "std::ascii::EscapeDefault",
    "std::async_iter::FromIter",
    "std::backtrace::Backtrace",
    "std::backtrace::BacktraceFrame",
    "std::boxed::Box",
    "std::boxed::ThinBox",
    "std::cell::BorrowError",
    "std::cell::BorrowMutError",
    "std::cell::Cell",
    "std::cell::LazyCell",
    "std::cell::OnceCell",
    "std::cell::Ref",
    "std::cell::RefCell",
    "std::cell::RefMut",
    "std::cell::SyncUnsafeCell",
    "std::cell::UnsafeCell",
    "std::char::CharTryFromError",
    "std::char::DecodeUtf16",
    "std::char::DecodeUtf16Error",
    "std::char::EscapeDebug",
    "std::char::EscapeDefault",
    "std::char::EscapeUnicode",
    "std::char::ParseCharError",
    "std::char::ToLowercase",
    "std::char::ToUppercase",
    "std::char::TryFromCharError",
    "std::cmp::Reverse",
    "std::collections::BTreeMap",
    "std::collections::BTreeSet",
    "std::collections::BinaryHeap",
    "std::collections::HashMap",
    "std::collections::HashSet",
    "std::collections::LinkedList",
    "std::collections::TryReserveError",
    "std::collections::VecDeque",
    "std::collections::binary_heap::BinaryHeap",
    "std::collections::binary_heap::Drain",
    "std::collections::binary_heap::DrainSorted",
    "std::collections::binary_heap::IntoIter",
    "std::collections::binary_heap::IntoIterSorted",
    "std::collections::binary_heap::Iter",
    "std::collections::binary_heap::PeekMut",
    "std::collections::btree_map::BTreeMap",
    "std::collections::btree_map::Cursor",
    "std::collections::btree_map::CursorMut",
    "std::collections::btree_map::DrainFilter",
    "std::collections::btree_map::IntoIter",
    "std::collections::btree_map::IntoKeys",
    "std::collections::btree_map::IntoValues",
    "std::collections::btree_map::Iter",
    "std::collections::btree_map::IterMut",
    "std::collections::btree_map::Keys",
    "std::collections::btree_map::OccupiedEntry",
    "std::collections::btree_map::OccupiedError",
    "std::collections::btree_map::Range",
    "std::collections::btree_map::RangeMut",
    "std::collections::btree_map::VacantEntry",
    "std::collections::btree_map::Values",
    "std::collections::btree_map::ValuesMut",
    "std::collections::btree_set::BTreeSet",
    "std::collections::btree_set::Difference",
    "std::collections::btree_set::DrainFilter",
    "std::collections::btree_set::Intersection",
    "std::collections::btree_set::IntoIter",
    "std::collections::btree_set::Iter",
    "std::collections::btree_set::Range",
    "std::collections::btree_set::SymmetricDifference",
    "std::collections::btree_set::Union",
    "std::collections::hash_map::DefaultHasher",
    "std::collections::hash_map::Drain",
    "std::collections::hash_map::DrainFilter",
    "std::collections::hash_map::HashMap",
    "std::collections::hash_map::IntoIter",
    "std::collections::hash_map::IntoKeys",
    "std::collections::hash_map::IntoValues",
    "std::collections::hash_map::Iter",
    "std::collections::hash_map::IterMut",
    "std::collections::hash_map::Keys",
    "std::collections::hash_map::OccupiedEntry",
    "std::collections::hash_map::OccupiedError",
    "std::collections::hash_map::RandomState",
    "std::collections::hash_map::RawEntryBuilder",
    "std::collections::hash_map::RawEntryBuilderMut",
    "std::collections::hash_map::RawOccupiedEntryMut",
    "std::collections::hash_map::RawVacantEntryMut",
    "std::collections::hash_map::VacantEntry",
    "std::collections::hash_map::Values",
    "std::collections::hash_map::ValuesMut",
    "std::collections::hash_set::Difference",
    "std::collections::hash_set::Drain",
    "std::collections::hash_set::DrainFilter",
    "std::collections::hash_set::HashSet",
    "std::collections::hash_set::Intersection",
    "std::collections::hash_set::IntoIter",
    "std::collections::hash_set::Iter",
    "std::collections::hash_set::SymmetricDifference",
    "std::collections::hash_set::Union",
    "std::collections::linked_list::Cursor",
    "std::collections::linked_list::CursorMut",
    "std::collections::linked_list::DrainFilter",
    "std::collections::linked_list::IntoIter",
    "std::collections::linked_list::Iter",
    "std::collections::linked_list::IterMut",
    "std::collections::linked_list::LinkedList",
    "std::collections::vec_deque::Drain",
    "std::collections::vec_deque::IntoIter",
    "std::collections::vec_deque::Iter",
    "std::collections::vec_deque::IterMut",
    "std::collections::vec_deque::VecDeque",
    "std::env::Args",
    "std::env::ArgsOs",
    "std::env::JoinPathsError",
    "std::env::SplitPaths",
    "std::env::Vars",
    "std::env::VarsOs",
    "std::error::Report",
    "std::ffi::CStr",
    "std::ffi::CString",
    "std::ffi::FromBytesWithNulError",
    "std::ffi::FromVecWithNulError",
    "std::ffi::IntoStringError",
    "std::ffi::NulError",
    "std::ffi::OsStr",
    "std::ffi::OsString",
    "std::ffi::VaList",
    "std::ffi::VaListImpl",
    "std::fmt::Arguments",
    "std::fmt::DebugList",
    "std::fmt::DebugMap",
    "std::fmt::DebugSet",
    "std::fmt::DebugStruct",
    "std::fmt::DebugTuple",
    "std::fmt::Error",
    "std::fmt::Formatter",
    "std::fs::DirBuilder",
    "std::fs::DirEntry",
    "std::fs::File",
    "std::fs::FileTimes",
    "std::fs::FileType",
    "std::fs::Metadata",
    "std::fs::OpenOptions",
    "std::fs::Permissions",
    "std::fs::ReadDir",
    "std::future::Pending",
    "std::future::PollFn",
    "std::future::Ready",
    "std::hash::BuildHasherDefault",
    "std::hash::SipHasher",
    "std::intrinsics::mir::BasicBlock",
    "std::io::BorrowedBuf",
    "std::io::BorrowedCursor",
    "std::io::BufReader",
    "std::io::BufWriter",
    "std::io::Bytes",
    "std::io::Chain",
    "std::io::Cursor",
    "std::io::Empty",
    "std::io::Error",
    "std::io::IntoInnerError",
    "std::io::IoSlice",
    "std::io::IoSliceMut",
    "std::io::LineWriter",
    "std::io::Lines",
    "std::io::Repeat",
    "std::io::Sink",
    "std::io::Split",
    "std::io::Stderr",
    "std::io::StderrLock",
    "std::io::Stdin",
    "std::io::StdinLock",
    "std::io::Stdout",
    "std::io::StdoutLock",
    "std::io::Take",
    "std::io::WriterPanicked",
    "std::iter::ArrayChunks",
    "std::iter::ByRefSized",
    "std::iter::Chain",
    "std::iter::Cloned",
    "std::iter::Copied",
    "std::iter::Cycle",
    "std::iter::Empty",
    "std::iter::Enumerate",
    "std::iter::Filter",
    "std::iter::FilterMap",
    "std::iter::FlatMap",
    "std::iter::Flatten",
    "std::iter::FromFn",
    "std::iter::Fuse",
    "std::iter::Inspect",
    "std::iter::Intersperse",
    "std::iter::IntersperseWith",
    "std::iter::Map",
    "std::iter::MapWhile",
    "std::iter::Once",
    "std::iter::OnceWith",
    "std::iter::Peekable",
    "std::iter::Repeat",
    "std::iter::RepeatWith",
    "std::iter::Rev",
    "std::iter::Scan",
    "std::iter::Skip",
    "std::iter::SkipWhile",
    "std::iter::StepBy",
    "std::iter::Successors",
    "std::iter::Take",
    "std::iter::TakeWhile",
    "std::iter::Zip",
    "std::marker::PhantomData",
    "std::marker::PhantomPinned",
    "std::mem::Assume",
    "std::mem::Discriminant",
    "std::mem::ManuallyDrop",
    "std::net::AddrParseError",
    "std::net::Incoming",
    "std::net::IntoIncoming",
    "std::net::Ipv4Addr",
    "std::net::Ipv6Addr",
    "std::net::SocketAddrV4",
    "std::net::SocketAddrV6",
    "std::net::TcpListener",
    "std::net::TcpStream",
    "std::net::UdpSocket",
    "std::num::NonZeroI128",
    "std::num::NonZeroI16",
    "std::num::NonZeroI32",
    "std::num::NonZeroI64",
    "std::num::NonZeroI8",
    "std::num::NonZeroIsize",
    "std::num::NonZeroU128",
    "std::num::NonZeroU16",
    "std::num::NonZeroU32",
    "std::num::NonZeroU64",
    "std::num::NonZeroU8",
    "std::num::NonZeroUsize",
    "std::num::ParseFloatError",
    "std::num::ParseIntError",
    "std::num::Saturating",
    "std::num::TryFromIntError",
    "std::num::Wrapping",
    "std::ops::Range",
    "std::ops::RangeFrom",
    "std::ops::RangeFull",
    "std::ops::RangeInclusive",
    "std::ops::RangeTo",
    "std::ops::RangeToInclusive",
    "std::ops::Yeet",
    "std::option::IntoIter",
    "std::option::Iter",
    "std::option::IterMut",
    "std::os::fd::BorrowedFd",
    "std::os::fd::OwnedFd",
    "std::os::linux::process::PidFd",
    "std::os::linux::raw::stat",
    "std::os::unix::net::Incoming",
    "std::os::unix::net::Messages",
    "std::os::unix::net::ScmCredentials",
    "std::os::unix::net::ScmRights",
    "std::os::unix::net::SocketAddr",
    "std::os::unix::net::SocketAncillary",
    "std::os::unix::net::SocketCred",
    "std::os::unix::net::UnixDatagram",
    "std::os::unix::net::UnixListener",
    "std::os::unix::net::UnixStream",
    "std::os::unix::ucred::UCred",
    "std::os::windows::ffi::EncodeWide",
    "std::os::windows::io::BorrowedHandle",
    "std::os::windows::io::BorrowedSocket",
    "std::os::windows::io::HandleOrInvalid",
    "std::os::windows::io::HandleOrNull",
    "std::os::windows::io::InvalidHandleError",
    "std::os::windows::io::NullHandleError",
    "std::os::windows::io::OwnedHandle",
    "std::os::windows::io::OwnedSocket",
    "std::panic::AssertUnwindSafe",
    "std::panic::Location",
    "std::panic::PanicInfo",
    "std::path::Ancestors",
    "std::path::Components",
    "std::path::Display",
    "std::path::Iter",
    "std::path::Path",
    "std::path::PathBuf",
    "std::path::PrefixComponent",
    "std::path::StripPrefixError",
    "std::pin::Pin",
    "std::process::Child",
    "std::process::ChildStderr",
    "std::process::ChildStdin",
    "std::process::ChildStdout",
    "std::process::Command",
    "std::process::CommandArgs",
    "std::process::CommandEnvs",
    "std::process::ExitCode",
    "std::process::ExitStatus",
    "std::process::ExitStatusError",
    "std::process::Output",
    "std::process::Stdio",
    "std::ptr::Alignment",
    "std::ptr::DynMetadata",
    "std::ptr::NonNull",
    "std::rc::Rc",
    "std::rc::Weak",
    "std::result::IntoIter",
    "std::result::Iter",
    "std::result::IterMut",
    "std::simd::LaneCount",
    "std::simd::Mask",
    "std::simd::Simd",
    "std::slice::ArrayChunks",
    "std::slice::ArrayChunksMut",
    "std::slice::ArrayWindows",
    "std::slice::Chunks",
    "std::slice::ChunksExact",
    "std::slice::ChunksExactMut",
    "std::slice::ChunksMut",
    "std::slice::EscapeAscii",
    "std::slice::GroupBy",
    "std::slice::GroupByMut",
    "std::slice::Iter",
    "std::slice::IterMut",
    "std::slice::RChunks",
    "std::slice::RChunksExact",
    "std::slice::RChunksExactMut",
    "std::slice::RChunksMut",
    "std::slice::RSplit",
    "std::slice::RSplitMut",
    "std::slice::RSplitN",
    "std::slice::RSplitNMut",
    "std::slice::Split",
    "std::slice::SplitInclusive",
    "std::slice::SplitInclusiveMut",
    "std::slice::SplitMut",
    "std::slice::SplitN",
    "std::slice::SplitNMut",
    "std::slice::Windows",
    "std::str::Bytes",
    "std::str::CharIndices",
    "std::str::Chars",
    "std::str::EncodeUtf16",
    "std::str::EscapeDebug",
    "std::str::EscapeDefault",
    "std::str::EscapeUnicode",
    "std::str::Lines",
    "std::str::LinesAny",
    "std::str::MatchIndices",
    "std::str::Matches",
    "std::str::ParseBoolError",
    "std::str::RMatchIndices",
    "std::str::RMatches",
    "std::str::RSplit",
    "std::str::RSplitN",
    "std::str::RSplitTerminator",
    "std::str::Split",
    "std::str::SplitAsciiWhitespace",
    "std::str::SplitInclusive",
    "std::str::SplitN",
    "std::str::SplitTerminator",
    "std::str::SplitWhitespace",
    "std::str::Utf8Chunk",
    "std::str::Utf8Chunks",
    "std::str::Utf8Error",
    "std::str::pattern::CharArrayRefSearcher",
    "std::str::pattern::CharArraySearcher",
    "std::str::pattern::CharPredicateSearcher",
    "std::str::pattern::CharSearcher",
    "std::str::pattern::CharSliceSearcher",
    "std::str::pattern::StrSearcher",
    "std::string::Drain",
    "std::string::FromUtf16Error",
    "std::string::FromUtf8Error",
    "std::string::String",
    "std::sync::Arc",
    "std::sync::Barrier",
    "std::sync::BarrierWaitResult",
    "std::sync::Condvar",
    "std::sync::Exclusive",
    "std::sync::LazyLock",
    "std::sync::Mutex",
    "std::sync::MutexGuard",
    "std::sync::Once",
    "std::sync::OnceLock",
    "std::sync::OnceState",
    "std::sync::PoisonError",
    "std::sync::RwLock",
    "std::sync::RwLockReadGuard",
    "std::sync::RwLockWriteGuard",
    "std::sync::WaitTimeoutResult",
    "std::sync::Weak",
    "std::sync::atomic::AtomicBool",
    "std::sync::atomic::AtomicI16",
    "std::sync::atomic::AtomicI32",
    "std::sync::atomic::AtomicI64",
    "std::sync::atomic::AtomicI8",
    "std::sync::atomic::AtomicIsize",
    "std::sync::atomic::AtomicPtr",
    "std::sync::atomic::AtomicU16",
    "std::sync::atomic::AtomicU32",
    "std::sync::atomic::AtomicU64",
    "std::sync::atomic::AtomicU8",
    "std::sync::atomic::AtomicUsize",
    "std::sync::mpsc::IntoIter",
    "std::sync::mpsc::Iter",
    "std::sync::mpsc::Receiver",
    "std::sync::mpsc::RecvError",
    "std::sync::mpsc::SendError",
    "std::sync::mpsc::Sender",
    "std::sync::mpsc::SyncSender",
    "std::sync::mpsc::TryIter",
    "std::task::Context",
    "std::task::RawWaker",
    "std::task::RawWakerVTable",
    "std::task::Ready",
    "std::task::Waker",
    "std::thread::AccessError",
    "std::thread::Builder",
    "std::thread::JoinHandle",
    "std::thread::LocalKey",
    "std::thread::Scope",
    "std::thread::ScopedJoinHandle",
    "std::thread::Thread",
    "std::thread::ThreadId",
    "std::time::Duration",
    "std::time::Instant",
    "std::time::SystemTime",
    "std::time::SystemTimeError",
    "std::time::TryFromFloatSecsError",
    "std::vec::Drain",
    "std::vec::DrainFilter",
    "std::vec::IntoIter",
    "std::vec::Splice",
    "std::vec::Vec",
    "std::backtrace::BacktraceStatus",
    "std::borrow::Cow",
    "std::cmp::Ordering",
    "std::collections::TryReserveErrorKind",
    "std::collections::btree_map::Entry",
    "std::collections::hash_map::Entry",
    "std::collections::hash_map::RawEntryMut",
    "std::convert::Infallible",
    "std::env::VarError",
    "std::ffi::c_void",
    "std::fmt::Alignment",
    "std::io::ErrorKind",
    "std::io::SeekFrom",
    "std::net::IpAddr",
    "std::net::Ipv6MulticastScope",
    "std::net::Shutdown",
    "std::net::SocketAddr",
    "std::num::FpCategory",
    "std::num::IntErrorKind",
    "std::ops::Bound",
    "std::ops::ControlFlow",
    "std::ops::GeneratorState",
    "std::option::Option",
    "std::os::unix::net::AncillaryData",
    "std::os::unix::net::AncillaryError",
    "std::panic::BacktraceStyle",
    "std::path::Component",
    "std::path::Prefix",
    "std::result::Result",
    "std::simd::Which",
    "std::str::pattern::SearchStep",
    "std::sync::TryLockError",
    "std::sync::atomic::Ordering",
    "std::sync::mpsc::RecvTimeoutError",
    "std::sync::mpsc::TryRecvError",
    "std::sync::mpsc::TrySendError",
    "std::task::Poll",
    "std::mem::MaybeUninit",
    "std::alloc::LayoutErr",
    "std::ffi::c_char",
    "std::ffi::c_double",
    "std::ffi::c_float",
    "std::ffi::c_int",
    "std::ffi::c_long",
    "std::ffi::c_longlong",
    "std::ffi::c_schar",
    "std::ffi::c_short",
    "std::ffi::c_uchar",
    "std::ffi::c_uint",
    "std::ffi::c_ulong",
    "std::ffi::c_ulonglong",
    "std::ffi::c_ushort",
    "std::fmt::Result",
    "std::io::RawOsError",
    "std::io::Result",
    "std::os::fd::RawFd",
    "std::os::linux::raw::blkcnt_t",
    "std::os::linux::raw::blksize_t",
    "std::os::linux::raw::dev_t",
    "std::os::linux::raw::ino_t",
    "std::os::linux::raw::mode_t",
    "std::os::linux::raw::nlink_t",
    "std::os::linux::raw::off_t",
    "std::os::linux::raw::pthread_t",
    "std::os::linux::raw::time_t",
    "std::os::raw::c_char",
    "std::os::raw::c_double",
    "std::os::raw::c_float",
    "std::os::raw::c_int",
    "std::os::raw::c_long",
    "std::os::raw::c_longlong",
    "std::os::raw::c_schar",
    "std::os::raw::c_short",
    "std::os::raw::c_uchar",
    "std::os::raw::c_uint",
    "std::os::raw::c_ulong",
    "std::os::raw::c_ulonglong",
    "std::os::raw::c_ushort",
    "std::os::raw::c_void",
    "std::os::unix::raw::blkcnt_t",
    "std::os::unix::raw::blksize_t",
    "std::os::unix::raw::dev_t",
    "std::os::unix::raw::gid_t",
    "std::os::unix::raw::ino_t",
    "std::os::unix::raw::mode_t",
    "std::os::unix::raw::nlink_t",
    "std::os::unix::raw::off_t",
    "std::os::unix::raw::pid_t",
    "std::os::unix::raw::pthread_t",
    "std::os::unix::raw::time_t",
    "std::os::unix::raw::uid_t",
    "std::os::unix::thread::RawPthread",
    "std::os::windows::io::RawHandle",
    "std::os::windows::io::RawSocket",
    "std::os::windows::raw::HANDLE",
    "std::os::windows::raw::SOCKET",
    "std::simd::f32x16",
    "std::simd::f32x2",
    "std::simd::f32x4",
    "std::simd::f32x8",
    "std::simd::f64x2",
    "std::simd::f64x4",
    "std::simd::f64x8",
    "std::simd::i16x16",
    "std::simd::i16x2",
    "std::simd::i16x32",
    "std::simd::i16x4",
    "std::simd::i16x8",
    "std::simd::i32x16",
    "std::simd::i32x2",
    "std::simd::i32x4",
    "std::simd::i32x8",
    "std::simd::i64x2",
    "std::simd::i64x4",
    "std::simd::i64x8",
    "std::simd::i8x16",
    "std::simd::i8x32",
    "std::simd::i8x4",
    "std::simd::i8x64",
    "std::simd::i8x8",
    "std::simd::isizex2",
    "std::simd::isizex4",
    "std::simd::isizex8",
    "std::simd::mask16x16",
    "std::simd::mask16x32",
    "std::simd::mask16x4",
    "std::simd::mask16x8",
    "std::simd::mask32x16",
    "std::simd::mask32x2",
    "std::simd::mask32x4",
    "std::simd::mask32x8",
    "std::simd::mask64x2",
    "std::simd::mask64x4",
    "std::simd::mask64x8",
    "std::simd::mask8x16",
    "std::simd::mask8x32",
    "std::simd::mask8x64",
    "std::simd::mask8x8",
    "std::simd::masksizex2",
    "std::simd::masksizex4",
    "std::simd::masksizex8",
    "std::simd::u16x16",
    "std::simd::u16x2",
    "std::simd::u16x32",
    "std::simd::u16x4",
    "std::simd::u16x8",
    "std::simd::u32x16",
    "std::simd::u32x2",
    "std::simd::u32x4",
    "std::simd::u32x8",
    "std::simd::u64x2",
    "std::simd::u64x4",
    "std::simd::u64x8",
    "std::simd::u8x16",
    "std::simd::u8x32",
    "std::simd::u8x4",
    "std::simd::u8x64",
    "std::simd::u8x8",
    "std::simd::usizex2",
    "std::simd::usizex4",
    "std::simd::usizex8",
    "std::string::ParseError",
    "std::sync::LockResult",
    "std::sync::TryLockResult",
    "std::thread::Result",
];

static STD_TRAITS_RAW: [&str; 182] = [
    "std::alloc::Allocator",
    "std::alloc::GlobalAlloc",
    "std::any::Any",
    "std::any::Provider",
    "std::ascii::AsciiExt",
    "std::async_iter::AsyncIterator",
    "std::borrow::Borrow",
    "std::borrow::BorrowMut",
    "std::borrow::ToOwned",
    "std::clone::Clone",
    "std::cmp::Eq",
    "std::cmp::Ord",
    "std::cmp::PartialEq",
    "std::cmp::PartialOrd",
    "std::convert::AsMut",
    "std::convert::AsRef",
    "std::convert::FloatToInt",
    "std::convert::From",
    "std::convert::Into",
    "std::convert::TryFrom",
    "std::convert::TryInto",
    "std::default::Default",
    "std::error::Error",
    "std::fmt::Binary",
    "std::fmt::Debug",
    "std::fmt::Display",
    "std::fmt::LowerExp",
    "std::fmt::LowerHex",
    "std::fmt::Octal",
    "std::fmt::Pointer",
    "std::fmt::UpperExp",
    "std::fmt::UpperHex",
    "std::fmt::Write",
    "std::future::Future",
    "std::future::IntoFuture",
    "std::hash::BuildHasher",
    "std::hash::Hash",
    "std::hash::Hasher",
    "std::io::BufRead",
    "std::io::IsTerminal",
    "std::io::Read",
    "std::io::Seek",
    "std::io::Write",
    "std::iter::DoubleEndedIterator",
    "std::iter::ExactSizeIterator",
    "std::iter::Extend",
    "std::iter::FromIterator",
    "std::iter::FusedIterator",
    "std::iter::IntoIterator",
    "std::iter::Iterator",
    "std::iter::Product",
    "std::iter::Step",
    "std::iter::Sum",
    "std::iter::TrustedLen",
    "std::iter::TrustedStep",
    "std::marker::Copy",
    "std::marker::Destruct",
    "std::marker::DiscriminantKind",
    "std::marker::PointerLike",
    "std::marker::Send",
    "std::marker::Sized",
    "std::marker::StructuralEq",
    "std::marker::StructuralPartialEq",
    "std::marker::Sync",
    "std::marker::Tuple",
    "std::marker::Unpin",
    "std::marker::Unsize",
    "std::mem::BikeshedIntrinsicFrom",
    "std::net::ToSocketAddrs",
    "std::ops::Add",
    "std::ops::AddAssign",
    "std::ops::BitAnd",
    "std::ops::BitAndAssign",
    "std::ops::BitOr",
    "std::ops::BitOrAssign",
    "std::ops::BitXor",
    "std::ops::BitXorAssign",
    "std::ops::CoerceUnsized",
    "std::ops::Deref",
    "std::ops::DerefMut",
    "std::ops::DispatchFromDyn",
    "std::ops::Div",
    "std::ops::DivAssign",
    "std::ops::Drop",
    "std::ops::Fn",
    "std::ops::FnMut",
    "std::ops::FnOnce",
    "std::ops::FromResidual",
    "std::ops::Generator",
    "std::ops::Index",
    "std::ops::IndexMut",
    "std::ops::Mul",
    "std::ops::MulAssign",
    "std::ops::Neg",
    "std::ops::Not",
    "std::ops::OneSidedRange",
    "std::ops::RangeBounds",
    "std::ops::Rem",
    "std::ops::RemAssign",
    "std::ops::Residual",
    "std::ops::Shl",
    "std::ops::ShlAssign",
    "std::ops::Shr",
    "std::ops::ShrAssign",
    "std::ops::Sub",
    "std::ops::SubAssign",
    "std::ops::Try",
    "std::os::fd::AsFd",
    "std::os::fd::AsRawFd",
    "std::os::fd::FromRawFd",
    "std::os::fd::IntoRawFd",
    "std::os::linux::fs::MetadataExt",
    "std::os::linux::net::SocketAddrExt",
    "std::os::linux::net::TcpStreamExt",
    "std::os::linux::process::ChildExt",
    "std::os::linux::process::CommandExt",
    "std::os::unix::ffi::OsStrExt",
    "std::os::unix::ffi::OsStringExt",
    "std::os::unix::fs::DirBuilderExt",
    "std::os::unix::fs::DirEntryExt",
    "std::os::unix::fs::DirEntryExt2",
    "std::os::unix::fs::FileExt",
    "std::os::unix::fs::FileTypeExt",
    "std::os::unix::fs::MetadataExt",
    "std::os::unix::fs::OpenOptionsExt",
    "std::os::unix::fs::PermissionsExt",
    "std::os::unix::process::CommandExt",
    "std::os::unix::process::ExitStatusExt",
    "std::os::unix::thread::JoinHandleExt",
    "std::os::wasi::ffi::OsStrExt",
    "std::os::wasi::ffi::OsStringExt",
    "std::os::wasi::fs::DirEntryExt",
    "std::os::wasi::fs::FileExt",
    "std::os::wasi::fs::FileTypeExt",
    "std::os::wasi::fs::MetadataExt",
    "std::os::wasi::fs::OpenOptionsExt",
    "std::os::wasi::net::TcpListenerExt",
    "std::os::windows::ffi::OsStrExt",
    "std::os::windows::ffi::OsStringExt",
    "std::os::windows::fs::FileExt",
    "std::os::windows::fs::FileTypeExt",
    "std::os::windows::fs::MetadataExt",
    "std::os::windows::fs::OpenOptionsExt",
    "std::os::windows::io::AsHandle",
    "std::os::windows::io::AsRawHandle",
    "std::os::windows::io::AsRawSocket",
    "std::os::windows::io::AsSocket",
    "std::os::windows::io::FromRawHandle",
    "std::os::windows::io::FromRawSocket",
    "std::os::windows::io::IntoRawHandle",
    "std::os::windows::io::IntoRawSocket",
    "std::os::windows::process::ChildExt",
    "std::os::windows::process::CommandExt",
    "std::os::windows::process::ExitCodeExt",
    "std::os::windows::process::ExitStatusExt",
    "std::panic::RefUnwindSafe",
    "std::panic::UnwindSafe",
    "std::process::Termination",
    "std::ptr::Pointee",
    "std::simd::MaskElement",
    "std::simd::SimdElement",
    "std::simd::SimdFloat",
    "std::simd::SimdInt",
    "std::simd::SimdOrd",
    "std::simd::SimdPartialEq",
    "std::simd::SimdPartialOrd",
    "std::simd::SimdUint",
    "std::simd::StdFloat",
    "std::simd::SupportedLaneCount",
    "std::simd::Swizzle",
    "std::simd::Swizzle2",
    "std::simd::ToBitMask",
    "std::slice::Concat",
    "std::slice::Join",
    "std::slice::SliceIndex",
    "std::str::FromStr",
    "std::str::pattern::DoubleEndedSearcher",
    "std::str::pattern::Pattern",
    "std::str::pattern::ReverseSearcher",
    "std::str::pattern::Searcher",
    "std::string::ToString",
    "std::task::Wake",
];
