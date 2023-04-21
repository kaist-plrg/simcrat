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
    def::Res,
    hir_id::HirId,
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
            UNIT
        };
        Self { params, ret }
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
    TraitObject(Vec<Type>),
    BareFn(Box<FunTySig>),
    Never,
    Impl(Vec<Type>),
    Err,
}

const UNIT: Type = Type::Tup(vec![]);

impl Type {
    fn from_path(path: &Path<'_>, tcx: TyCtxt<'_>) -> Self {
        let Path { segments, .. } = path;
        let segs = segments
            .iter()
            .map(|seg| PathSeg::from_path_segment(seg, tcx))
            .collect();
        Self::Path(segs)
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
                let init = format!(
                    "unsafe {{ std::mem::transmute([0u8; std::mem::size_of::<{}>()]) }}",
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
    if code.contains("async move") {
        return None;
    }

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
                    let item_code = source_map.span_to_snippet(item.span).unwrap();
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
                            if let Type::Path(mut ss) = Type::from_ty(i.self_ty, tcx) {
                                assert_eq!(ss.len(), 1);
                                let seg = ss.pop().unwrap();
                                assert!(seg.args.is_empty());
                                derives.entry(seg.ident).or_default().insert(item_code);
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
                            let signature_ty = FunTySig::from_fn_decl(sig.decl, tcx);
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
                        ItemKind::ExternCrate(_) => {
                            assert_eq!(&item_code, "");
                            continue;
                        }
                        ItemKind::OpaqueTy(_) => continue,
                        i => panic!("{:?}", i),
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
    })
}

pub fn parse_one(code: &str) -> Option<ParsedItem> {
    let mut parsed = parse(code)?;
    assert_eq!(parsed.len(), 1);
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
    let suggestions: Vec<_> = rustc_interface::run_compiler(config, |compiler| {
        let sess = compiler.session();
        rustc_parse::maybe_new_parser_from_source_str(
            &sess.parse_sess,
            FileName::Custom("main.rs".to_string()),
            code.to_string(),
        )
        .ok()?;
        compiler.enter(|queries| {
            queries.global_ctxt().ok()?.enter(|tcx| {
                let mut visitor = ResultVisitor::new(tcx);
                tcx.hir().visit_all_item_likes_in_crate(&mut visitor);
                Some(visitor.suggestions)
            })
        })
    })?;
    Some(rustfix::apply_suggestions(code, &suggestions).unwrap())
}

pub fn resolve_free_types(code: &str, prefix: &str) -> Option<String> {
    let full_code = format!("{}{}", prefix, code);
    let config = make_config(&full_code);
    let suggestions: Vec<_> = rustc_interface::run_compiler(config, |compiler| {
        let sess = compiler.session();
        rustc_parse::maybe_new_parser_from_source_str(
            &sess.parse_sess,
            FileName::Custom("main.rs".to_string()),
            full_code.to_string(),
        )
        .ok()?;
        compiler.enter(|queries| {
            queries.global_ctxt().ok()?.enter(|tcx| {
                let mut visitor = FreeTypeVisitor::new(tcx);
                tcx.hir().visit_all_item_likes_in_crate(&mut visitor);
                let source_map = sess.source_map();
                let types = visitor.undefined_types.into_iter().map(|span| {
                    let s = source_map.span_to_snippet(span).unwrap();
                    let replacement = match s.as_str() {
                        "c_int" | "c_void" | "c_char" | "stat" | "off_t" | "time_t" => {
                            format!("libc::{}", s)
                        }
                        "int" => "i32".to_string(),
                        "Void" => "libc::c_void".to_string(),
                        "CStr" | "ffi::CStr" => "std::ffi::CStr".to_string(),
                        "Path" | "path::Path" => "std::path::Path".to_string(),
                        "Args" | "env::Args" => "std::env::Args".to_string(),
                        "Metadata" | "fs::Metadata" => "std::fs::Metadata".to_string(),
                        _ => {
                            println!("{}", s);
                            "usize".to_string()
                        }
                    };
                    let snippet = span_to_snippet(span, source_map);
                    make_suggestion(snippet, &replacement)
                });
                let traits = visitor.undefined_traits.into_iter().map(|span| {
                    let s = source_map.span_to_snippet(span).unwrap();
                    let replacement = match s.as_str() {
                        "Read" | "io::Read" => "std::io::Read",
                        "Seek" | "io::Seek" => "std::io::Seek",
                        "Write" | "io::Write" => "std::io::Write",
                        "AsRawFd" | "fd::AsRawFd" | "os::fd::AsRawFd" => "std::os::fd::AsRawFd",
                        _ => panic!("{}", s),
                    };
                    let snippet = span_to_snippet(span, source_map);
                    make_suggestion(snippet, replacement)
                });
                Some(types.chain(traits).collect())
            })
        })
    })?;
    let full_code = rustfix::apply_suggestions(&full_code, &suggestions).unwrap();
    Some(full_code.strip_prefix(prefix).unwrap().to_string())
}

pub fn resolve_imports(code: &str, prefix: &str) -> Option<String> {
    let full_code = format!("{}{}", prefix, code);
    let config = make_config(&full_code);
    let suggestions: Vec<_> = rustc_interface::run_compiler(config, |compiler| {
        let sess = compiler.session();
        rustc_parse::maybe_new_parser_from_source_str(
            &sess.parse_sess,
            FileName::Custom("main.rs".to_string()),
            full_code.to_string(),
        )
        .ok()?;
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
    })?;
    let full_code = rustfix::apply_suggestions(&full_code, &suggestions).unwrap();
    Some(full_code.strip_prefix(prefix).unwrap().to_string())
}

pub fn add_trait_uses<'i, I: IntoIterator<Item = &'i String>>(
    code: &str,
    uses: I,
) -> Option<String> {
    let uses: String = uses.into_iter().map(|s| format!("\n    {}", s)).collect();
    let config = make_config(code);
    let suggestions: Vec<_> = rustc_interface::run_compiler(config, |compiler| {
        let sess = compiler.session();
        rustc_parse::maybe_new_parser_from_source_str(
            &sess.parse_sess,
            FileName::Custom("main.rs".to_string()),
            code.to_string(),
        )
        .ok()?;
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
                        let snippet = span_to_snippet(span, sess.source_map());
                        let suggestion = make_suggestion(snippet, &uses);
                        return Some(vec![suggestion]);
                    }
                }
                Some(vec![])
            })
        })
    })?;
    Some(rustfix::apply_suggestions(code, &suggestions).unwrap())
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
}

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

const LENGTH_MSG: &str = "consider specifying the actual array length";
const IMPORT_TRAIT_MSG: &str = "implemented but not in scope; perhaps add a `use` for";
const IMPORT_MSG: &str = "consider importing";
const RET_IMPL_MSG: &str = "as the return type if all return paths have the same type but you want to expose only the trait in the signature";
const SIMILAR_MSG: &str = "a similar name";
const MAX_VAL_MSG: &str = "you may have meant the maximum value of";
const FORMAT_MSG: &str = "use the `Display` trait";
const CHANGE_IMPORT_MSG: &str = "you can use `as` to change the binding name of the import";
const BINDING_MSG: &str = "you might have meant to introduce a new binding";
const RELAX_MSG: &str = "consider relaxing the implicit `Sized` restriction";
const DOTS_MSG: &str = "you might have meant to write `.` instead of `..`";
const LIFETIME_MSG: &str = "consider introducing a named lifetime parameter";
const RESTRICT_MSG: &str = "perhaps you need to restrict type parameter";
const ASSIGN_MSG: &str = "consider assigning a value";
const WRAPPING_MSG: &str = "try wrapping";
const QUESTION_MARK_MSG: &str = "use `?` to coerce and return an appropriate";
const COMMA_MSG: &str = "missing `,`";
const LET_MSG: &str = "maybe you meant to write an assignment here";
const ANNOTATION_MSG: &str = "consider annotating";
const POINTER_MSG: &str = "is a raw pointer; try dereferencing it";
const REMOVE_ARG_MSG: &str = "remove the arguments";
const FIELD_METHOD_MSG: &str = "a method of the same name";
const FIELD_FIELD_MSG: &str = "a field of the same name";
const SEMICOLON_MSG: &str = "consider using a semicolon here";
const RECEIVER_MSG: &str = "consider wrapping the receiver expression with the appropriate type";
const PATH_MSG: &str = "a similar path exists";
const STATIC_LIFETIME_MSG: &str = "consider using the `'static` lifetime";
const LIFETIME_BOUND_MSG: &str = "consider adding an explicit lifetime bound";
const INTO_MSG: &str = "call `Into::into` on this expression to convert";
const RETURN_MSG: &str = "consider returning the local binding";
const BACKTICK_MSG: &str =
    "Unicode character '`' (Grave Accent) looks like ''' (Single Quote), but it is not";
const ESCAPE_MSG: &str = "escape `match` to use it as an identifier";
const UB_MSG: &str = "The rules on what exactly is undefined behavior aren't clear, so this check might be overzealous. Please open an issue on the rustc repository if you believe it should not be considered undefined behavior.";
const TYPE_PARAM_MSG: &str = "you might be missing a type parameter";
const RUST_TYPE_MSG: &str = "perhaps you intended to use this type";
const CONST_MSG: &str = "consider using `const` instead of `let`";
const ADD_EXPR_MSG: &str = "try adding an expression at the end of the block";
const CONVERSION_MSG: &str = "try using a conversion method";
const UNICODE_MSG: &str = "if you meant to use the unicode code point for";
const REMOVE_REF_MSG: &str = "consider removing `&` from the pattern";
const BRACE_MSG: &str = "try adding braces";
const NUMERIC_MSG: &str = "you must specify a concrete type for this numeric value";
const BLOCK_MSG: &str = "try placing this code inside a block";
const METHOD_MSG: &str = "use parentheses to call the method";

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
                let mut visitor = FreeTypeVisitor::new(tcx);
                tcx.hir().visit_all_item_likes_in_crate(&mut visitor);
                if !visitor.undefined_traits.is_empty() {
                    return None;
                }
                let _ = tcx.analysis(());
                Some(())
            })
        })?;
        let mut errors = vec![];
        for diag in inner.lock().unwrap().diagnostics.iter() {
            let message = format!("{}", WithSourceMap::new(source_map, diag));
            if message.contains(UB_MSG) {
                continue;
            }
            let line = diag.span.primary_line(source_map);
            let fix = diag.suggestions.iter().find_map(|sugg| {
                let msg = &sugg.msg;
                let (is_suggestion, is_trait) = match &sugg.applicability {
                    Applicability::HasPlaceholders => return None,
                    Applicability::MachineApplicable => {
                        if msg.contains(RELAX_MSG) {
                            return None;
                        } else {
                            (true, false)
                        }
                    }
                    _ => {
                        if msg.contains(LENGTH_MSG)
                            || msg.contains(SIMILAR_MSG)
                            || msg.contains(RET_IMPL_MSG)
                            || msg.contains(MAX_VAL_MSG)
                            || msg.contains(FORMAT_MSG)
                            || msg.contains(LIFETIME_MSG)
                            || msg.contains(RESTRICT_MSG)
                            || msg.contains(ASSIGN_MSG)
                            || msg.contains(WRAPPING_MSG)
                            || msg.contains(QUESTION_MARK_MSG)
                            || msg.contains(POINTER_MSG)
                            || msg.contains(REMOVE_ARG_MSG)
                            || msg.contains(FIELD_METHOD_MSG)
                            || msg.contains(FIELD_FIELD_MSG)
                            || msg.contains(SEMICOLON_MSG)
                            || msg.contains(RECEIVER_MSG)
                            || msg.contains(PATH_MSG)
                            || msg.contains(STATIC_LIFETIME_MSG)
                            || msg.contains(LIFETIME_BOUND_MSG)
                            || msg.contains(INTO_MSG)
                            || msg.contains(RETURN_MSG)
                            || msg.contains(BACKTICK_MSG)
                            || msg.contains(ESCAPE_MSG)
                            || msg.contains(TYPE_PARAM_MSG)
                            || msg.contains(RUST_TYPE_MSG)
                            || msg.contains(CONST_MSG)
                            || msg.contains(ADD_EXPR_MSG)
                            || msg.contains(CONVERSION_MSG)
                            || msg.contains(UNICODE_MSG)
                            || msg.contains(REMOVE_REF_MSG)
                            || msg.contains(NUMERIC_MSG)
                            || msg.contains(BLOCK_MSG)
                            || msg.contains(METHOD_MSG)
                        {
                            (true, false)
                        } else if msg.contains(IMPORT_MSG) {
                            (false, false)
                        } else if msg.contains(IMPORT_TRAIT_MSG) {
                            (false, true)
                        } else if msg.contains(BINDING_MSG)
                            || msg.contains(DOTS_MSG)
                            || msg.contains(COMMA_MSG)
                            || msg.contains(LET_MSG)
                            || msg.contains(ANNOTATION_MSG)
                            || msg.contains(CHANGE_IMPORT_MSG)
                            || msg.contains(BRACE_MSG)
                        {
                            return None;
                        } else {
                            panic!("{}\n{:?}\n{:?}", code, diag, sugg);
                        }
                    }
                };
                let subst = &sugg.substitutions[0];
                let fix = if is_suggestion {
                    let suggestions = subst
                        .parts
                        .iter()
                        .map(|(span, replacement)| {
                            let snippet = span_to_snippet(span.span(), source_map);
                            make_suggestion(snippet, replacement)
                        })
                        .collect();
                    PossibleFix::Suggestion(suggestions)
                } else {
                    assert_eq!(subst.parts.len(), 1);
                    let s = subst.parts[0].1.trim().to_string();
                    assert!(s.starts_with("use "));
                    assert!(s.ends_with(';'));
                    if is_trait {
                        PossibleFix::UseTrait(s)
                    } else {
                        PossibleFix::Use(s)
                    }
                };
                Some(fix)
            });
            let error = TypeError { message, line, fix };
            errors.push(error);
        }
        Some(TypeCheckingResult { errors })
    })
}

struct FreeTypeVisitor<'tcx> {
    tcx: TyCtxt<'tcx>,
    undefined_traits: Vec<Span>,
    undefined_types: Vec<Span>,
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

impl<'tcx> Visitor<'tcx> for FreeTypeVisitor<'tcx> {
    type NestedFilter = nested_filter::OnlyBodies;

    fn nested_visit_map(&mut self) -> Self::Map {
        self.tcx.hir()
    }

    fn visit_trait_ref(&mut self, t: &'tcx TraitRef<'tcx>) {
        if t.trait_def_id().is_none() {
            self.undefined_traits.push(t.path.span);
        }
        intravisit::walk_trait_ref(self, t)
    }

    fn visit_ty(&mut self, ty: &'tcx Ty<'tcx>) {
        if let TyKind::Path(QPath::Resolved(_, path)) = &ty.kind {
            if path.res == Res::Err {
                self.undefined_types.push(path.span);
            }
        }
        intravisit::walk_ty(self, ty);
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
        }
        intravisit::walk_path(self, path);
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
                    && full_path != "std::result::Result"
                    && full_path != "std::option::Option"
                    && !full_path.starts_with("std::prelude")
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
            resolve_free_types("fn foo(x: Foo) {}", "").unwrap(),
            "fn foo(x: usize) {}"
        );
        assert_eq!(
            resolve_free_types("fn foo() -> Foo {}", "").unwrap(),
            "fn foo() -> usize {}"
        );
        assert_eq!(
            resolve_free_types("fn foo<T: Read>(t: T) {}", "").unwrap(),
            "fn foo<T: std::io::Read>(t: T) {}"
        );
        assert_eq!(
            resolve_free_types("fn foo<T: io::Read>(t: T) {}", "").unwrap(),
            "fn foo<T: std::io::Read>(t: T) {}"
        );
        assert_eq!(
            resolve_free_types("fn foo<T: Seek>(t: T) {}", "").unwrap(),
            "fn foo<T: std::io::Seek>(t: T) {}"
        );
        assert_eq!(
            resolve_free_types("fn foo<T: io::Seek>(t: T) {}", "").unwrap(),
            "fn foo<T: std::io::Seek>(t: T) {}"
        );
        assert_eq!(
            resolve_free_types("fn foo<T: Write>(t: T) {}", "").unwrap(),
            "fn foo<T: std::io::Write>(t: T) {}"
        );
        assert_eq!(
            resolve_free_types("fn foo<T: io::Write>(t: T) {}", "").unwrap(),
            "fn foo<T: std::io::Write>(t: T) {}"
        );
        assert_eq!(
            resolve_free_types("fn foo() -> Foo {}", "type Foo = usize;").unwrap(),
            "fn foo() -> Foo {}"
        );
        assert_eq!(
            resolve_free_types("fn foo<T: Foo>(t: T) {}", "trait Foo {}").unwrap(),
            "fn foo<T: Foo>(t: T) {}"
        );
        assert_eq!(
            resolve_free_types("fn foo() -> path::Path {}", "").unwrap(),
            "fn foo() -> std::path::Path {}"
        );
        assert_eq!(
            resolve_free_types("fn foo() -> Path {}", "use std::path;").unwrap(),
            "fn foo() -> std::path::Path {}"
        );
        assert_eq!(
            resolve_free_types("fn foo() -> std::path::Path {}", "").unwrap(),
            "fn foo() -> std::path::Path {}"
        );
        assert_eq!(
            resolve_free_types("fn foo() -> path::Path {}", "use std::path;").unwrap(),
            "fn foo() -> path::Path {}"
        );
        assert_eq!(
            resolve_free_types("fn foo() -> Path {}", "use std::path::Path;").unwrap(),
            "fn foo() -> Path {}"
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
