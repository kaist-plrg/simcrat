use std::{
    path::PathBuf,
    process::Command,
    sync::{Arc, Mutex},
};

use rustc_data_structures::sync::Lrc;
use rustc_errors::{
    emitter::Emitter, registry::Registry, translation::Translate, Applicability, DiagnosticId,
    DiagnosticMessage, FluentBundle, Handler, Level, SpanLabel, SubstitutionPart,
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
    source_map::{FileName, SourceMap},
    Span, SpanData,
};
use rustfix::{LinePosition, LineRange, Replacement, Snippet, Solution, Suggestion};

#[derive(Debug, Clone)]
struct Substitution {
    parts: Vec<(SpanData, String)>,
}

impl From<&rustc_errors::Substitution> for Substitution {
    fn from(subst: &rustc_errors::Substitution) -> Self {
        let parts = subst
            .parts
            .iter()
            .map(|SubstitutionPart { span, snippet }| (span.data(), snippet.clone()))
            .collect();
        Self { parts }
    }
}

#[derive(Debug, Clone)]
struct CodeSuggestion {
    substitutions: Vec<Substitution>,
    msg: String,
    applicable: bool,
}

impl From<&rustc_errors::CodeSuggestion> for CodeSuggestion {
    fn from(suggestion: &rustc_errors::CodeSuggestion) -> Self {
        let substitutions = suggestion.substitutions.iter().map(|s| s.into()).collect();
        let msg = message_to_string(suggestion.msg.clone());
        let applicable = matches!(suggestion.applicability, Applicability::MachineApplicable);
        Self {
            substitutions,
            msg,
            applicable,
        }
    }
}

#[derive(Debug, Clone)]
struct Diagnostic {
    message: Vec<String>,
    code: Option<String>,
    span_labels: Vec<(SpanData, bool, Option<String>)>,
    suggestions: Vec<CodeSuggestion>,
}

fn message_to_string(msg: DiagnosticMessage) -> String {
    match msg {
        DiagnosticMessage::Str(s) => s,
        msg => panic!("{:?}", msg),
    }
}

fn code_to_string(code: DiagnosticId) -> String {
    match code {
        DiagnosticId::Error(s) => s,
        code => panic!("{:?}", code),
    }
}

impl From<&rustc_errors::Diagnostic> for Diagnostic {
    fn from(diag: &rustc_errors::Diagnostic) -> Self {
        let message = diag
            .message
            .iter()
            .map(|(m, _)| message_to_string(m.clone()))
            .collect();
        let code = diag.code.clone().map(code_to_string);
        let span_labels = diag
            .span
            .span_labels()
            .drain(..)
            .map(
                |SpanLabel {
                     span,
                     is_primary,
                     label,
                 }| { (span.data(), is_primary, label.map(message_to_string)) },
            )
            .collect();
        let empty = vec![];
        let suggestions = diag
            .suggestions
            .as_ref()
            .unwrap_or(&empty)
            .iter()
            .map(|s| s.into())
            .collect();
        Self {
            message,
            code,
            span_labels,
            suggestions,
        }
    }
}

struct CollectingEmitter {
    source_map: Lrc<SourceMap>,
    diagnostics: Arc<Mutex<Vec<Diagnostic>>>,
}

impl Translate for CollectingEmitter {
    fn fluent_bundle(&self) -> Option<&Lrc<FluentBundle>> {
        None
    }

    fn fallback_fluent_bundle(&self) -> &FluentBundle {
        panic!()
    }
}

impl Emitter for CollectingEmitter {
    fn emit_diagnostic(&mut self, diag: &rustc_errors::Diagnostic) {
        // println!("{:?}", diag);
        if matches!(diag.level(), Level::Error { .. }) {
            self.diagnostics.lock().unwrap().push(diag.into());
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
    Config {
        opts: Options {
            maybe_sysroot: Some(PathBuf::from(sys_root())),
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
        registry: Registry::new(&rustc_error_codes::DIAGNOSTICS),
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunTySig {
    pub params: Vec<Type>,
    pub ret: Type,
}

impl std::fmt::Display for FunTySig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt_list(f, self.params.iter(), "(", ", ", ")")?;
        write!(f, " -> {}", self.ret)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
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
    fn from_path(path: &Path<'_>) -> Self {
        let Path { segments, .. } = path;
        let PathSegment { ident, args, .. } = segments.iter().last().unwrap();
        let id = ident.name.to_ident_string();
        let id = match id.as_str() {
            "i8" | "i16" | "i32" | "i64" | "i128" | "isize" | "u8" | "u16" | "u32" | "u64"
            | "u128" | "usize" => "int".to_string(),
            "f32" | "f64" => "float".to_string(),
            _ => id,
        };
        let mut args = if let Some(args) = args {
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
        if id == "Result" && args.len() == 2 {
            args.pop();
            args.push(Self::Tup(vec![]));
        }
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

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Slice(t) => write!(f, "[{}]", t),
            Self::Array(t) => write!(f, "[{}; _]", t),
            Self::Ptr(t, m) => write!(f, "*{} {}", if *m { "mut" } else { "const" }, t),
            Self::Ref(t, m) => write!(f, "&{}{}", if *m { "mut " } else { "" }, t),
            Self::Tup(ts) => fmt_list(f, ts.iter(), "(", ", ", ")"),
            Self::Path(x, ts) => {
                write!(f, "{}", x)?;
                if ts.len() != 0 {
                    fmt_list(f, ts.iter(), "<", ", ", ">")?;
                }
                Ok(())
            }
            Self::TraitObject(ts) => fmt_list(f, ts.iter(), "dyn ", " + ", ""),
        }
    }
}

fn fmt_list<T: std::fmt::Display, I: Iterator<Item = T>>(
    f: &mut std::fmt::Formatter<'_>,
    iter: I,
    begin: &str,
    sep: &str,
    end: &str,
) -> std::fmt::Result {
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

pub fn parse_signature(code: &str) -> (FunTySig, String) {
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
                let params: Vec<_> = decl.inputs.iter().map(|ty| Type::from_ty(ty)).collect();
                let mut spans: Vec<_> = decl.inputs.iter().flat_map(result_targ_spans).collect();
                let (ret, mut ret_spans) = if let FnRetTy::Return(ty) = decl.output {
                    (Type::from_ty(ty), result_targ_spans(ty))
                } else {
                    (Type::Tup(vec![]), vec![])
                };
                spans.append(&mut ret_spans);
                let suggestions: Vec<_> = spans
                    .iter()
                    .map(|span| make_suggestion(*span, "()", source_map))
                    .collect();
                let code = rustfix::apply_suggestions(code, &suggestions).unwrap();
                (FunTySig { params, ret }, code)
            })
        })
    })
}

pub fn type_check(code: &str) -> (Vec<(String, String)>, Vec<Suggestion>) {
    let diags = Arc::new(Mutex::new(vec![]));
    let diagnostics = diags.clone();
    let mut config = make_config(code);
    config.parse_sess_created = Some(Box::new(|ps: &mut ParseSess| {
        let source_map = ps.clone_source_map();
        ps.span_diagnostic = Handler::with_emitter(
            false,
            None,
            Box::new(CollectingEmitter {
                source_map,
                diagnostics,
            }),
        );
    }));
    rustc_interface::run_compiler(config, |compiler| {
        let sess = compiler.session();
        let source_map = sess.source_map();
        let parsed = rustc_parse::maybe_new_parser_from_source_str(
            &sess.parse_sess,
            FileName::Custom("main.rs".to_string()),
            code.to_string(),
        );
        if let Err(diagnostics) = parsed {
            for mut diag in diagnostics {
                sess.parse_sess.span_diagnostic.emit_diagnostic(&mut diag);
            }
        } else {
            compiler.enter(|queries| {
                queries.global_ctxt().unwrap().enter(|tcx| {
                    let _ = tcx.analysis(());
                })
            });
        }
        let mut errors = vec![];
        let mut suggestions = vec![];
        for diag in diags.lock().unwrap().iter() {
            assert!(diag.suggestions.len() <= 1);
            if let Some(suggestion) = diag.suggestions.get(0) {
                assert!(suggestion.applicable);
                assert_eq!(suggestion.substitutions.len(), 1);
                let subst = &suggestion.substitutions[0];
                for (span, replacement) in &subst.parts {
                    let suggestion = make_suggestion(span.span(), replacement, source_map);
                    suggestions.push(suggestion);
                }
            } else {
                let mut error_msgs = "error".to_string();
                let error_code = if let Some(code) = &diag.code {
                    format!("[{}]", code)
                } else {
                    "".to_string()
                };
                error_msgs.push_str(&error_code);
                error_msgs.push_str(&format!(": {}", diag.message.join("\n")));
                let mut labels = diag.span_labels.clone();
                labels.sort_by_key(|(span, _, _)| *span);
                for (span, _, msg) in labels {
                    let span = span.span();
                    let code = source_map.span_to_snippet(span).unwrap();
                    let line_span = source_map.span_extend_to_line(span);
                    let line_code = source_map.span_to_snippet(line_span).unwrap();
                    if let Some(msg) = msg {
                        error_msgs.push_str(&format!("\n{} -- {}: {}", line_code, code, msg));
                    }
                }

                let lo = diag
                    .span_labels
                    .iter()
                    .map(|(span, _, _)| span.lo)
                    .min()
                    .unwrap();
                let hi = diag
                    .span_labels
                    .iter()
                    .map(|(span, _, _)| span.hi)
                    .max()
                    .unwrap();
                let span = diag.span_labels[0].0.span().with_lo(lo).with_hi(hi);
                let span = source_map.span_extend_to_line(span);
                let code = source_map.span_to_snippet(span).unwrap();
                errors.push((error_msgs, code));
            }
        }
        (errors, suggestions)
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

fn make_suggestion(span: Span, replacement: &str, source_map: &SourceMap) -> Suggestion {
    let snippet = span_to_snippet(span, source_map);
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
    use super::is_proper_semipredicate;

    #[test]
    fn test_semipredicate() {
        assert_eq!(is_proper_semipredicate("fn f() { Some(0) }", true), false);
        assert_eq!(is_proper_semipredicate("fn f() { None }", true), false);
        assert_eq!(is_proper_semipredicate("fn f() { bar()?; }", true), false);
        assert_eq!(is_proper_semipredicate("fn f() { bar()?; None }", true), false);
        assert_eq!(is_proper_semipredicate("fn f() { match x { Some(_) => {} None => {} }; None }", true), false);
        assert_eq!(is_proper_semipredicate("fn f() { bar() }", true), true);
        assert_eq!(is_proper_semipredicate("fn f() { None; Some(0) }", true), true);
        assert_eq!(is_proper_semipredicate("fn f() { bar()?; Some(0) }", true), true);

        assert_eq!(is_proper_semipredicate("fn f() { Ok(0) }", false), false);
        assert_eq!(is_proper_semipredicate("fn f() { Err(()) }", false), false);
        assert_eq!(is_proper_semipredicate("fn f() { bar()?; }", false), false);
        assert_eq!(is_proper_semipredicate("fn f() { bar()?; Err(()) }", false), false);
        assert_eq!(is_proper_semipredicate("fn f() { match x { Ok(_) => {} Err(_) => {} }; Err(()) }", false), false);
        assert_eq!(is_proper_semipredicate("fn f() { bar() }", false), true);
        assert_eq!(is_proper_semipredicate("fn f() { Err(()); Ok(0) }", false), true);
        assert_eq!(is_proper_semipredicate("fn f() { bar()?; Ok(0) }", false), true);
    }
}
