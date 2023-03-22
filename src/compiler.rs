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
use rustc_interface::Config;
use rustc_session::{
    config::{CheckCfg, Input, Options},
    parse::ParseSess,
};
use rustc_span::{
    source_map::{FileName, SourceMap},
    SpanData,
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
        _ => panic!(),
    }
}

fn code_to_string(code: DiagnosticId) -> String {
    match code {
        DiagnosticId::Error(s) => s,
        _ => panic!(),
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

struct Collector {
    source_map: Lrc<SourceMap>,
    diagnostics: Arc<Mutex<Vec<Diagnostic>>>,
}

impl Translate for Collector {
    fn fluent_bundle(&self) -> Option<&Lrc<FluentBundle>> {
        None
    }

    fn fallback_fluent_bundle(&self) -> &FluentBundle {
        panic!()
    }
}

impl Emitter for Collector {
    fn emit_diagnostic(&mut self, diag: &rustc_errors::Diagnostic) {
        if matches!(diag.level(), Level::Error { .. }) {
            // println!("{:?}", diag);
            self.diagnostics.lock().unwrap().push(diag.into());
        }
    }

    fn source_map(&self) -> Option<&Lrc<SourceMap>> {
        Some(&self.source_map)
    }
}

pub fn compile(code: &str) {
    let diags = Arc::new(Mutex::new(vec![]));
    let diagnostics = diags.clone();
    let config = Config {
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
        // parse_sess_created: None,
        parse_sess_created: Some(Box::new(|ps: &mut ParseSess| {
            let source_map = ps.clone_source_map();
            ps.span_diagnostic = Handler::with_emitter(
                false,
                None,
                Box::new(Collector {
                    source_map,
                    diagnostics,
                }),
            );
        })),
        register_lints: None,
        override_queries: None,
        make_codegen_backend: None,
        registry: Registry::new(&rustc_error_codes::DIAGNOSTICS),
    };
    let suggestions = rustc_interface::run_compiler(config, |compiler| {
        compiler.enter(|queries| {
            queries.global_ctxt().unwrap().enter(|tcx| {
                let mut suggestions = vec![];
                let source_map = compiler.session().source_map();
                let _ = tcx.analysis(());
                for diag in diags.lock().unwrap().iter() {
                    println!("{:?}", diag.code);
                    println!("{:?}", diag.message);
                    for (span, primary, msg) in &diag.span_labels {
                        println!("{:?} {} {:?}", span, primary, msg);
                    }
                    assert!(diag.suggestions.len() <= 1);
                    if let Some(suggestion) = diag.suggestions.get(0) {
                        assert!(suggestion.applicable);
                        assert_eq!(suggestion.substitutions.len(), 1);
                        let subst = &suggestion.substitutions[0];
                        for (span, replacement) in &subst.parts {
                            let snippet = span_data_to_snippet(span, source_map);
                            let replacement = Replacement {
                                snippet: snippet.clone(),
                                replacement: replacement.clone(),
                            };
                            let solution = Solution {
                                message: "".into(),
                                replacements: vec![replacement],
                            };
                            let suggestion = Suggestion {
                                message: "".into(),
                                snippets: vec![snippet],
                                solutions: vec![solution],
                            };
                            suggestions.push(suggestion);
                        }
                    }
                }
                suggestions
            })
        })
    });
    println!("{:?}", suggestions);
    if suggestions.len() > 0 {
        println!(
            "{}",
            rustfix::apply_suggestions(code, &suggestions).unwrap()
        );
    }
}

fn span_data_to_snippet(span: &SpanData, source_map: &SourceMap) -> Snippet {
    let span = span.span();
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
