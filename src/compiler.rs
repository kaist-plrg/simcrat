use std::{path::PathBuf, process::Command, rc::Rc, cell::RefCell};

use rustc_data_structures::sync::Lrc;
use rustc_errors::{
    Level, emitter::Emitter, registry::Registry, translation::Translate, Diagnostic, FluentBundle, Handler,
};
use rustc_hash::{FxHashMap, FxHashSet};
use rustc_interface::Config;
use rustc_session::{
    config::{CheckCfg, Input, Options},
    parse::ParseSess,
};
use rustc_span::source_map::{FileName, SourceMap};

#[derive(Default, Clone)]
struct Diagnostics(Rc<RefCell<Vec<Diagnostic>>>);

unsafe impl Send for Diagnostics {}
unsafe impl Sync for Diagnostics {}

struct Collector {
    source_map: Lrc<SourceMap>,
    diagnostics: Diagnostics,
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
    fn emit_diagnostic(&mut self, diag: &Diagnostic) {
        self.diagnostics.0.borrow_mut().push(diag.clone());
    }

    fn source_map(&self) -> Option<&Lrc<SourceMap>> {
        Some(&self.source_map)
    }
}

pub fn compile(code: &str) {
    let diags = Diagnostics::default();
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
            ps.span_diagnostic =
                Handler::with_emitter(false, None, Box::new(Collector { source_map, diagnostics }));
        })),
        register_lints: None,
        override_queries: None,
        make_codegen_backend: None,
        registry: Registry::new(&rustc_error_codes::DIAGNOSTICS),
    };
    rustc_interface::run_compiler(config, |compiler| {
        compiler.enter(|queries| {
            queries.global_ctxt().unwrap().enter(|tcx| {
                let _ = tcx.analysis(());
            })
        })
    });
    for diag in diags.0.borrow().iter() {
        if let Level::Error { .. } = diag.level() {
            println!("{:?}", diag.code);
            for (msg, _) in &diag.message {
                println!("{:?}", msg);
            }
        }
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
