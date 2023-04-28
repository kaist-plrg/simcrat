use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
    fs::File,
    io::BufReader,
    path::{Path, PathBuf},
    process::Command,
};

use lang_c::{
    ast::*,
    driver::{self, Config, Parse},
    span::{Node, Span},
    visit::{self, Visit},
};
use serde::{Deserialize, Serialize};

use crate::compiler::{self, FunTySig, Type};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum TypeSort {
    Typedef,
    Struct,
    Union,
    Enum,
}

impl fmt::Display for TypeSort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            TypeSort::Typedef => "type",
            TypeSort::Struct => "struct",
            TypeSort::Union => "union",
            TypeSort::Enum => "enum",
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct CustomType<'ast> {
    pub name: &'ast str,
    pub sort: TypeSort,
}

impl<'ast> CustomType<'ast> {
    pub fn mk_typedef(name: &'ast str) -> Self {
        Self {
            name,
            sort: TypeSort::Typedef,
        }
    }

    pub fn mk_struct(name: &'ast str) -> Self {
        Self {
            name,
            sort: TypeSort::Struct,
        }
    }

    pub fn mk_union(name: &'ast str) -> Self {
        Self {
            name,
            sort: TypeSort::Union,
        }
    }

    pub fn mk_enum(name: &'ast str) -> Self {
        Self {
            name,
            sort: TypeSort::Enum,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeDependency<'ast> {
    pub typ: CustomType<'ast>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Typedef<'ast> {
    pub declaration: &'ast Node<Declaration>,
    pub cnst: bool,
    pub types: Vec<&'ast Node<TypeSpecifier>>,
    pub name: &'ast str,
    pub identifier: &'ast Node<Identifier>,
    pub declarator: &'ast Node<Declarator>,
    pub is_struct_alias: bool,
    pub dependencies: Vec<TypeDependency<'ast>>,
    pub path: &'ast str,
}

#[derive(Debug)]
pub struct Struct<'ast> {
    pub declaration: &'ast Node<Declaration>,
    pub strct: bool,
    pub name: &'ast str,
    pub struct_type: &'ast Node<StructType>,
    pub dependencies: Vec<TypeDependency<'ast>>,
    pub path: &'ast str,
}

#[derive(Debug)]
pub struct Enum<'ast> {
    pub declaration: &'ast Node<Declaration>,
    pub name: &'ast str,
    pub enum_type: &'ast Node<EnumType>,
    pub path: &'ast str,
}

#[derive(Debug)]
pub struct Variable<'ast> {
    pub declaration: &'ast Node<Declaration>,
    pub cnst: bool,
    pub types: Vec<&'ast Node<TypeSpecifier>>,
    pub name: &'ast str,
    pub identifier: &'ast Node<Identifier>,
    pub declarator: &'ast Node<InitDeclarator>,
    pub type_dependencies: Vec<TypeDependency<'ast>>,
    pub dependencies: Vec<&'ast Node<Identifier>>,
    pub path: &'ast str,
}

#[derive(Debug)]
pub struct Function<'ast> {
    pub name: &'ast str,
    pub identifier: &'ast Node<Identifier>,
    pub type_signature: FunTySig,
    pub definition: &'ast Node<FunctionDefinition>,
    pub type_dependencies: Vec<TypeDependency<'ast>>,
    pub dependencies: Vec<&'ast Node<Identifier>>,
    pub callees: Vec<&'ast Node<Identifier>>,
    pub path: &'ast str,
}

#[derive(Debug)]
pub struct Program {
    pub parses: BTreeMap<String, Parse>,
    pub typedef_set: BTreeSet<String>,
    pub struct_set: BTreeSet<String>,
    pub variable_set: BTreeSet<String>,
    pub proto_set: BTreeSet<String>,
    pub function_set: BTreeSet<String>,
}

impl Program {
    pub fn from_compile_commands<P: AsRef<Path>>(path: P) -> Self {
        let file = File::open(path).unwrap();
        let reader = BufReader::new(file);
        let commands: Vec<CompileCommand> = serde_json::from_reader(reader).unwrap();
        let files: Vec<_> = commands
            .into_iter()
            .flat_map(|command| command.preprocess())
            .collect();
        Self::new(files)
    }

    fn new(files: Vec<Preprocessed>) -> Self {
        let mut parses = BTreeMap::new();
        let mut typedef_set = BTreeSet::new();
        let mut struct_set = BTreeSet::new();
        let mut variable_set = BTreeSet::new();
        let mut proto_set = BTreeSet::new();
        let mut function_set = BTreeSet::new();

        let config = Config::with_gcc();

        for file in files {
            let path = file.path.to_str().unwrap();

            let parse = driver::parse_preprocessed(&config, file.code).expect(path);
            parses.insert(path.to_string(), parse);

            let parse = driver::parse_preprocessed(&config, file.long_code).expect(path);
            let lib_spans = find_lib_spans(&parse);
            let is_lib = |span: Span| lib_spans.iter().any(|s| overlap(*s, span));

            for decl in &parse.unit.0 {
                if is_lib(decl.span) {
                    continue;
                }
                match &decl.node {
                    ExternalDeclaration::Declaration(decl) => {
                        if is_typedef(&decl.node) {
                            for n in declarator_names(&decl.node) {
                                typedef_set.insert(n.to_string());
                            }
                        } else {
                            let extrn = is_extern(&decl.node);
                            for decl in &decl.node.declarators {
                                let name = declarator_name(&decl.node.declarator.node);
                                if is_function_proto(&decl.node) {
                                    proto_set.insert(name.to_string());
                                } else if !extrn {
                                    variable_set.insert(name.to_string());
                                }
                            }
                        }
                        for s in &decl.node.specifiers {
                            if let DeclarationSpecifier::TypeSpecifier(t) = &s.node {
                                match &t.node {
                                    TypeSpecifier::Struct(s) => {
                                        if let Some(id) = &s.node.identifier {
                                            if s.node.declarations.is_some() {
                                                struct_set.insert(id.node.name.to_string());
                                            }
                                        }
                                    }
                                    TypeSpecifier::Enum(e) => {
                                        if let Some(id) = &e.node.identifier {
                                            if !e.node.enumerators.is_empty() {
                                                struct_set.insert(id.node.name.to_string());
                                            }
                                        }
                                    }
                                    _ => (),
                                }
                            }
                        }
                    }
                    ExternalDeclaration::FunctionDefinition(func) => {
                        let name = get_identifier(&func.node.declarator.node)
                            .unwrap()
                            .node
                            .name
                            .clone();
                        function_set.insert(name.to_string());
                    }
                    _ => (),
                }
            }
        }

        for f in &function_set {
            variable_set.remove(f);
            proto_set.remove(f);
        }

        Self {
            parses,
            typedef_set,
            struct_set,
            variable_set,
            proto_set,
            function_set,
        }
    }

    pub fn lines(&self, path: &str, span: Span) -> usize {
        self.parses.get(path).unwrap().source[span.start..span.end]
            .lines()
            .count()
    }

    pub fn typedefs(&self) -> BTreeMap<&str, Typedef<'_>> {
        let mut typedefs = BTreeMap::new();
        let mut typedef_set: BTreeSet<_> = self.typedef_set.iter().map(|s| s.as_str()).collect();

        for (path, parse) in &self.parses {
            let path = path.as_str();
            for decl in &parse.unit.0 {
                if let ExternalDeclaration::Declaration(decl) = &decl.node {
                    if !is_typedef(&decl.node) {
                        continue;
                    }
                    let cnst = is_const(&decl.node);
                    for d in &decl.node.declarators {
                        let declarator = &d.node.declarator;
                        let name = declarator_name(&declarator.node);
                        if !typedef_set.remove(name) {
                            continue;
                        }
                        let identifier = get_identifier(&declarator.node).unwrap();
                        let types = type_specifiers(&decl.node);
                        let mut visitor = TypeSpecifierVisitor::default();
                        let mut struct_or_enum = None;
                        for t in &types {
                            let span = t.span;
                            match &t.node {
                                TypeSpecifier::Struct(s) => {
                                    if let Some(x) = &s.node.identifier {
                                        let x = x.node.name.as_str();
                                        if !self.struct_set.contains(x) {
                                            continue;
                                        }
                                        let typ = if matches!(s.node.kind.node, StructKind::Struct)
                                        {
                                            CustomType::mk_struct(x)
                                        } else {
                                            CustomType::mk_union(x)
                                        };
                                        let t = TypeDependency { typ, span };
                                        struct_or_enum = Some(t);
                                        break;
                                    }
                                }
                                TypeSpecifier::Enum(e) => {
                                    if let Some(x) = &e.node.identifier {
                                        let x = x.node.name.as_str();
                                        if !self.struct_set.contains(x) {
                                            continue;
                                        }
                                        let typ = CustomType::mk_enum(x);
                                        let t = TypeDependency { typ, span };
                                        struct_or_enum = Some(t);
                                        break;
                                    }
                                }
                                _ => (),
                            }
                            visitor.visit_type_specifier(&t.node, &t.span);
                        }
                        let is_struct_alias = struct_or_enum.is_some()
                            && matches!(declarator.node.kind.node, DeclaratorKind::Identifier(_))
                            && declarator.node.derived.is_empty();
                        let mut dependencies = if let Some(t) = struct_or_enum {
                            vec![t]
                        } else {
                            visitor.visit_declarator(&declarator.node, &declarator.span);
                            visitor.0
                        };
                        self.refine_type_dependencies(&mut dependencies);
                        let typedef = Typedef {
                            declaration: decl,
                            cnst,
                            types,
                            name,
                            identifier,
                            declarator,
                            is_struct_alias,
                            dependencies,
                            path,
                        };
                        typedefs.insert(name, typedef);
                    }
                }
            }
        }

        assert!(typedef_set.is_empty(), "{:?}", typedef_set);
        typedefs
    }

    pub fn typedef_to_string<S: AsRef<str> + Clone>(
        &self,
        typedef: &Typedef<'_>,
        vec: Vec<(Span, S)>,
    ) -> String {
        let Typedef {
            cnst,
            types,
            declarator,
            path,
            ..
        } = typedef;
        let cnst = if *cnst { "const " } else { "" };
        let types = types
            .iter()
            .map(|t| self.replace(t, path, vec.clone()))
            .collect::<Vec<_>>()
            .join(" ");
        let declarator = self.replace(declarator, path, vec);
        format!("typedef {}{} {};", cnst, types, declarator)
    }

    pub fn typedef_to_struct_string<S: AsRef<str> + Clone>(
        &self,
        typedef: &Typedef<'_>,
        vec: Vec<(Span, S)>,
        new_name: &str,
    ) -> Result<(String, &'static str), Vec<(Span, S)>> {
        let Typedef {
            types,
            declarator,
            path,
            ..
        } = typedef;
        if !matches!(declarator.node.kind.node, DeclaratorKind::Identifier(_)) {
            return Err(vec);
        }
        if types.len() != 1 {
            return Err(vec);
        }
        let typ = types[0];
        match &typ.node {
            TypeSpecifier::Struct(s) => {
                if s.node.identifier.is_some() {
                    return Err(vec);
                }
                let sort = match s.node.kind.node {
                    StructKind::Struct => "struct",
                    StructKind::Union => "union",
                };
                let s = self.replace(s, path, vec);
                Ok((s.replacen(sort, &format!("{} {}", sort, new_name), 1), sort))
            }
            TypeSpecifier::Enum(e) => {
                if e.node.identifier.is_some() {
                    return Err(vec);
                }
                let sort = "enum";
                let s = self.replace(e, path, vec);
                Ok((s.replacen(sort, &format!("{} {}", sort, new_name), 1), sort))
            }
            _ => Err(vec),
        }
    }

    pub fn structs(&self) -> BTreeMap<&str, Struct<'_>> {
        let mut structs = BTreeMap::new();
        let mut struct_set: BTreeSet<_> = self.struct_set.iter().map(|s| s.as_str()).collect();

        for (path, parse) in &self.parses {
            let path = path.as_str();
            for decl in &parse.unit.0 {
                if let ExternalDeclaration::Declaration(decl) = &decl.node {
                    for s in &decl.node.specifiers {
                        if let DeclarationSpecifier::TypeSpecifier(t) = &s.node {
                            if let TypeSpecifier::Struct(s) = &t.node {
                                let name = if let Some(id) = &s.node.identifier {
                                    id.node.name.as_str()
                                } else {
                                    continue;
                                };
                                let declarations = if let Some(declarations) = &s.node.declarations
                                {
                                    declarations
                                } else {
                                    continue;
                                };
                                if !struct_set.remove(name) {
                                    continue;
                                }
                                let mut visitor = TypeSpecifierVisitor::default();
                                for d in declarations {
                                    if let StructDeclaration::Field(f) = &d.node {
                                        visitor.visit_struct_field(&f.node, &f.span);
                                    }
                                }
                                let mut dependencies = visitor.0;
                                self.refine_type_dependencies(&mut dependencies);
                                let strct = matches!(s.node.kind.node, StructKind::Struct);
                                let s = Struct {
                                    declaration: decl,
                                    strct,
                                    name,
                                    struct_type: s,
                                    dependencies,
                                    path,
                                };
                                structs.insert(name, s);
                            }
                        }
                    }
                }
            }
        }

        structs
    }

    pub fn struct_to_string<S: AsRef<str>>(
        &self,
        strct: &Struct<'_>,
        vec: Vec<(Span, S)>,
    ) -> String {
        self.replace(strct.struct_type, strct.path, vec)
    }

    pub fn enums(&self) -> BTreeMap<&str, Enum<'_>> {
        let mut enums = BTreeMap::new();
        let mut enum_set: BTreeSet<_> = self.struct_set.iter().map(|s| s.as_str()).collect();

        for (path, parse) in &self.parses {
            let path = path.as_str();
            for decl in &parse.unit.0 {
                if let ExternalDeclaration::Declaration(decl) = &decl.node {
                    for s in &decl.node.specifiers {
                        if let DeclarationSpecifier::TypeSpecifier(t) = &s.node {
                            if let TypeSpecifier::Enum(e) = &t.node {
                                let name = if let Some(id) = &e.node.identifier {
                                    id.node.name.as_str()
                                } else {
                                    continue;
                                };
                                if !enum_set.remove(name) {
                                    continue;
                                }
                                let e = Enum {
                                    declaration: decl,
                                    name,
                                    enum_type: e,
                                    path,
                                };
                                enums.insert(name, e);
                            }
                        }
                    }
                }
            }
        }

        enums
    }

    pub fn enum_to_string<S: AsRef<str>>(&self, enm: &Enum<'_>, vec: Vec<(Span, S)>) -> String {
        self.replace(enm.enum_type, enm.path, vec)
    }

    pub fn variables(&self) -> (BTreeMap<&str, Variable<'_>>, BTreeMap<&str, Variable<'_>>) {
        let mut variables: BTreeMap<_, Vec<_>> = BTreeMap::new();
        let mut protos = BTreeMap::new();

        for (path, parse) in &self.parses {
            let path = path.as_str();
            for decl in &parse.unit.0 {
                if let ExternalDeclaration::Declaration(decl) = &decl.node {
                    if is_typedef(&decl.node) {
                        continue;
                    }
                    let cnst = is_const(&decl.node);
                    for declarator in &decl.node.declarators {
                        let d = &declarator.node.declarator;
                        let name = declarator_name(&d.node);
                        let identifier = get_identifier(&d.node).unwrap();
                        let types = type_specifiers(&decl.node);
                        let mut visitor = TypeSpecifierVisitor::default();
                        for s in &decl.node.specifiers {
                            visitor.visit_declaration_specifier(&s.node, &s.span);
                        }
                        visitor.visit_init_declarator(&declarator.node, &declarator.span);
                        let mut type_dependencies = visitor.0;
                        self.refine_type_dependencies(&mut type_dependencies);
                        let mut dependencies = if let Some(i) = &declarator.node.initializer {
                            let mut visitor = IdentifierVisitor::default();
                            visitor.visit_initializer(&i.node, &i.span);
                            visitor.0
                        } else {
                            vec![]
                        };
                        self.refine_dependencies(&mut dependencies);
                        let variable = Variable {
                            declaration: decl,
                            cnst,
                            types,
                            name,
                            identifier,
                            declarator,
                            type_dependencies,
                            dependencies,
                            path,
                        };
                        if is_function_proto(&declarator.node) {
                            if self.proto_set.contains(name) {
                                protos.insert(name, variable);
                            }
                        } else if !is_extern(&decl.node) && self.variable_set.contains(name) {
                            variables.entry(name).or_default().push(variable);
                        }
                    }
                }
            }
        }

        let variables = variables
            .into_iter()
            .map(|(name, mut vars)| {
                vars.sort_by_key(|v| {
                    if v.declarator.node.initializer.is_some() {
                        1
                    } else {
                        0
                    }
                });
                (name, vars.pop().unwrap())
            })
            .collect();
        (variables, protos)
    }

    pub fn variable_to_string<S: AsRef<str> + Clone>(
        &self,
        variable: &Variable<'_>,
        vec: Vec<(Span, S)>,
        short: bool,
    ) -> String {
        let Variable {
            cnst,
            types,
            declarator,
            path,
            ..
        } = variable;
        let cnst = if *cnst { "const " } else { "" };
        let types = types
            .iter()
            .map(|t| self.replace(t, path, vec.clone()))
            .collect::<Vec<_>>()
            .join(" ");
        let declarator = if short {
            let d = self.replace(&declarator.node.declarator, path, vec.clone());
            if let Some(i) = &declarator.node.initializer {
                let i = self
                    .replace(i, path, vec)
                    .trim()
                    .strip_prefix('=')
                    .unwrap()
                    .trim()
                    .chars()
                    .next()
                    .unwrap();
                let i = if i == '{' { "{}" } else { "0" };
                format!("{} = {}", d, i)
            } else {
                d
            }
        } else {
            self.replace(declarator, path, vec)
        };
        format!("{}{} {};", cnst, types, declarator)
    }

    pub fn functions(&self) -> BTreeMap<&str, Function<'_>> {
        let mut functions = BTreeMap::new();
        let mut function_set: BTreeSet<_> = self.function_set.iter().map(|s| s.as_str()).collect();

        for (path, parse) in &self.parses {
            let path = path.as_str();
            for decl in &parse.unit.0 {
                if let ExternalDeclaration::FunctionDefinition(func) = &decl.node {
                    let identifier = get_identifier(&func.node.declarator.node).unwrap();
                    let name = identifier.node.name.as_str();
                    if !function_set.remove(name) {
                        continue;
                    }

                    let derived = &func.node.declarator.node.derived;
                    let params = derived
                        .iter()
                        .find_map(|d| match &d.node {
                            DerivedDeclarator::Function(d) => Some(
                                d.node
                                    .parameters
                                    .iter()
                                    .map(|p| {
                                        type_of(&p.node.specifiers, p.node.declarator.as_ref())
                                    })
                                    .collect::<Vec<_>>(),
                            ),
                            DerivedDeclarator::KRFunction(ps) => {
                                assert!(ps.is_empty());
                                Some(vec![])
                            }
                            _ => None,
                        })
                        .expect(
                            &self.parses.get(path).unwrap().source[func.span.start..func.span.end],
                        );
                    let params = if params.len() == 1 && params[0] == compiler::UNIT {
                        vec![]
                    } else {
                        params
                    };
                    let ret = type_of(&func.node.specifiers, Some(&func.node.declarator));
                    let type_signature = FunTySig {
                        params,
                        ret,
                        generic: false,
                    };

                    let mut visitor = TypeSpecifierVisitor::default();
                    visitor.visit_function_definition(&func.node, &func.span);
                    let mut type_dependencies = visitor.0;
                    self.refine_type_dependencies(&mut type_dependencies);

                    let mut dependencies = get_identifiers(&func.node);
                    let local: BTreeSet<_> = get_local_variables(&func.node).into_iter().collect();
                    dependencies.retain(|i| !local.contains(i.node.name.as_str()));
                    self.refine_dependencies(&mut dependencies);

                    let mut callees = get_callees(&func.node);
                    self.refine_callees(&mut callees);

                    let f = Function {
                        identifier,
                        name,
                        type_signature,
                        definition: func,
                        type_dependencies,
                        dependencies,
                        callees,
                        path,
                    };
                    functions.insert(name, f);
                }
            }
        }

        assert!(function_set.is_empty(), "{:?}", function_set);
        functions
    }

    pub fn function_to_string(
        &self,
        function: &Function<'_>,
        mut vec: Vec<(Span, &str)>,
    ) -> String {
        for s in &function.definition.node.specifiers {
            if matches!(
                s.node,
                DeclarationSpecifier::StorageClass(_)
                    | DeclarationSpecifier::Function(_)
                    | DeclarationSpecifier::Alignment(_)
                    | DeclarationSpecifier::Extension(_)
            ) {
                vec.push((s.span, ""));
            }
        }
        self.replace(function.definition, function.path, vec)
    }

    pub fn function_to_signature_string<S: AsRef<str> + Clone>(
        &self,
        function: &Function<'_>,
        vec: Vec<(Span, S)>,
    ) -> String {
        let FunctionDefinition {
            specifiers,
            declarator,
            declarations,
            ..
        } = &function.definition.node;
        let specifiers = specifiers
            .iter()
            .map(|s| self.replace(s, function.path, vec.clone()))
            .collect::<Vec<_>>()
            .join(" ");
        let declarations = declarations
            .iter()
            .map(|d| self.replace(d, function.path, vec.clone()))
            .collect::<Vec<_>>()
            .join(" ");
        let declarator = self.replace(declarator, function.path, vec);
        format!(
            "{} {} {} {{\n    // TODO\n}}",
            specifiers, declarator, declarations
        )
    }

    pub fn span_to_string(&self, path: &str, span: Span) -> &str {
        &self.parses.get(path).unwrap().source[span.start..span.end]
    }

    pub fn replace<T, S: AsRef<str>>(
        &self,
        node: &Node<T>,
        path: &str,
        mut vec: Vec<(Span, S)>,
    ) -> String {
        let parse = self.parses.get(path).unwrap();
        vec.retain(|(span, _)| overlap(*span, node.span));
        vec.sort_by_key(|(span, _)| span.start);
        let mut i = node.span.start;
        let mut res = String::new();
        for (span, s) in vec {
            assert!(i <= span.start);
            res.push_str(&parse.source[i..span.start]);
            res.push_str(s.as_ref());
            i = span.end;
        }
        res.push_str(&parse.source[i..node.span.end]);
        res
    }

    pub fn refine_type_dependencies(&self, deps: &mut Vec<TypeDependency<'_>>) {
        deps.retain(|d| match d.typ.sort {
            TypeSort::Typedef => self.typedef_set.contains(d.typ.name),
            _ => self.struct_set.contains(d.typ.name),
        });
    }

    pub fn refine_dependencies(&self, deps: &mut Vec<&Node<Identifier>>) {
        deps.retain(|d| self.variable_set.contains(&d.node.name));
    }

    pub fn refine_callees(&self, deps: &mut Vec<&Node<Identifier>>) {
        deps.retain(|d| {
            self.proto_set.contains(&d.node.name) || self.function_set.contains(&d.node.name)
        });
    }
}

fn declarator_names(decl: &Declaration) -> Vec<&str> {
    decl.declarators
        .iter()
        .map(|node| declarator_name(&node.node.declarator.node))
        .collect()
}

fn declarator_name(decl: &Declarator) -> &str {
    match &decl.kind.node {
        DeclaratorKind::Identifier(i) => i.node.name.as_str(),
        DeclaratorKind::Declarator(d) => declarator_name(&d.node),
        _ => panic!(),
    }
}

fn type_specifiers(decl: &Declaration) -> Vec<&Node<TypeSpecifier>> {
    decl.specifiers
        .iter()
        .filter_map(|s| {
            if let DeclarationSpecifier::TypeSpecifier(t) = &s.node {
                Some(t)
            } else {
                None
            }
        })
        .collect()
}

fn find_lib_spans(parse: &Parse) -> Vec<Span> {
    let mut lib_start = None;
    let mut lib_spans = vec![];
    let mut pos = 0;
    for line in parse.source.lines() {
        if line.starts_with('#') {
            let path = line
                .split(' ')
                .find_map(|s| s.strip_prefix('"'))
                .expect(line);
            if path.starts_with('/') && !path.starts_with("/usr/include/arpa") {
                lib_start.get_or_insert(pos);
            } else if let Some(start) = lib_start.take() {
                lib_spans.push(Span::span(start, pos));
            }
        }
        pos += line.len() + 1;
    }
    if let Some(start) = lib_start {
        lib_spans.push(Span::span(start, pos - 1));
    }
    lib_spans
}

pub fn overlap(s1: Span, s2: Span) -> bool {
    s1.start < s2.end && s2.start < s1.end
}

fn is_typedef(decl: &Declaration) -> bool {
    decl.specifiers.iter().any(|s| match &s.node {
        DeclarationSpecifier::StorageClass(s) => matches!(s.node, StorageClassSpecifier::Typedef),
        _ => false,
    })
}

fn is_extern(decl: &Declaration) -> bool {
    decl.specifiers.iter().any(|s| match &s.node {
        DeclarationSpecifier::StorageClass(s) => matches!(s.node, StorageClassSpecifier::Extern),
        _ => false,
    })
}

fn is_const(decl: &Declaration) -> bool {
    decl.specifiers.iter().any(|s| match &s.node {
        DeclarationSpecifier::TypeQualifier(t) => matches!(t.node, TypeQualifier::Const),
        _ => false,
    })
}

#[derive(Default)]
struct TypeSpecifierVisitor<'ast>(Vec<TypeDependency<'ast>>);

impl<'ast> Visit<'ast> for TypeSpecifierVisitor<'ast> {
    fn visit_type_specifier(&mut self, type_specifier: &'ast TypeSpecifier, span: &'ast Span) {
        match type_specifier {
            TypeSpecifier::Struct(s) => {
                if let Some(x) = &s.node.identifier {
                    if s.node.declarations.is_none() {
                        let x = x.node.name.as_str();
                        let typ = match &s.node.kind.node {
                            StructKind::Struct => CustomType::mk_struct(x),
                            StructKind::Union => CustomType::mk_union(x),
                        };
                        self.0.push(TypeDependency { typ, span: *span });
                    }
                }
            }
            TypeSpecifier::Enum(e) => {
                if let Some(x) = &e.node.identifier {
                    if e.node.enumerators.is_empty() {
                        let typ = CustomType::mk_enum(x.node.name.as_str());
                        self.0.push(TypeDependency { typ, span: *span });
                    }
                }
            }
            TypeSpecifier::TypedefName(x) => {
                let typ = CustomType::mk_typedef(x.node.name.as_str());
                self.0.push(TypeDependency { typ, span: *span });
            }
            _ => (),
        }
        visit::visit_type_specifier(self, type_specifier, span);
    }
}

#[derive(Default)]
struct CalleeVisitor<'ast>(Vec<&'ast Node<Identifier>>);

impl<'ast> Visit<'ast> for CalleeVisitor<'ast> {
    fn visit_call_expression(&mut self, call_expression: &'ast CallExpression, span: &'ast Span) {
        if let Expression::Identifier(x) = &call_expression.callee.node {
            self.0.push(x);
        }
        visit::visit_call_expression(self, call_expression, span)
    }
}

fn get_callees(function_definition: &FunctionDefinition) -> Vec<&Node<Identifier>> {
    let mut visitor = CalleeVisitor::default();
    let body = &function_definition.statement;
    visitor.visit_statement(&body.node, &body.span);
    visitor.0
}

#[derive(Default)]
struct IdentifierVisitor<'ast>(Vec<&'ast Node<Identifier>>);

impl<'ast> Visit<'ast> for IdentifierVisitor<'ast> {
    fn visit_expression(&mut self, expression: &'ast Expression, span: &'ast Span) {
        match expression {
            Expression::Identifier(x) => self.0.push(x),
            _ => visit::visit_expression(self, expression, span),
        }
    }
}

fn get_identifiers(function_definition: &FunctionDefinition) -> Vec<&Node<Identifier>> {
    let mut visitor = IdentifierVisitor::default();
    let body = &function_definition.statement;
    visitor.visit_statement(&body.node, &body.span);
    visitor.0
}

#[derive(Default)]
struct LocalVariableVisitor<'ast>(Vec<&'ast str>);

impl<'ast> Visit<'ast> for LocalVariableVisitor<'ast> {
    fn visit_declaration(&mut self, declaration: &'ast Declaration, span: &'ast Span) {
        if is_variable_declaration(declaration) {
            for x in variable_names(declaration) {
                self.0.push(x);
            }
        }
        visit::visit_declaration(self, declaration, span)
    }
}

fn get_local_variables(function_definition: &FunctionDefinition) -> Vec<&str> {
    let mut visitor = LocalVariableVisitor::default();
    let body = &function_definition.statement;
    visitor.visit_statement(&body.node, &body.span);
    visitor.0
}

#[derive(Default)]
struct NameVisitor<'ast>(Vec<(&'ast str, Span)>);

impl<'ast> Visit<'ast> for NameVisitor<'ast> {
    fn visit_identifier(&mut self, identifier: &'ast Identifier, span: &'ast Span) {
        self.0.push((identifier.name.as_str(), *span));
    }
}

pub fn find_names(node: &Node<FunctionDefinition>, name: &str) -> Vec<Span> {
    let mut visitor = NameVisitor::default();
    visitor.visit_function_definition(&node.node, &node.span);
    visitor
        .0
        .into_iter()
        .filter_map(|(s, span)| if s == name { Some(span) } else { None })
        .collect()
}

fn get_identifier(decl: &Declarator) -> Option<&Node<Identifier>> {
    match &decl.kind.node {
        DeclaratorKind::Identifier(x) => Some(x),
        DeclaratorKind::Declarator(x) => get_identifier(&x.node),
        _ => None,
    }
}

fn variable_names(decl: &Declaration) -> Vec<&str> {
    decl.declarators
        .iter()
        .filter_map(|d| match &d.node.declarator.node.kind.node {
            DeclaratorKind::Identifier(x) => Some(x.node.name.as_str()),
            _ => None,
        })
        .collect()
}

fn is_variable_declaration(decl: &Declaration) -> bool {
    !decl.specifiers.iter().any(|s| match &s.node {
        DeclarationSpecifier::StorageClass(s) => matches!(
            s.node,
            StorageClassSpecifier::Extern | StorageClassSpecifier::Typedef
        ),
        _ => false,
    }) && !decl.declarators.is_empty()
        && !decl.declarators.iter().any(|d| {
            d.node
                .declarator
                .node
                .derived
                .iter()
                .any(|d| matches!(d.node, DerivedDeclarator::Function(_)))
        })
}

fn is_function_proto(decl: &InitDeclarator) -> bool {
    decl.initializer.is_none()
        && matches!(
            decl.declarator.node.kind.node,
            DeclaratorKind::Identifier(_)
        )
        && decl.declarator.node.derived.iter().any(|d| {
            matches!(
                d.node,
                DerivedDeclarator::Function(_) | DerivedDeclarator::KRFunction(_)
            )
        })
}

fn type_of(
    specifiers: &[Node<DeclarationSpecifier>],
    mut declarator: Option<&Node<Declarator>>,
) -> Type {
    let ty = specifiers
        .iter()
        .find_map(|node| match &node.node {
            DeclarationSpecifier::TypeSpecifier(t) => Some(&t.node),
            _ => None,
        })
        .unwrap();
    let mut ty = match ty {
        TypeSpecifier::Void => compiler::UNIT,
        TypeSpecifier::Char
        | TypeSpecifier::Short
        | TypeSpecifier::Int
        | TypeSpecifier::Long
        | TypeSpecifier::Signed
        | TypeSpecifier::Unsigned => Type::from_name("int".to_string()),
        TypeSpecifier::Float | TypeSpecifier::Double => Type::from_name("float".to_string()),
        TypeSpecifier::Bool => Type::from_name("bool".to_string()),
        TypeSpecifier::Complex => todo!("{:?}", ty),
        TypeSpecifier::Atomic(_) => todo!("{:?}", ty),
        TypeSpecifier::Struct(s) => {
            Type::from_name(s.node.identifier.as_ref().unwrap().node.name.clone())
        }
        TypeSpecifier::Enum(e) => {
            Type::from_name(e.node.identifier.as_ref().unwrap().node.name.clone())
        }
        TypeSpecifier::TypedefName(t) => {
            let name = &t.node.name;
            if name.contains("size_t") || name.contains("int") || name.contains("pid_t") {
                Type::from_name("int".to_string())
            } else {
                Type::from_name(name.clone())
            }
        }
        TypeSpecifier::TypeOf(_) => todo!("{:?}", ty),
        TypeSpecifier::TS18661Float(_) => todo!("{:?}", ty),
    };
    while let Some(decl) = &declarator {
        for d in &decl.node.derived {
            if matches!(d.node, DerivedDeclarator::Pointer(_)) {
                ty = Type::Ptr(Box::new(ty), true);
            }
        }
        for d in &decl.node.derived {
            if matches!(d.node, DerivedDeclarator::Array(_)) {
                ty = Type::Array(Box::new(ty), "_".to_string());
            }
        }
        if let DeclaratorKind::Declarator(inner) = &decl.node.kind.node {
            declarator = Some(inner);
        } else {
            declarator = None;
        }
    }
    ty
}

#[derive(Serialize, Deserialize)]
struct CompileCommand {
    arguments: Vec<String>,
    directory: PathBuf,
    file: PathBuf,
}

impl CompileCommand {
    fn preprocess(&self) -> Option<Preprocessed> {
        if self.arguments[0] != "cc" {
            return None;
        }

        let mut arguments = self.arguments.clone();
        arguments.retain(|x| !x.starts_with("-g") && !x.starts_with("-O"));

        let mut command = Command::new("gcc");
        command.current_dir(&self.directory).arg("-E");
        if let Some(i) = arguments.iter().position(|x| x == "-o") {
            command.args(&arguments[1..i]);
            command.args(&arguments[i + 2..]);
        } else {
            command.args(&arguments[1..]);
        }
        let output = command.output().unwrap();
        assert!(output.status.success());
        let long_code = String::from_utf8(output.stdout).unwrap();

        command.arg("-P");
        let output = command.output().unwrap();
        assert!(output.status.success());
        let code = String::from_utf8(output.stdout).unwrap();

        let mut path = self.directory.clone();
        path.push(&self.file);

        let preprocessed = Preprocessed {
            path,
            code,
            long_code,
        };
        Some(preprocessed)
    }
}

struct Preprocessed {
    path: PathBuf,
    code: String,
    long_code: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(code: &str) -> Program {
        let preprocessed = Preprocessed {
            path: PathBuf::from("test.c"),
            code: code.to_string(),
            long_code: code.to_string(),
        };
        Program::new(vec![preprocessed])
    }

    fn get_signature(code: &str) -> FunTySig {
        let program = parse(code);
        let func = program.functions().into_values().next().unwrap();
        func.type_signature
    }

    #[test]
    fn test_signature() {
        let int = Type::from_name("int".to_string());
        let float = Type::from_name("float".to_string());
        let boolean = Type::from_name("bool".to_string());
        let foo = Type::from_name("foo".to_string());
        let ptr = |ty: &Type| Type::Ptr(Box::new(ty.clone()), true);
        let arr = |ty: &Type| Type::Array(Box::new(ty.clone()), "_".to_string());

        let FunTySig { params, ret, .. } = get_signature("void f() {}");
        assert_eq!(params.len(), 0);
        assert_eq!(ret, compiler::UNIT);

        let FunTySig { params, ret, .. } =
            get_signature("void f(int a, signed b, unsigned c, short d, long e, char f) {}");
        assert_eq!(params.len(), 6);
        assert_eq!(params[0], int);
        assert_eq!(params[1], int);
        assert_eq!(params[2], int);
        assert_eq!(params[3], int);
        assert_eq!(params[4], int);
        assert_eq!(params[5], int);
        assert_eq!(ret, compiler::UNIT);

        let FunTySig { params, ret, .. } = get_signature("void f(float a, double b) {}");
        assert_eq!(params.len(), 2);
        assert_eq!(params[0], float);
        assert_eq!(params[1], float);
        assert_eq!(ret, compiler::UNIT);

        let FunTySig { params, ret, .. } = get_signature("void f(_Bool b) {}");
        assert_eq!(params.len(), 1);
        assert_eq!(params[0], boolean);
        assert_eq!(ret, compiler::UNIT);

        let FunTySig { params, ret, .. } = get_signature("void f(int *a, int **b, int ***c) {}");
        assert_eq!(params.len(), 3);
        assert_eq!(params[0], ptr(&int));
        assert_eq!(params[1], ptr(&ptr(&int)));
        assert_eq!(params[2], ptr(&ptr(&ptr(&int))));
        assert_eq!(ret, compiler::UNIT);

        let FunTySig { params, ret, .. } =
            get_signature("void f(int a[1], int b[1][1], int c[1][1][1]) {}");
        assert_eq!(params.len(), 3);
        assert_eq!(params[0], arr(&int));
        assert_eq!(params[1], arr(&arr(&int)));
        assert_eq!(params[2], arr(&arr(&arr(&int))));
        assert_eq!(ret, compiler::UNIT);

        let FunTySig { params, ret, .. } = get_signature("void f(int *a[1], int (*b)[1]) {}");
        assert_eq!(params.len(), 2);
        assert_eq!(params[0], arr(&ptr(&int)));
        assert_eq!(params[1], ptr(&arr(&int)));
        assert_eq!(ret, compiler::UNIT);

        let FunTySig { params, ret, .. } = get_signature("int f() {}");
        assert_eq!(params.len(), 0);
        assert_eq!(ret, int);

        let FunTySig { params, ret, .. } = get_signature("int *f() {}");
        assert_eq!(params.len(), 0);
        assert_eq!(ret, ptr(&int));

        let FunTySig { params, ret, .. } = get_signature("int **f() {}");
        assert_eq!(params.len(), 0);
        assert_eq!(ret, ptr(&ptr(&int)));

        let FunTySig { params, ret, .. } = get_signature("typedef int foo; foo f() {}");
        assert_eq!(params.len(), 0);
        assert_eq!(ret, foo);

        let FunTySig { params, ret, .. } =
            get_signature("struct foo { int x; }; struct foo f() {}");
        assert_eq!(params.len(), 0);
        assert_eq!(ret, foo);

        let FunTySig { params, ret, .. } = get_signature("union foo { int x; }; union foo f() {}");
        assert_eq!(params.len(), 0);
        assert_eq!(ret, foo);

        let FunTySig { params, ret, .. } = get_signature("enum foo { A }; enum foo f() {}");
        assert_eq!(params.len(), 0);
        assert_eq!(ret, foo);
    }
}
