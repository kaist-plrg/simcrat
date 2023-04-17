use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
    path::Path,
};

use lang_c::{
    ast::*,
    driver::{self, Config, Parse},
    span::{Node, Span},
    visit::{self, Visit},
};

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

#[derive(Debug)]
pub struct TypeDependency<'ast> {
    pub typ: CustomType<'ast>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Typedef<'ast> {
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
    pub strct: bool,
    pub name: &'ast str,
    pub struct_type: &'ast Node<StructType>,
    pub dependencies: Vec<TypeDependency<'ast>>,
    pub path: &'ast str,
}

#[derive(Debug)]
pub struct Variable<'ast> {
    pub extrn: bool,
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
    pub params: usize,
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
    pub function_set: BTreeSet<String>,
}

impl Program {
    pub fn new<P: AsRef<Path>>(paths: &[P]) -> Self {
        let mut config = Config::with_gcc();

        let mut typedef_set = BTreeSet::new();
        let mut struct_set = BTreeSet::new();
        let mut variable_set = BTreeSet::new();
        let mut function_set = BTreeSet::new();

        for path in paths {
            let parse = driver::parse(&config, path).unwrap();
            let lib_spans = find_lib_spans(&parse);
            let is_lib = |span: Span| lib_spans.iter().any(|s| overlap(*s, span));

            for decl in &parse.unit.0 {
                if is_lib(decl.span) {
                    continue;
                }
                match &decl.node {
                    ExternalDeclaration::Declaration(decl) => {
                        let set = if is_typedef(&decl.node) {
                            &mut typedef_set
                        } else {
                            &mut variable_set
                        };
                        for n in declarator_names(&decl.node) {
                            set.insert(n.to_string());
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
        }

        config.cpp_options.push("-P".to_string());

        let mut parses = BTreeMap::new();
        for path in paths {
            let parse = driver::parse(&config, path).unwrap();
            let path = path.as_ref().as_os_str().to_str().unwrap().to_string();
            parses.insert(path, parse);
        }

        Self {
            parses,
            typedef_set,
            struct_set,
            variable_set,
            function_set,
        }
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
                            && matches!(declarator.node.kind.node, DeclaratorKind::Identifier(_));
                        let mut dependencies = if let Some(t) = struct_or_enum {
                            vec![t]
                        } else {
                            visitor.visit_declarator(&declarator.node, &declarator.span);
                            visitor.0
                        };
                        self.refine_type_dependencies(&mut dependencies);
                        let typedef = Typedef {
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
                                if !struct_set.remove(name) {
                                    continue;
                                }
                                let declarations = if let Some(declarations) = &s.node.declarations
                                {
                                    declarations
                                } else {
                                    continue;
                                };
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

    pub fn variables(&self) -> BTreeMap<&str, Variable<'_>> {
        let mut variables: BTreeMap<_, Vec<_>> = BTreeMap::new();

        for (path, parse) in &self.parses {
            let path = path.as_str();
            for decl in &parse.unit.0 {
                if let ExternalDeclaration::Declaration(decl) = &decl.node {
                    if is_typedef(&decl.node) {
                        continue;
                    }
                    let extrn = is_extern(&decl.node);
                    let cnst = is_const(&decl.node);
                    for declarator in &decl.node.declarators {
                        if is_function_proto(&declarator.node) {
                            continue;
                        }
                        let d = &declarator.node.declarator;
                        let name = declarator_name(&d.node);
                        if !self.variable_set.contains(name) {
                            continue;
                        }
                        let identifier = get_identifier(&d.node).unwrap();
                        let types = type_specifiers(&decl.node);
                        let mut visitor = TypeSpecifierVisitor::default();
                        for s in &decl.node.specifiers {
                            visitor.visit_declaration_specifier(&s.node, &s.span);
                        }
                        visitor.visit_declarator(&d.node, &d.span);
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
                            extrn,
                            cnst,
                            types,
                            name,
                            identifier,
                            declarator,
                            type_dependencies,
                            dependencies,
                            path,
                        };
                        variables.entry(name).or_default().push(variable);
                    }
                }
            }
        }

        variables
            .into_iter()
            .map(|(name, mut vars)| {
                vars.sort_by_key(|v| {
                    if v.declarator.node.initializer.is_some() {
                        2
                    } else if v.extrn {
                        0
                    } else {
                        1
                    }
                });
                (name, vars.pop().unwrap())
            })
            .collect()
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
                            DerivedDeclarator::Function(d) => Some(d.node.parameters.len()),
                            DerivedDeclarator::KRFunction(is) => Some(is.len()),
                            _ => None,
                        })
                        .unwrap();

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
                        params,
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

        functions
    }

    pub fn function_to_string<S: AsRef<str>>(
        &self,
        function: &Function<'_>,
        vec: Vec<(Span, S)>,
    ) -> String {
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
        format!("{} {} {} {{}}", specifiers, declarator, declarations)
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
        deps.retain(|d| self.function_set.contains(&d.node.name));
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
    for line in parse.source.split('\n') {
        if line.starts_with('#') {
            let path = line.split(' ').find(|s| s.starts_with('"')).unwrap();
            if path.chars().nth(1).unwrap() == '/' {
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

fn overlap(s1: Span, s2: Span) -> bool {
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
