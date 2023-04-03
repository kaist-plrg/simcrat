use std::{
    collections::{BTreeMap, BTreeSet},
    path::Path,
};

use lang_c::{
    ast::*,
    driver::{self, Config, Parse},
    span::{Node, Span},
    visit::{self, Visit},
};

#[derive(Debug)]
pub enum CustomType<'ast> {
    TypedefName(&'ast str),
    Struct(&'ast str),
    Union(&'ast str),
    Enum(&'ast str),
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
    pub declarator: &'ast Node<Declarator>,
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
    pub cnst: bool,
    pub types: Vec<&'ast Node<TypeSpecifier>>,
    pub name: &'ast str,
    pub declarator: &'ast Node<InitDeclarator>,
    pub type_dependencies: Vec<TypeDependency<'ast>>,
    pub dependencies: Vec<&'ast Node<Identifier>>,
    pub path: &'ast str,
}

#[derive(Debug)]
pub struct Function<'ast> {
    pub name: &'ast str,
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
                        let name = function_name(&func.node);
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
                                            CustomType::Struct(x)
                                        } else {
                                            CustomType::Union(x)
                                        };
                                        let t = TypeDependency { typ, span };
                                        struct_or_enum = Some(t);
                                        break;
                                    }
                                }
                                TypeSpecifier::Enum(e) => {
                                    if let Some(x) = &e.node.identifier {
                                        let x = x.node.name.as_str();
                                        let typ = CustomType::Enum(x);
                                        let t = TypeDependency { typ, span };
                                        struct_or_enum = Some(t);
                                        break;
                                    }
                                }
                                _ => (),
                            }
                            visitor.visit_type_specifier(&t.node, &t.span);
                        }
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
                            declarator,
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

    pub fn typedef_to_string(&self, typedef: &Typedef<'_>) -> String {
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
            .map(|t| self.span_to_string(path, t.span))
            .collect::<Vec<_>>()
            .join(" ");
        let declarator = self.span_to_string(path, declarator.span);
        format!("typedef {}{} {};", cnst, types, declarator)
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

    pub fn struct_to_string(&self, strct: &Struct<'_>) -> &str {
        let Struct {
            struct_type, path, ..
        } = strct;
        self.span_to_string(path, struct_type.span)
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
                    let cnst = is_const(&decl.node);
                    for declarator in &decl.node.declarators {
                        let d = &declarator.node.declarator;
                        let name = declarator_name(&d.node);
                        if !self.variable_set.contains(name) {
                            continue;
                        }
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
                            cnst,
                            types,
                            name,
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
                        1
                    } else {
                        0
                    }
                });
                (name, vars.pop().unwrap())
            })
            .collect()
    }

    pub fn variable_to_string(&self, variable: &Variable<'_>) -> String {
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
            .map(|t| self.span_to_string(path, t.span))
            .collect::<Vec<_>>()
            .join(" ");
        let declarator = self.span_to_string(path, declarator.span);
        format!("{}{} {};", cnst, types, declarator)
    }

    pub fn functions(&self) -> BTreeMap<&str, Function<'_>> {
        let mut functions = BTreeMap::new();
        let mut function_set: BTreeSet<_> = self.function_set.iter().map(|s| s.as_str()).collect();

        for (path, parse) in &self.parses {
            let path = path.as_str();
            for decl in &parse.unit.0 {
                if let ExternalDeclaration::FunctionDefinition(func) = &decl.node {
                    let name = function_name(&func.node);
                    if !function_set.remove(name) {
                        continue;
                    }

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
                        name,
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
        let Function {
            definition, path, ..
        } = function;
        self.replace(definition, path, vec)
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
        deps.retain(|d| match d.typ {
            CustomType::TypedefName(s) => self.typedef_set.contains(s),
            CustomType::Enum(s) | CustomType::Struct(s) | CustomType::Union(s) => {
                self.struct_set.contains(s)
            }
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
                            StructKind::Struct => CustomType::Struct(x),
                            StructKind::Union => CustomType::Union(x),
                        };
                        self.0.push(TypeDependency { typ, span: *span });
                    }
                }
            }
            TypeSpecifier::Enum(e) => {
                if let Some(x) = &e.node.identifier {
                    if e.node.enumerators.is_empty() {
                        let typ = CustomType::Enum(x.node.name.as_str());
                        self.0.push(TypeDependency { typ, span: *span });
                    }
                }
            }
            TypeSpecifier::TypedefName(x) => {
                let typ = CustomType::TypedefName(x.node.name.as_str());
                self.0.push(TypeDependency { typ, span: *span });
            }
            _ => (),
        }
        visit::visit_type_specifier(self, type_specifier, span);
    }
}

#[derive(Default)]
struct FunctionDefinitionVisitor<'ast>(Vec<&'ast Node<FunctionDefinition>>);

impl<'ast> Visit<'ast> for FunctionDefinitionVisitor<'ast> {
    fn visit_external_declaration(
        &mut self,
        external_declaration: &'ast ExternalDeclaration,
        _span: &'ast Span,
    ) {
        if let ExternalDeclaration::FunctionDefinition(f) = external_declaration {
            self.0.push(f);
        }
    }
}

pub fn get_function_definitions(parsed: &Parse) -> Vec<&Node<FunctionDefinition>> {
    let mut visitor = FunctionDefinitionVisitor::default();
    visitor.visit_translation_unit(&parsed.unit);
    visitor.0
}

#[derive(Default)]
struct GlobalVariableVisitor<'ast>(Vec<&'ast Node<Declaration>>);

impl<'ast> Visit<'ast> for GlobalVariableVisitor<'ast> {
    fn visit_external_declaration(
        &mut self,
        external_declaration: &'ast ExternalDeclaration,
        _span: &'ast Span,
    ) {
        if let ExternalDeclaration::Declaration(d) = external_declaration {
            if is_variable_declaration(&d.node) {
                self.0.push(d);
            }
        }
    }
}

pub fn get_variable_declarations(parsed: &Parse) -> Vec<&Node<Declaration>> {
    let mut visitor = GlobalVariableVisitor::default();
    visitor.visit_translation_unit(&parsed.unit);
    visitor.0
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

pub fn get_callees(function_definition: &FunctionDefinition) -> Vec<&Node<Identifier>> {
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

pub fn get_identifiers(function_definition: &FunctionDefinition) -> Vec<&Node<Identifier>> {
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

pub fn get_local_variables(function_definition: &FunctionDefinition) -> Vec<&str> {
    let mut visitor = LocalVariableVisitor::default();
    let body = &function_definition.statement;
    visitor.visit_statement(&body.node, &body.span);
    visitor.0
}

pub fn function_name(fun_def: &FunctionDefinition) -> &str {
    if let DeclaratorKind::Identifier(x) = &fun_def.declarator.node.kind.node {
        &x.node.name
    } else {
        panic!()
    }
}

pub fn function_name_span(fun_def: &FunctionDefinition) -> Span {
    if let DeclaratorKind::Identifier(x) = &fun_def.declarator.node.kind.node {
        x.span
    } else {
        panic!()
    }
}

pub fn variable_names(decl: &Declaration) -> Vec<&str> {
    decl.declarators
        .iter()
        .filter_map(|d| match &d.node.declarator.node.kind.node {
            DeclaratorKind::Identifier(x) => Some(x.node.name.as_str()),
            _ => None,
        })
        .collect()
}

pub fn node_to_string<'a, T>(node: &Node<T>, parse: &'a Parse) -> &'a str {
    &parse.source[node.span.start..node.span.end]
}

pub fn replace<T, S: AsRef<str>>(node: &Node<T>, parse: &Parse, mut vec: Vec<(Span, S)>) -> String {
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

pub fn get_line<T>(node: &Node<T>, parse: &Parse, span: Span) -> usize {
    assert!(node.span.start <= span.start && span.end <= node.span.end);
    parse.source[node.span.start..span.start]
        .chars()
        .filter(|c| *c == '\n')
        .count()
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

#[cfg(test)]
mod tests {
    use lang_c::driver;

    use super::*;

    #[test]
    fn test_get_function_definitions() {
        let config = Config::with_gcc();
        let code = "int main() {} int foo() {}";
        let parsed = driver::parse_preprocessed(&config, code.to_string()).unwrap();
        let defs = get_function_definitions(&parsed);
        assert_eq!(defs.len(), 2);
        assert_eq!(function_name(&defs[0].node), "main");
        assert_eq!(function_name(&defs[1].node), "foo");
    }

    #[test]
    fn test_get_variable_declarations() {
        let config = Config::with_gcc();
        let code = "int x = 0; int y = 1, z = 2; int main() {}";
        let parsed = driver::parse_preprocessed(&config, code.to_string()).unwrap();
        let defs = get_variable_declarations(&parsed);
        assert_eq!(defs.len(), 2);
        assert_eq!(variable_names(&defs[0].node), vec!["x"]);
        assert_eq!(variable_names(&defs[1].node), vec!["y", "z"]);
    }

    #[test]
    fn test_get_callees() {
        let config = Config::with_gcc();
        let code = "int main() { foo(); } int foo() { bar(); }";
        let parsed = driver::parse_preprocessed(&config, code.to_string()).unwrap();
        let defs = get_function_definitions(&parsed);
        assert_eq!(defs.len(), 2);

        let callees = get_callees(&defs[0].node);
        assert_eq!(callees.len(), 1);
        assert_eq!(callees[0].node.name, "foo");

        let callees = get_callees(&defs[1].node);
        assert_eq!(callees.len(), 1);
        assert_eq!(callees[0].node.name, "bar");
    }

    #[test]
    fn test_get_identifiers() {
        let config = Config::with_gcc();
        let code = "int main() { x; y; } int foo() { z; w; }";
        let parsed = driver::parse_preprocessed(&config, code.to_string()).unwrap();
        let defs = get_function_definitions(&parsed);
        assert_eq!(defs.len(), 2);

        let ids = get_identifiers(&defs[0].node);
        assert_eq!(ids.len(), 2);
        assert_eq!(ids[0].node.name, "x");
        assert_eq!(ids[1].node.name, "y");

        let ids = get_identifiers(&defs[1].node);
        assert_eq!(ids.len(), 2);
        assert_eq!(ids[0].node.name, "z");
        assert_eq!(ids[1].node.name, "w");
    }

    #[test]
    fn test_get_local_variables() {
        let config = Config::with_gcc();
        let code = "int main() { int x; y; } int foo() { int y; x; }";
        let parsed = driver::parse_preprocessed(&config, code.to_string()).unwrap();
        let defs = get_function_definitions(&parsed);
        assert_eq!(defs.len(), 2);

        let ids = get_local_variables(&defs[0].node);
        assert_eq!(ids, vec!["x"]);

        let ids = get_local_variables(&defs[1].node);
        assert_eq!(ids, vec!["y"]);
    }

    #[test]
    fn test_replace() {
        let config = Config::with_gcc();
        let code = "int main() {} int foo() { bar(); }";
        let parsed = driver::parse_preprocessed(&config, code.to_string()).unwrap();
        let fun_def = get_function_definitions(&parsed).pop().unwrap();
        let span = if let DeclaratorKind::Identifier(x) = &fun_def.node.declarator.node.kind.node {
            x.span
        } else {
            panic!()
        };
        let replaced = replace(fun_def, &parsed, vec![(span, "func")]);
        assert_eq!(replaced, "int func() { bar(); }");
    }
}
