use std::path::Path;

use lang_c::{
    ast::*,
    driver::{self, Config, Parse},
    span::{Node, Span},
    visit::{self, Visit},
};

pub fn parse<P: AsRef<Path>>(path: P) -> Parse {
    let mut config = Config::with_gcc();
    config.cpp_options.push("-P".to_string());
    driver::parse(&config, path).unwrap()
}

#[derive(Default)]
struct FunctionDefinitionVisitor<'ast>(Vec<Node<&'ast FunctionDefinition>>);

impl<'ast> Visit<'ast> for FunctionDefinitionVisitor<'ast> {
    fn visit_function_definition(
        &mut self,
        function_definition: &'ast FunctionDefinition,
        span: &'ast Span,
    ) {
        self.0.push(Node::new(function_definition, *span));
    }
}

pub fn get_function_definitions(parsed: &Parse) -> Vec<Node<&FunctionDefinition>> {
    let mut visitor = FunctionDefinitionVisitor::default();
    visitor.visit_translation_unit(&parsed.unit);
    visitor.0
}

#[derive(Default)]
struct GlobalVariableVisitor<'ast>(Vec<Node<&'ast Declaration>>);

impl<'ast> Visit<'ast> for GlobalVariableVisitor<'ast> {
    fn visit_external_declaration(
        &mut self,
        external_declaration: &'ast ExternalDeclaration,
        span: &'ast Span,
    ) {
        if let ExternalDeclaration::Declaration(d) = external_declaration {
            if !d.node.specifiers.iter().any(|s| match &s.node {
                DeclarationSpecifier::StorageClass(s) => match &s.node {
                    StorageClassSpecifier::Extern => true,
                    StorageClassSpecifier::Typedef => true,
                    _ => false,
                },
                _ => false,
            }) && d.node.declarators.len() != 0
                && !d.node.declarators.iter().any(|d| {
                    d.node
                        .declarator
                        .node
                        .derived
                        .iter()
                        .any(|d| match &d.node {
                            DerivedDeclarator::Function(_) => true,
                            _ => false,
                        })
                })
            {
                self.0.push(Node::new(&d.node, *span));
            }
        }
    }
}

pub fn get_global_variables(parsed: &Parse) -> Vec<Node<&Declaration>> {
    let mut visitor = GlobalVariableVisitor::default();
    visitor.visit_translation_unit(&parsed.unit);
    visitor.0
}

#[derive(Default)]
struct CallVisitor<'ast>(Vec<&'ast String>);

impl<'ast> Visit<'ast> for CallVisitor<'ast> {
    fn visit_call_expression(&mut self, call_expression: &'ast CallExpression, span: &'ast Span) {
        match &call_expression.callee.node {
            Expression::Identifier(x) => self.0.push(&x.node.name),
            _ => (),
        }
        visit::visit_call_expression(self, call_expression, span)
    }
}

pub fn get_callees(function_definition: &FunctionDefinition) -> Vec<&String> {
    let mut visitor = CallVisitor::default();
    let body = &function_definition.statement;
    visitor.visit_statement(&body.node, &body.span);
    visitor.0
}

#[derive(Default)]
struct IdentifierVisitor<'ast>(Vec<&'ast String>);

impl<'ast> Visit<'ast> for IdentifierVisitor<'ast> {
    fn visit_expression(&mut self, expression: &'ast Expression, span: &'ast Span) {
        match expression {
            Expression::Identifier(x) => self.0.push(&x.node.name),
            _ => visit::visit_expression(self, expression, span),
        }
    }
}

pub fn get_identifiers(function_definition: &FunctionDefinition) -> Vec<&String> {
    let mut visitor = IdentifierVisitor::default();
    let body = &function_definition.statement;
    visitor.visit_statement(&body.node, &body.span);
    visitor.0
}

pub fn function_name(fun_def: &FunctionDefinition) -> &String {
    if let DeclaratorKind::Identifier(x) = &fun_def.declarator.node.kind.node {
        &x.node.name
    } else {
        panic!()
    }
}

pub fn variable_names(decl: &Declaration) -> Vec<&String> {
    decl.declarators
        .iter()
        .filter_map(|d| match &d.node.declarator.node.kind.node {
            DeclaratorKind::Identifier(x) => Some(&x.node.name),
            _ => None,
        })
        .collect()
}

pub fn node_to_string<'a, T>(node: &Node<T>, parse: &'a Parse) -> &'a str {
    &parse.source[node.span.start..node.span.end]
}
