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
            if !d.node.specifiers.iter().any(|s| match &s.node {
                DeclarationSpecifier::StorageClass(s) => matches!(
                    s.node,
                    StorageClassSpecifier::Extern | StorageClassSpecifier::Typedef
                ),
                _ => false,
            }) && !d.node.declarators.is_empty()
                && !d.node.declarators.iter().any(|d| {
                    d.node
                        .declarator
                        .node
                        .derived
                        .iter()
                        .any(|d| matches!(d.node, DerivedDeclarator::Function(_)))
                })
            {
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
struct CallVisitor<'ast>(Vec<&'ast str>);

impl<'ast> Visit<'ast> for CallVisitor<'ast> {
    fn visit_call_expression(&mut self, call_expression: &'ast CallExpression, span: &'ast Span) {
        if let Expression::Identifier(x) = &call_expression.callee.node {
            self.0.push(&x.node.name);
        }
        visit::visit_call_expression(self, call_expression, span)
    }
}

pub fn get_callees(function_definition: &FunctionDefinition) -> Vec<&str> {
    let mut visitor = CallVisitor::default();
    let body = &function_definition.statement;
    visitor.visit_statement(&body.node, &body.span);
    visitor.0
}

#[derive(Default)]
struct IdentifierVisitor<'ast>(Vec<&'ast str>);

impl<'ast> Visit<'ast> for IdentifierVisitor<'ast> {
    fn visit_expression(&mut self, expression: &'ast Expression, span: &'ast Span) {
        match expression {
            Expression::Identifier(x) => self.0.push(&x.node.name),
            _ => visit::visit_expression(self, expression, span),
        }
    }
}

pub fn get_identifiers(function_definition: &FunctionDefinition) -> Vec<&str> {
    let mut visitor = IdentifierVisitor::default();
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
