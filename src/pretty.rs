use std::{
    io::{Result, Write},
    ops::Deref,
};

use lang_c::{ast::*, span::Node};

const INDENT_SIZE: usize = 2;

pub trait WriteLine {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()>;
    fn pretty(&self) -> String {
        let mut v = vec![];
        self.write_line(0, &mut v).unwrap();
        String::from_utf8(v).unwrap()
    }
}

pub trait WriteString {
    fn write_string(&self) -> String;
    fn pretty(&self) -> String {
        self.write_string()
    }
}

pub fn pretty<T: WriteLine>(t: &T, write: &mut dyn Write) -> Result<()> {
    t.write_line(0, write)
}

pub fn write<T: WriteLine>(t: &T, write: &mut dyn Write) -> Result<()> {
    t.write_line(0, write)
}

#[inline]
fn write_indent(indent: usize, write: &mut dyn Write) -> Result<()> {
    write!(write, "{}", "  ".repeat(indent))
}

impl<T: WriteLine> WriteLine for Node<T> {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        self.node.write_line(indent, write)
    }
}

impl<T: WriteString> WriteString for Node<T> {
    fn write_string(&self) -> String {
        self.node.write_string()
    }
}

impl<T: WriteString> WriteString for Box<T> {
    fn write_string(&self) -> String {
        self.deref().write_string()
    }
}

impl<T: WriteString> WriteString for &T {
    fn write_string(&self) -> String {
        (*self).write_string()
    }
}

impl<T: WriteString> WriteString for Option<T> {
    fn write_string(&self) -> String {
        if let Some(this) = self {
            this.write_string()
        } else {
            "".to_string()
        }
    }
}

impl WriteString for Identifier {
    fn write_string(&self) -> String {
        self.name.clone()
    }
}

impl WriteString for Constant {
    fn write_string(&self) -> String {
        match self {
            Self::Integer(i) => i.write_string(),
            Self::Float(f) => f.write_string(),
            Self::Character(c) => c.clone(),
        }
    }
}

impl WriteString for Integer {
    fn write_string(&self) -> String {
        let b = self.base.write_string();
        let s = self.suffix.write_string();
        format!("{}{}{}", b, self.number, s)
    }
}

impl WriteString for IntegerBase {
    fn write_string(&self) -> String {
        (match &self {
            Self::Decimal => "",
            Self::Octal => "0",
            Self::Hexadecimal => "0x",
            Self::Binary => "0b",
        })
        .to_string()
    }
}

impl WriteString for IntegerSuffix {
    fn write_string(&self) -> String {
        let s = self.size.write_string();
        let u = if self.unsigned { "u" } else { "" };
        format!("{}{}", u, s)
    }
}

impl WriteString for IntegerSize {
    fn write_string(&self) -> String {
        (match self {
            Self::Int => "",
            Self::Long => "l",
            Self::LongLong => "ll",
        })
        .to_string()
    }
}

impl WriteString for Float {
    fn write_string(&self) -> String {
        let b = self.base.write_string();
        let s = self.suffix.write_string();
        format!("{}{}{}", b, self.number, s)
    }
}

impl WriteString for FloatBase {
    fn write_string(&self) -> String {
        (match self {
            Self::Decimal => "",
            Self::Hexadecimal => "0x",
        })
        .to_string()
    }
}

impl WriteString for FloatSuffix {
    fn write_string(&self) -> String {
        self.format.write_string()
    }
}

impl WriteString for FloatFormat {
    fn write_string(&self) -> String {
        (match self {
            Self::Float => "f",
            Self::Double => "",
            Self::LongDouble => "l",
            Self::TS18661Format(_) => panic!("TS18861"),
        })
        .to_string()
    }
}

impl WriteString for Expression {
    fn write_string(&self) -> String {
        match self {
            Self::Identifier(i) => i.write_string(),
            Self::Constant(c) => c.write_string(),
            Self::StringLiteral(s) => s.node[0].clone(),
            Self::GenericSelection(_) => panic!("Expression::GenericSelection"),
            Self::Member(e) => e.write_string(),
            Self::Call(e) => e.write_string(),
            Self::CompoundLiteral(_) => panic!("Expression::CompoundLiteral"),
            Self::SizeOfTy(t) => format!("sizeof ({})", t.write_string()),
            Self::SizeOfVal(v) => format!("sizeof ({})", v.write_string()),
            Self::AlignOf(a) => format!("_Alignof ({})", a.write_string()),
            Self::UnaryOperator(e) => e.write_string(),
            Self::Cast(e) => e.write_string(),
            Self::BinaryOperator(e) => format!("({})", e.write_string()),
            Self::Conditional(e) => e.write_string(),
            Self::Comma(es) => mk_string(es, ", "),
            Self::OffsetOf(_) => panic!("Expression::OffsetOf"),
            Self::VaArg(_) => panic!("Expression::VaArg"),
            Self::Statement(_) => panic!("Expression::Statement"),
        }
    }
}

impl WriteString for SizeOfTy {
    fn write_string(&self) -> String {
        self.0.write_string()
    }
}

impl WriteString for SizeOfVal {
    fn write_string(&self) -> String {
        self.0.write_string()
    }
}

impl WriteString for AlignOf {
    fn write_string(&self) -> String {
        self.0.write_string()
    }
}

impl WriteString for MemberOperator {
    fn write_string(&self) -> String {
        (match self {
            Self::Direct => ".",
            Self::Indirect => "->",
        })
        .to_string()
    }
}

impl WriteString for MemberExpression {
    fn write_string(&self) -> String {
        let o = self.operator.write_string();
        let e = self.expression.write_string();
        let i = self.identifier.write_string();
        format!("{}{}{}", e, o, i)
    }
}

impl WriteString for CallExpression {
    fn write_string(&self) -> String {
        let c = self.callee.write_string();
        let a = mk_string(&self.arguments, ", ");
        format!("{}({})", c, a)
    }
}

impl WriteString for UnaryOperatorExpression {
    fn write_string(&self) -> String {
        let (pre, o) = match self.operator.node {
            UnaryOperator::PostIncrement => (false, "++"),
            UnaryOperator::PostDecrement => (false, "--"),
            UnaryOperator::PreIncrement => (true, "++"),
            UnaryOperator::PreDecrement => (true, "--"),
            UnaryOperator::Address => (true, "&"),
            UnaryOperator::Indirection => (true, "*"),
            UnaryOperator::Plus => (true, "+"),
            UnaryOperator::Minus => (true, "-"),
            UnaryOperator::Complement => (true, "~"),
            UnaryOperator::Negate => (true, "!"),
        };
        let e = self.operand.write_string();
        if pre {
            format!("{}{}", o, e)
        } else {
            format!("{}{}", e, o)
        }
    }
}

impl WriteString for CastExpression {
    fn write_string(&self) -> String {
        let t = self.type_name.write_string();
        let e = self.expression.write_string();
        format!("({}) {}", t, e)
    }
}

impl WriteString for BinaryOperator {
    fn write_string(&self) -> String {
        (match self {
            Self::Index => panic!("Unreachable:BinaryOperator:Index"),
            Self::Multiply => "*",
            Self::Divide => "/",
            Self::Modulo => "%",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::ShiftLeft => "<<",
            Self::ShiftRight => ">>",
            Self::Less => "<",
            Self::Greater => ">",
            Self::LessOrEqual => "<=",
            Self::GreaterOrEqual => ">=",
            Self::Equals => "==",
            Self::NotEquals => "!=",
            Self::BitwiseAnd => "&",
            Self::BitwiseXor => "^",
            Self::BitwiseOr => "|",
            Self::LogicalAnd => "&&",
            Self::LogicalOr => "||",
            Self::Assign => "=",
            Self::AssignMultiply => "*=",
            Self::AssignDivide => "/=",
            Self::AssignModulo => "%=",
            Self::AssignPlus => "+=",
            Self::AssignMinus => "-=",
            Self::AssignShiftLeft => "<<=",
            Self::AssignShiftRight => ">>=",
            Self::AssignBitwiseAnd => "&=",
            Self::AssignBitwiseXor => "^=",
            Self::AssignBitwiseOr => "|=",
        })
        .to_string()
    }
}

impl WriteString for BinaryOperatorExpression {
    fn write_string(&self) -> String {
        let l = self.lhs.write_string();
        let r = self.rhs.write_string();
        if let BinaryOperator::Index = self.operator.node {
            format!("{}[{}]", l, r)
        } else {
            let o = self.operator.write_string();
            format!("{} {} {}", l, o, r)
        }
    }
}

impl WriteString for ConditionalExpression {
    fn write_string(&self) -> String {
        let c = self.condition.write_string();
        let t = self.then_expression.write_string();
        let e = self.else_expression.write_string();
        format!("{} ? {} : {}", c, t, e)
    }
}

impl WriteString for Declaration {
    fn write_string(&self) -> String {
        let s = mk_string(&self.specifiers, " ");
        let d = mk_string(&self.declarators, ", ");
        format!("{} {};", s, d)
    }
}

impl WriteString for DeclarationSpecifier {
    fn write_string(&self) -> String {
        match self {
            Self::StorageClass(c) => c.write_string(),
            Self::TypeSpecifier(s) => s.write_string(),
            Self::TypeQualifier(q) => q.write_string(),
            Self::Function(f) => f.write_string(),
            Self::Alignment(_) => panic!("DeclarationSpecifier::Alignment"),
            Self::Extension(_) => panic!("DeclarationSpecifier::Extension"),
        }
    }
}

impl WriteString for FunctionSpecifier {
    fn write_string(&self) -> String {
        (match self {
            Self::Inline => "inline",
            Self::Noreturn => "_Noreturn",
        })
        .to_string()
    }
}

impl WriteString for StorageClassSpecifier {
    fn write_string(&self) -> String {
        match self {
            Self::Typedef => "typedef",
            Self::Extern => "extern",
            Self::Static => "static",
            Self::ThreadLocal => "_Thread_local",
            Self::Auto => "auto",
            Self::Register => "register",
        }
        .to_string()
    }
}

impl WriteString for InitDeclarator {
    fn write_string(&self) -> String {
        let d = self.declarator.write_string();
        if let Some(i) = &self.initializer {
            format!("{} = {}", d, i.write_string())
        } else {
            d
        }
    }
}

impl WriteString for TypeSpecifier {
    fn write_string(&self) -> String {
        match self {
            Self::Void => "void".to_string(),
            Self::Char => "char".to_string(),
            Self::Short => "short".to_string(),
            Self::Int => "int".to_string(),
            Self::Long => "long".to_string(),
            Self::Float => "float".to_string(),
            Self::Double => "double".to_string(),
            Self::Signed => "signed".to_string(),
            Self::Unsigned => "unsigned".to_string(),
            Self::Bool => "_Bool".to_string(),
            Self::Complex => panic!("TypeSpecifier::Complex"),
            Self::Atomic(_) => panic!("TypeSpecifier::Atomic"),
            Self::Struct(t) => t.write_string(),
            Self::Enum(e) => e.write_string(),
            Self::TypedefName(n) => n.write_string(),
            Self::TypeOf(_) => panic!("TypeSpecifier::TypeOf"),
            Self::TS18661Float(t) => t.write_string(),
        }
    }
}

impl WriteString for TS18661FloatType {
    fn write_string(&self) -> String {
        let (prefix, suffix) = match self.format {
            TS18661FloatFormat::BinaryInterchange => ("Float", ""),
            TS18661FloatFormat::BinaryExtended => ("Float", "x"),
            TS18661FloatFormat::DecimalInterchange => ("Decimal", ""),
            TS18661FloatFormat::DecimalExtended => ("Decimal", "x"),
        };
        format!("_{}{}{}", prefix, self.width, suffix)
    }
}

impl WriteString for EnumType {
    fn write_string(&self) -> String {
        let i = self.identifier.write_string();
        if self.enumerators.len() == 0 {
            format!("enum {}", i)
        } else {
            let e = mk_string(&self.enumerators, ", ");
            format!("enum {} {{ {} }}", i, e)
        }
    }
}

impl WriteString for Enumerator {
    fn write_string(&self) -> String {
        assert_eq!(self.extensions.len(), 0);
        let i = self.identifier.write_string();
        if let Some(e) = &self.expression {
            format!("{} = {}", i, e.write_string())
        } else {
            i
        }
    }
}

impl WriteString for StructType {
    fn write_string(&self) -> String {
        let k = self.kind.write_string();
        let i = self.identifier.write_string();
        let d = if let Some(ds) = &self.declarations {
            format!("{{ {} }}", mk_string(&ds, " "))
        } else {
            "".to_string()
        };
        format!("{} {} {}", k, i, d)
    }
}

impl WriteString for StructKind {
    fn write_string(&self) -> String {
        (match self {
            Self::Struct => "struct",
            Self::Union => "union",
        })
        .to_string()
    }
}

impl WriteString for StructDeclaration {
    fn write_string(&self) -> String {
        match self {
            Self::Field(f) => f.write_string(),
            Self::StaticAssert(_) => panic!("StructDeclaration::StaticAssert"),
        }
    }
}

impl WriteString for StructField {
    fn write_string(&self) -> String {
        let s = mk_string(&self.specifiers, " ");
        let d = mk_string(&self.declarators, ", ");
        format!("{} {};", s, d)
    }
}

impl WriteString for SpecifierQualifier {
    fn write_string(&self) -> String {
        match self {
            Self::TypeSpecifier(s) => s.write_string(),
            Self::TypeQualifier(q) => q.write_string(),
            Self::Extension(_) => panic!("SpecifierQualifier::Extension"),
        }
    }
}

impl WriteString for StructDeclarator {
    fn write_string(&self) -> String {
        self.declarator.write_string()
    }
}

impl WriteString for TypeQualifier {
    fn write_string(&self) -> String {
        (match self {
            Self::Const => "const",
            Self::Restrict => "restrict",
            Self::Volatile => "volatile",
            Self::Nonnull => "_Nonnull",
            Self::NullUnspecified => "_Null_unspecified",
            Self::Nullable => "_Nullable",
            Self::Atomic => "_Atomic",
        })
        .to_string()
    }
}

impl WriteString for Declarator {
    fn write_string(&self) -> String {
        let mut k = self.kind.write_string();
        let mut p = String::new();
        for d in self.derived.iter() {
            if let DerivedDeclarator::Pointer(_) = d.node {
                &mut p
            } else {
                &mut k
            }
            .push_str(&d.write_string());
        }
        format!("{} {}", p, k)
    }
}

impl WriteString for DeclaratorKind {
    fn write_string(&self) -> String {
        match self {
            Self::Abstract => "".to_string(),
            Self::Identifier(i) => i.write_string(),
            Self::Declarator(d) => format!("( {} )", d.write_string()),
        }
    }
}

impl WriteString for DerivedDeclarator {
    fn write_string(&self) -> String {
        match self {
            Self::Pointer(q) => format!("* {}", mk_string(q, " ")),
            Self::Array(d) => d.write_string(),
            Self::Function(d) => d.write_string(),
            Self::KRFunction(_) => "()".to_string(),
            Self::Block(_) => panic!("DerivedDeclarator::Block"),
        }
    }
}

impl WriteString for ArrayDeclarator {
    fn write_string(&self) -> String {
        let q = mk_string(&self.qualifiers, " ");
        let s = self.size.write_string();
        format!("[ {} {} ]", q, s)
    }
}

impl WriteString for FunctionDeclarator {
    fn write_string(&self) -> String {
        let p = mk_string(&self.parameters, ", ");
        format!("({})", p)
    }
}

impl WriteString for PointerQualifier {
    fn write_string(&self) -> String {
        match self {
            Self::TypeQualifier(q) => q.write_string(),
            Self::Extension(_) => panic!("PointerQualifier::Extension"),
        }
    }
}

impl WriteString for ArraySize {
    fn write_string(&self) -> String {
        match self {
            Self::Unknown => "".to_string(),
            Self::VariableUnknown => "*".to_string(),
            Self::VariableExpression(e) => e.write_string(),
            Self::StaticExpression(e) => format!("static {}", e.write_string()),
        }
    }
}

impl WriteString for ParameterDeclaration {
    fn write_string(&self) -> String {
        let s = mk_string(&self.specifiers, " ");
        let d = self.declarator.write_string();
        format!("{} {}", s, d)
    }
}

impl WriteString for TypeName {
    fn write_string(&self) -> String {
        let s = mk_string(&self.specifiers, " ");
        let d = self.declarator.write_string();
        format!("{} {}", s, d)
    }
}

impl WriteString for Initializer {
    fn write_string(&self) -> String {
        match self {
            Self::Expression(e) => e.write_string(),
            Self::List(is) => {
                let i = mk_string(is, ", ");
                format!("{{ {} }}", i)
            }
        }
    }
}

impl WriteString for InitializerListItem {
    fn write_string(&self) -> String {
        self.initializer.write_string()
    }
}

impl WriteLine for Statement {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        match self {
            Self::Labeled(s) => s.write_line(indent, write),
            Self::Compound(ss) => {
                let i = if indent >= INDENT_SIZE {
                    indent - INDENT_SIZE
                } else {
                    0
                };
                write_indent(i, write)?;
                writeln!(write, "{{")?;
                for s in ss.iter() {
                    s.write_line(indent, write)?;
                }
                write_indent(i, write)?;
                writeln!(write, "}}")
            }
            Self::Expression(e) => {
                write_indent(indent, write)?;
                writeln!(write, "{};", e.write_string())
            }
            Self::If(s) => s.write_line(indent, write),
            Self::Switch(s) => s.write_line(indent, write),
            Self::While(s) => s.write_line(indent, write),
            Self::DoWhile(s) => s.write_line(indent, write),
            Self::For(s) => s.write_line(indent, write),
            Self::Goto(_) => panic!("Statement::Goto"),
            Self::Continue => {
                write_indent(indent, write)?;
                writeln!(write, "continue;")
            }
            Self::Break => {
                write_indent(indent, write)?;
                writeln!(write, "break;")
            }
            Self::Return(e) => {
                write_indent(indent, write)?;
                writeln!(write, "return {};", e.write_string())
            }
            Self::Asm(_) => panic!("Statement::Asm"),
        }
    }
}

impl WriteLine for LabeledStatement {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        let l = self.label.write_string();
        write_indent(indent, write)?;
        writeln!(write, "{}:", l)?;
        self.statement.write_line(indent + INDENT_SIZE, write)
    }
}

impl WriteLine for IfStatement {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        write_indent(indent, write)?;
        writeln!(write, "if ({})", self.condition.write_string())?;
        self.then_statement
            .write_line(indent + INDENT_SIZE, write)?;
        if let Some(e) = &self.else_statement {
            write_indent(indent, write)?;
            writeln!(write, "else")?;
            e.write_line(indent + INDENT_SIZE, write)
        } else {
            Ok(())
        }
    }
}

impl WriteLine for SwitchStatement {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        write_indent(indent, write)?;
        writeln!(write, "switch ({})", self.expression.write_string())?;
        self.statement.write_line(indent + INDENT_SIZE, write)
    }
}

impl WriteLine for WhileStatement {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        write_indent(indent, write)?;
        writeln!(write, "while ({})", self.expression.write_string())?;
        self.statement.write_line(indent + INDENT_SIZE, write)
    }
}

impl WriteLine for DoWhileStatement {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        write_indent(indent, write)?;
        writeln!(write, "do")?;
        self.statement.write_line(indent + INDENT_SIZE, write)?;
        write_indent(indent, write)?;
        writeln!(write, "while ({});", self.expression.write_string())
    }
}

impl WriteLine for ForStatement {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        write_indent(indent, write)?;
        let i = self.initializer.write_string();
        let c = self.condition.write_string();
        let s = self.step.write_string();
        writeln!(write, "for ({} {}; {})", i, c, s)?;
        self.statement.write_line(indent + INDENT_SIZE, write)
    }
}

impl WriteString for Label {
    fn write_string(&self) -> String {
        match self {
            Self::Identifier(_) => panic!("Label::Identifier"),
            Self::Case(e) => format!("case {}", e.write_string()),
            Self::Default => "default".to_string(),
            Self::CaseRange(_) => panic!("Label::CaseRange"),
        }
    }
}

impl WriteString for ForInitializer {
    fn write_string(&self) -> String {
        match self {
            Self::Empty => ";".to_string(),
            Self::Expression(e) => format!("{};", e.write_string()),
            Self::Declaration(d) => d.write_string(),
            Self::StaticAssert(_) => panic!("ForInitializer::StaticAssert"),
        }
    }
}

impl WriteLine for BlockItem {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        match self {
            Self::Declaration(d) => {
                write_indent(indent, write)?;
                writeln!(write, "{}", d.write_string())
            }
            Self::StaticAssert(_) => panic!("BlockItem::StaticAssert"),
            Self::Statement(s) => s.write_line(indent, write),
        }
    }
}

impl WriteLine for TranslationUnit {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        for d in self.0.iter() {
            d.write_line(indent, write)?;
        }
        Ok(())
    }
}

impl WriteLine for ExternalDeclaration {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        match self {
            Self::Declaration(d) => {
                write_indent(indent, write)?;
                writeln!(write, "{}", d.write_string())
            }
            Self::StaticAssert(_) => panic!("ExternalDeclaration::StaticAssert"),
            Self::FunctionDefinition(d) => d.write_line(indent, write),
        }
    }
}

impl WriteLine for FunctionDefinition {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        let s = mk_string(&self.specifiers, " ");
        let d = self.declarator.write_string();
        write_indent(indent, write)?;
        writeln!(write, "{} {}", s, d)?;
        self.statement.write_line(indent + INDENT_SIZE, write)
    }
}

fn mk_string<T: WriteString>(v: &Vec<T>, sep: &str) -> String {
    let v1: Vec<_> = v.iter().map(|t| t.write_string()).collect();
    v1.join(sep)
}
