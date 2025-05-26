mod statement;
mod expression;

use std::collections::HashMap;
use crate::error;
use crate::lexer::{Token, TokenPos, TokenValue};
use crate::parser::statement::parse_class_declaration;

#[derive(Debug, Clone)]
pub struct Statement {
    kind: StatementKind,
    pos: TokenPos,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueType {
    PrimitiveInteger,
    PrimitiveFloat,
    PrimitiveString,
    PrimitiveBool,
    Class(String),
    None,
}

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i32),
    Float(f32),
    String(String),
    Bool(bool),
    Variable(String),
    ClassVariable(String),
    Function(String),
    Class(String),
    Arithmetic(String),
    None,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    ClassDeclaration(ClassDeclaration),
    VariableDeclaration(VariableDeclaration),
    FunctionDeclaration(FunctionDeclaration),
    Block(Vec<Statement>),
}

#[derive(Debug, Clone)]
pub struct ClassDeclaration {
    name: String,
    extends: Option<String>,
    variables: Vec<VariableDeclaration>,
    functions: Vec<FunctionDeclaration>,
    pos: TokenPos,
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    name: String,
    typ: ValueType,
    expr: Option<Expression>,
    pos: TokenPos,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    name: String,
    typ: ValueType,
    body: Vec<Statement>,
    pos: TokenPos,
    pub public: bool,
    pub args: HashMap<String, VariableOptions>,
}

#[derive(Debug, Clone)]
pub struct VariableOptions {
    pub mutable: bool,
    pub public: bool,
    pub typ: ValueType,
}

#[derive(Debug, Clone)]
pub struct FunctionOptions {
    pub args: HashMap<String, VariableOptions>,
    pub typ: ValueType,
    pub public: bool,
}

#[derive(Debug, Clone)]
pub struct ClassOptions {
    pub functions: HashMap<String, FunctionOptions>,
    pub variables: HashMap<String, VariableOptions>,
    pub public: bool,
    pub extends: Option<String>,
}

impl ClassOptions {
    pub fn empty() -> Self {
        ClassOptions {
            functions: HashMap::new(),
            variables: HashMap::new(),
            extends: None,
            public: false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Scope {
    variables: HashMap<String, VariableOptions>,
    functions: HashMap<String, FunctionOptions>,
    classes: HashMap<String, ClassOptions>,
    current_class: ClassOptions,
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    typ: ValueType,
    expr: Expression,
    pos: TokenPos,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Primary(PrimaryExpression),
    Unary(Box<UnaryExpression>),
    Term(Box<TermExpression>),
    Binary(Box<BinaryExpression>),
    Comparison(Box<ComparisonExpression>),
    Instantiation(InstantiationExpression),
}

#[derive(Debug, Clone)]
pub struct Expression {
    kind: ExpressionKind,
    typ: ValueType,
    pos: TokenPos,
}

#[derive(Debug, Clone)]
pub struct PrimaryExpression {
    value: Value,
    typ: ValueType,
    pos: TokenPos,
}

#[derive(Debug, Clone)]
pub struct UnaryExpression {
    left: ExpressionKind,
    typ: ValueType,
    op: Value,
    pos: TokenPos,
}

#[derive(Debug, Clone)]
pub struct TermExpression {
    left: ExpressionKind,
    right: ExpressionKind,
    typ: ValueType,
    op: Value,
    pos: TokenPos,
}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    left: ExpressionKind,
    right: ExpressionKind,
    typ: ValueType,
    op: Value,
    pos: TokenPos,
}

#[derive(Debug, Clone)]
pub struct ComparisonExpression {
    left: ExpressionKind,
    right: ExpressionKind,
    typ: ValueType,
    op: Value,
    pos: TokenPos,
}

#[derive(Debug, Clone)]
pub struct InstantiationExpression {
    class: String,
    args: Vec<Expression>,
    pos: TokenPos,
}

const RESERVED: [&str; 11] = [
    "class",
    "fn",
    "val",
    "if",
    "else",
    "while",
    "for",
    "return",
    "break",
    "continue",
    "Self",
];

fn enter_scope(scope: &mut Vec<Scope>) -> Vec<Scope> {
    let parent_scope = scope.last().cloned().unwrap_or(Scope {
        variables: HashMap::new(),
        functions: HashMap::new(),
        classes: HashMap::new(),
        current_class: ClassOptions::empty(),
    });
    scope.push(parent_scope);
    scope.clone()
}

fn exit_scope(scope: &mut Vec<Scope>) -> Vec<Scope> {
    scope.pop().unwrap_or_else(|| Scope {
        variables: HashMap::new(),
        functions: HashMap::new(),
        classes: HashMap::new(),
        current_class: ClassOptions::empty(),
    });
    scope.clone()
}

impl From<TokenValue> for Value {
    fn from(token_value: TokenValue) -> Self {
        match token_value {
            TokenValue::Integer(i) => Value::Integer(i),
            TokenValue::Float(f) => Value::Float(f),
            TokenValue::String(s) => Value::String(s),
            TokenValue::Bool(b) => Value::Bool(b),
            TokenValue::Arithmetic(s) => Value::Arithmetic(s),
            _ => unimplemented!("Conversion for this TokenValue variant is not implemented"),
        }
    }
}

fn expect(i: &usize, toks: &Vec<Token>, value: TokenValue) -> Result<Token, String> {
    let i = *i;
    if i >= toks.len() {
        return Err(error("Unexpected end of file".to_string(), toks.last().unwrap().pos.clone()));
    }

    if toks[i].value == value {
        return Ok(toks[i].clone());
    }

    Err(error(format!("Expected {:?} but got {:?}", value, toks[i].value), toks[i].pos.clone()))
}

fn expect_unstrict(i: &usize, toks: &Vec<Token>, value: TokenValue) -> Result<Token, String> {
    let i = *i;
    if i >= toks.len() {
        return Err(error("Unexpected end of file".to_string(), toks.last().unwrap().pos.clone()));
    }

    if std::mem::discriminant(&toks[i].value) == std::mem::discriminant(&value) {
        return Ok(toks[i].clone());
    }

    Err(error(format!("Expected {:?} but got {:?}", value, toks[i].value), toks[i].pos.clone()))
}

fn parse_type(tok: &Token, scope_stack: &Vec<Scope>) -> Result<ValueType, String> {
    //let current_class = scope_stack.last().unwrap().current_class.clone().unwrap();
    match tok.value {
        TokenValue::Identifier(ref s) => match s.as_str() {
            "_i" => Ok(ValueType::PrimitiveInteger),
            "_s" => Ok(ValueType::PrimitiveString),
            "_f" => Ok(ValueType::PrimitiveFloat),
            "_b" => Ok(ValueType::PrimitiveBool),
            "i" => Ok(ValueType::Class("Integer".to_string())),
            "s" => Ok(ValueType::Class("String".to_string())),
            "f" => Ok(ValueType::Class("Float".to_string())),
            "b" => Ok(ValueType::Class("Bool".to_string())),
            _ => {
                todo!("it may be a class");
            }
        },
        _ => Err(error("Expected an identifier while parsing type".to_string(), tok.pos.clone())),
    }
}

pub fn parse(toks: Vec<Token>) -> Result<Vec<Statement>, String> {
    let mut ast: Vec<Statement> = Vec::new();
    let mut i = 0;

    let mut scope_stack: Vec<Scope> = Vec::new();
    scope_stack.push(Scope { variables: HashMap::new(), functions: HashMap::new(), classes: HashMap::new(), current_class: ClassOptions::empty() });

    while i < toks.len() {
        if toks[i].value != TokenValue::Identifier("class".to_string()) {
            Err(error("Expected a class declaration".to_string(), toks[i].pos.clone()))?;
        }

        let (stmt, j, scope) = parse_class_declaration(&i, &toks, &mut scope_stack)?;
        scope_stack = scope;
        ast.push(stmt);
        i = j;
    }

    Ok(ast)
}