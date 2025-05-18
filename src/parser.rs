mod statement;
mod expression;

use statement::parse_class_declaration;

use std::collections::HashMap;
use crate::error;
use crate::lexer::{Token, TokenPos, TokenValue};

#[derive(Debug, Clone)]
pub struct Statement {
    kind: StatementKind,
    pos: TokenPos,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub enum ValueType {
    #[default]
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
    Variable(String, bool),
    Function(String),
    Class(String),
    Arithmetic(String),
    None,
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

#[derive(Debug, Clone)]
pub enum StatementKind {
    VariableDeclaration(VariableDeclaration),
    VariableReassignment(VariableReassignment),
    FunctionDeclaration(FunctionDeclaration),
    ClassDeclaration(ClassDeclaration),
    ConditionalStatement(ConditionalStatement),
    ExpressionStatement(ExpressionStatement),
    Block(Vec<Statement>),
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    name: String,
    typ: ValueType,
    expr: Expression,
    class: bool,
    pos: TokenPos,
}

#[derive(Debug, Clone)]
pub struct VariableReassignment {
    name: String,
    typ: ValueType,
    expr: Expression,
    class: bool,
    pos: TokenPos,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    name: String,
    typ: ValueType,
    body: Vec<Statement>,
    pos: TokenPos,
}

#[derive(Debug, Clone)]
pub struct ClassDeclaration {
    name: String,
    extends: Option<String>,
    body: Vec<Statement>,
    pos: TokenPos,
}

#[derive(Debug, Clone)]
pub struct ConditionalStatement {
    condition: Expression,
    body: Vec<Statement>,
    else_body: Option<Vec<Statement>>,
    pos: TokenPos,
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
pub struct InstantiationExpression {
    class: String,
    args: Vec<Expression>,
    pos: TokenPos,
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
pub struct VariableOptions {
    pub mutable: bool,
    pub typ: ValueType,
}

#[derive(Debug, Clone)]
pub struct FunctionOptions {
    pub args: Vec<VariableOptions>,
    pub typ: ValueType,
    pub public: bool,
}

#[derive(Debug, Clone)]
pub struct ClassOptions {
    pub functions: HashMap<String, FunctionOptions>,
    pub variables: HashMap<String, VariableOptions>,
    pub public: bool,
}

#[derive(Clone, Debug)]
pub struct Scope {
    variables: HashMap<String, VariableOptions>, // Function-nested variables
    classes: HashMap<String, ClassOptions>,
    current_class: Option<String>,
}

fn expect(i: &usize, toks: &Vec<Token>, value: TokenValue, strict: bool) -> Result<Token, String> {
    let i = *i;
    if i >= toks.len() {
        return Err(error("Unexpected end of file".to_string(), toks.last().unwrap().pos.clone()));
    }

    if toks[i].value == value && strict {
        return Ok(toks[i].clone());
    }

    if std::mem::discriminant(&toks[i].value) == std::mem::discriminant(&value) && !strict {
        return Ok(toks[i].clone());
    }

    Err(error(format!("Expected {:?} but got {:?}", value, toks[i].value), toks[i].pos.clone()))
}

fn enter_scope(scope: &mut Vec<Scope>) -> Vec<Scope> {
    let parent_scope = scope.last().cloned().unwrap_or(Scope {
        variables: HashMap::new(),
        classes: HashMap::new(),
        current_class: None,
    });
    scope.push(parent_scope);
    scope.clone()
}

fn exit_scope(scope: &mut Vec<Scope>) -> Vec<Scope> {
    scope.pop().unwrap_or_else(|| Scope {
        variables: HashMap::new(),
        classes: HashMap::new(),
        current_class: None,
    });
    scope.clone()
}

fn parse_type(tok: &Token, scope_stack: &Vec<Scope>) -> Result<ValueType, String> {
    let current_class = scope_stack.last().unwrap().current_class.clone().unwrap();
    match tok.value {
        TokenValue::Identifier(ref s) => match s.as_str() {
            "__prim_int" => Ok(ValueType::PrimitiveInteger),
            "__prim_str" => Ok(ValueType::PrimitiveString),
            "__prim_float" => Ok(ValueType::PrimitiveFloat),
            "__prim_bool" => Ok(ValueType::PrimitiveBool),
            _ => {
                if let Some(_) = scope_stack.last().unwrap().classes.get(s) {
                    Ok(ValueType::Class(s.clone()))
                } else if s == "Self" {
                    Ok(ValueType::Class(current_class.clone()))
                } else if let Some(f) = scope_stack.last().unwrap().classes.get(current_class.as_str()).unwrap().functions.get(s.as_str()) {
                    Ok(f.typ.clone())
                } else if let Some(v) = scope_stack.last().unwrap().variables.get(s) {
                    Ok(v.typ.clone())
                } else {
                    Err(error(format!("Unknown type: '{}'", s), tok.pos.clone()))
                }
            }
        },
        _ => Err(error("Expected an identifier while parsing type".to_string(), tok.pos.clone())),
    }
}



pub fn parse(toks: Vec<Token>) -> Result<Vec<Statement>, String> {
    let mut ast: Vec<Statement> = Vec::new();
    let mut i = 0;

    let mut scope_stack: Vec<Scope> = Vec::new();
    scope_stack.push(Scope { variables: HashMap::new(), classes: HashMap::new(), current_class: None });

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