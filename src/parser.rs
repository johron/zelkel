mod statement;

use std::collections::HashMap;
use crate::error;
use crate::lexer::{Token, TokenPos, TokenValue};

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
    //name: String,
    //typ: ValueType,
    //expr: Expression,
    //class: bool,
    //pos: TokenPos,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    //name: String,
    //typ: crate::parser_old::ValueType,
    //body: Vec<crate::parser_old::Statement>,
    //pos: TokenPos,
}

#[derive(Debug, Clone)]
pub struct VariableOptions {
    pub mutable: bool,
    pub typ: crate::parser_old::ValueType,
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