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
    Integer,
    Float,
    String,
    Bool,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    VariableDeclaration(VariableDeclaration),
    FunctionDeclaration(FunctionDeclaration),
    ExpressionStatement(ExpressionStatement),
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    name: String,
    typ: ValueType,
    expr: Expression,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    name: String,
    typ: ValueType,
    args: Vec<VariableDeclaration>,
    body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    typ: ValueType,
    expr: Expression,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Primary(PrimaryExpression),
    Unary(Box<UnaryExpression>),
    Term(Box<TermExpression>),
    Binary(Box<BinaryExpression>),
    Comparison(Box<ComparisonExpression>),
}

#[derive(Debug, Clone)]
pub struct Expression {
    kind: ExpressionKind,
    typ: ValueType
}

#[derive(Debug, Clone)]
pub struct PrimaryExpression {
    value: TokenValue,
    typ: ValueType,
}

#[derive(Debug, Clone)]
pub struct UnaryExpression {
    left: ExpressionKind,
    typ: ValueType,
    op: TokenValue,
}

#[derive(Debug, Clone)]
pub struct TermExpression {
    left: ExpressionKind,
    right: ExpressionKind,
    typ: ValueType,
    op: TokenValue,
}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    left: ExpressionKind,
    right: ExpressionKind,
    typ: ValueType,
    op: TokenValue,
}

#[derive(Debug, Clone)]
pub struct ComparisonExpression {
    left: ExpressionKind,
    right: ExpressionKind,
    typ: ValueType,
    op: TokenValue,
}

#[derive(Debug, Clone)]
pub struct VariableOptions {
    pub mutable: bool,
    pub typ: ValueType,
}

#[derive(Clone, Debug)]
pub struct Scope {
    variables: HashMap<String, VariableOptions>,
    functions: Vec<String>,
}

fn expect(i: &usize, toks: &Vec<Token>, value: TokenValue) -> Result<Token, String> {
    if i >= &toks.len() {
        return Err(error("Unexpected end of file".to_string(), toks[*i].pos.clone()));
    }

    if toks[*i].value == value {
        return Ok(toks[*i].clone());
    } else if let TokenValue::Identifier(_) | TokenValue::String(_) | TokenValue::Arithmetic(_) | TokenValue::Punctuation(_) = value {
        return Ok(toks[*i].clone());
    }

    Err(error(format!("Expected {:?} but got {:?}", value, toks[*i].value), toks[*i].pos.clone()))
}

fn enter_scope(scope: &mut Vec<Scope>) -> Vec<Scope> {
    let parent_scope = scope.last().cloned().unwrap_or(Scope {
        variables: HashMap::new(),
        functions: Vec::new(),
    });
    scope.push(parent_scope);
    scope.clone()
}

fn exit_scope(scope: &mut Vec<Scope>) -> Vec<Scope> {
    scope.pop();
    scope.clone()
}

fn parse_type(tok: &Token) -> Result<ValueType, String> {
    match tok.value {
        TokenValue::Identifier(ref s) => match s.as_str() {
            "int" => Ok(ValueType::Integer),
            "str" => Ok(ValueType::String),
            "float" => Ok(ValueType::Float),
            "bool" => Ok(ValueType::Bool),
            _ => Err(error(format!("Unknown type: '{}'", s), tok.pos.clone())),
        },
        _ => Err(error("Expected an identifier while parsing type".to_string(), tok.pos.clone())),
    }
}

fn parse_primary_expression(i: &usize, toks: &Vec<Token>) -> Result<(Expression, usize), String> {
    let mut i = *i;
    let tok = &toks[i];
    let expr = match &tok.value {
        TokenValue::Integer(_) | TokenValue::Float(_) | TokenValue::String(_) | TokenValue::Bool(_) => {
            Expression {
                kind: ExpressionKind::Primary(PrimaryExpression {
                    value: tok.value.clone(),
                    typ: match &tok.value {
                        TokenValue::Integer(_) => ValueType::Integer,
                        TokenValue::Float(_) => ValueType::Float,
                        TokenValue::String(_) => ValueType::String,
                        TokenValue::Bool(_) => ValueType::Bool,
                        _ => unreachable!(),
                    },
                }),
                typ: match &tok.value {
                    TokenValue::Integer(_) => ValueType::Integer,
                    TokenValue::Float(_) => ValueType::Float,
                    TokenValue::String(_) => ValueType::String,
                    TokenValue::Bool(_) => ValueType::Bool,
                    _ => unreachable!(),
                },
            }
        }
        TokenValue::Punctuation(p) => {
            if p == "(" {
                i += 1;
                let (expr, j) = parse_expression(&i, toks)?;
                i = j;
                expect(&i, &toks, TokenValue::Punctuation(")".to_string()))?;
                expr
            } else {
                return Err(error("Expected a primary expression".to_string(), tok.pos.clone()));
            }
        }
        _ => return Err(error("Expected a primary expression".to_string(), tok.pos.clone())),
    };
    i += 1;
    Ok((expr, i))
}

fn parse_unary_expression(i: &usize, toks: &Vec<Token>) -> Result<(Expression, usize), String> {
    let mut i = *i;
    let tok = &toks[i];
    if let TokenValue::Arithmetic(_) = &tok.value {
        i += 1;
        let (expr, j) = parse_primary_expression(&mut i, toks)?;
        return Ok((Expression {
            kind: ExpressionKind::Unary(Box::from(UnaryExpression {
                left: expr.kind,
                typ: expr.typ.clone(),
                op: tok.clone().value,
            })),
            typ: expr.typ,
        }, j));
    }
    parse_primary_expression(&mut i, toks)
}

fn parse_term_expression(i: &usize, toks: &Vec<Token>) -> Result<(Expression, usize), String> {
    let mut i = *i;
    let (mut expr, j) = parse_unary_expression(&mut i, toks)?;
    i = j;
    while i < toks.len() {
        let tok = &toks[i];
        if let TokenValue::Arithmetic(op) = &tok.value {
            if op == "*" || op == "/" {
                i += 1;
                let (right, h) = parse_unary_expression(&mut i, toks)?;
                i = h;
                expr = Expression {
                    kind: ExpressionKind::Term(Box::from(TermExpression {
                        left: expr.kind,
                        right: right.kind,
                        typ: expr.typ.clone(),
                        op: tok.clone().value,
                    })),
                    typ: expr.typ,
                };
            } else {
                break;
            }
        } else {
            break;
        }
    }
    Ok((expr, i))
}

fn parse_binary_expression(i: &usize, toks: &Vec<Token>) -> Result<(Expression, usize), String> {
    let mut i = *i;
    let (mut expr, j) = parse_term_expression(&mut i, toks)?;
    i = j;
    while i < toks.len() {
        let tok = &toks[i];
        if let TokenValue::Arithmetic(op) = &tok.value {
            if op == "+" || op == "-" || op == "%" {
                i += 1;
                let (right, h) = parse_term_expression(&mut i, toks)?;
                i = h;
                expr = Expression {
                    kind: ExpressionKind::Binary(Box::from(BinaryExpression {
                        left: expr.kind,
                        right: right.kind,
                        typ: expr.typ.clone(),
                        op: tok.clone().value,
                    })),
                    typ: expr.typ,
                };
            } else {
                break;
            }
        } else {
            break;
        }
    }
    Ok((expr, i))
}

fn parse_comparison_expression(i: &usize, toks: &Vec<Token>) -> Result<(Expression, usize), String> {
    let mut i = *i;
    let (mut expr, j) = parse_binary_expression(&mut i, toks)?;
    i = j;
    while i < toks.len() {
        let tok = &toks[i];
        if let TokenValue::Arithmetic(op) = &tok.value {
            if op == "==" || op == "!=" || op == "<" || op == ">" || op == "<=" || op == ">=" {
                i += 1;
                let (right, h) = parse_binary_expression(&mut i, toks)?;
                i = h;
                expr = Expression {
                    kind: ExpressionKind::Comparison(Box::from(ComparisonExpression {
                        left: expr.kind,
                        right: right.kind,
                        typ: expr.typ.clone(),
                        op: tok.clone().value,
                    })),
                    typ: expr.typ,
                };
            } else {
                break;
            }
        } else {
            break;
        }
    }
    Ok((expr, i))
}

fn parse_expression(i: &usize, toks: &Vec<Token>) -> Result<(Expression, usize), String> {
    parse_comparison_expression(i, toks)
}

fn parse_class_declaration(i: &usize, toks: &Vec<Token>) -> Result<(Statement, usize), String> {
    let mut i = *i;
    i += 1;
    let name = expect(&i, &toks, TokenValue::empty("identifier")?)?.value.as_string();
    i += 1;
    expect(&i, &toks, TokenValue::Punctuation("{".to_string()))?;
    i += 1;
    todo!("parse class body, should only be function declarations");
    expect(&i, &toks, TokenValue::Punctuation("}".to_string()))?;
    i += 1;
    todo!("do rest");
}

fn parse_function_declaration(i: &usize, toks: &Vec<Token>, global_scope: &mut Vec<Scope>) -> Result<(Statement, usize, Vec<Scope>), String> {
    let mut i = *i;
    i += 1;
    let name = expect(&i, &toks, TokenValue::empty("identifier")?)?.value.as_string();
    i += 1;
    expect(&i, &toks, TokenValue::Punctuation("(".to_string()))?;
    todo!("parse function arguments");
    i += 1;
    expect(&i, &toks, TokenValue::Punctuation(")".to_string()))?;
    i += 1;
    expect(&i, &toks, TokenValue::Punctuation("->".to_string()))?;
    i += 1;
    let return_type = parse_type(&expect(&i, &toks, TokenValue::empty("identifier")?)?)?;
    i += 1;
    expect(&i, &toks, TokenValue::Punctuation("{".to_string()))?;
    i += 1;
    todo!("parse function body");
    expect(&i, &toks, TokenValue::Punctuation("}".to_string()))?;

    Ok((Statement {
        kind: StatementKind::FunctionDeclaration(FunctionDeclaration {
            name,
            args: todo!("function arguments"),
            typ: return_type,
            body: todo!("return function body"),
        }),
        pos: toks[i].pos.clone(),
    }, i, todo!("return scope")))
}

fn parse_variable_declaration(i: &usize, toks: &Vec<Token>, global_scope: &mut Vec<Scope>) -> Result<(Statement, usize, Vec<Scope>), String> {
    let mut i = *i;
    let mut global_scope = global_scope.clone();
    i += 1;
    let name = expect(&i, &toks, TokenValue::empty("identifier")?)?.value.as_string();

    if global_scope.last().unwrap().variables.iter().any(|v| v.0 == &name) {
        return Err(error(format!("Variable '{}' already declared", name), toks[i].pos.clone()));
    }

    i += 1;
    expect(&i, &toks, TokenValue::Punctuation(":".to_string()))?;
    i += 1;
    let type_ident = expect(&i, &toks, TokenValue::empty("identifier")?)?;
    let typ = parse_type(&type_ident)?;
    i += 1;
    expect(&i, &toks, TokenValue::Punctuation("=".to_string()))?;
    i += 1;
    let (expr, j) = parse_expression(&i, toks)?;
    if typ != expr.typ {
        return Err(error(format!("Type mismatch: expected {:?}, but found {:?}", typ, expr.typ), toks[i].pos.clone()));
    }

    i = j;
    expect(&i, &toks, TokenValue::Punctuation(";".to_string()))?;

    global_scope.last_mut().unwrap().variables.insert(name.clone(), VariableOptions {
        mutable: false,
        typ: typ.clone(),
    });

    Ok((Statement {
        kind: StatementKind::VariableDeclaration(VariableDeclaration {
            name,
            typ,
            expr,
        }),
        pos: toks[i].pos.clone(),
    }, i + 1, global_scope))
}

fn parse_expression_statement(i: &usize, toks: &Vec<Token>, global_scope: &mut Vec<Scope>) -> Result<(Statement, usize, Vec<Scope>), String> {
    let mut i = *i;
    let mut global_scope = global_scope.clone();
    let (expr, j) = parse_expression(&i, toks)?;
    i = j;
    expect(&i, &toks, TokenValue::Punctuation(";".to_string()))?;

    Ok((Statement {
        kind: StatementKind::ExpressionStatement(ExpressionStatement {
            typ: expr.clone().typ,
            expr,
        }),
        pos: toks[i].pos.clone(),
    }, j, global_scope))
}

fn parse_identifier(i: &usize, toks: &Vec<Token>, global_scope: &mut Vec<Scope>) -> Result<(Statement, usize, Vec<Scope>), String> {
    let mut i = *i;
    let t = toks[i].clone();
    let val = t.value;

    let stmt: Result<(Statement, usize, Vec<Scope>), String> = match val {
        TokenValue::Identifier(ref s) => match s.as_str() {
            "fn" => parse_function_declaration(&i, toks, global_scope),
            "let" => parse_variable_declaration(&i, toks, global_scope),
            _ => Err(error(format!("Unknown identifier: '{}'", s), t.pos)),
        },
        _ => Err(error("Expected an identifier while parsing identifier".to_string(), t.pos)),
    };

    stmt
}

fn parse_statement(i: &usize, toks: &Vec<Token>, global_scope: &mut Vec<Scope>) -> Result<(Statement, usize, Vec<Scope>), String> {
    let mut i = *i;
    let pos = toks[i].pos.clone();

    while i < toks.len() {
        return match toks[i].value {
            TokenValue::Identifier(_) => Ok(parse_identifier(&i, &toks, global_scope)?),
            _ => Ok(parse_expression_statement(&i, &toks, global_scope)?),
        }
    }

    Err(error("Unexpected end of file".to_string(), pos))
}

pub fn parse(toks: Vec<Token>) -> Result<Vec<Statement>, String> {
    let mut ast: Vec<Statement> = Vec::new();
    let mut i = 0;

    let mut global_scope: Vec<Scope> = Vec::new();
    global_scope.push(Scope { variables: HashMap::new(), functions: Vec::new() });

    while i < toks.len() {
        let (stmt, j, scope) = parse_statement(&i, &toks, &mut global_scope)?;
        global_scope = scope;
        ast.push(stmt);
        i = j;
    }

    Ok(ast)
}