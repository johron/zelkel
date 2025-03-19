use crate::error;
use crate::lexer::{Token, TokenPos, TokenValue};

#[derive(Debug)]
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

#[derive(Debug)]
pub enum StatementKind {
    VariableDeclaration(VariableDeclaration),
    FunctionDeclaration(FunctionDeclaration),
    ExpressionStatement(ExpressionStatement),
}

#[derive(Debug)]
pub struct VariableDeclaration {
    name: String,
    typ: ValueType,
    expr: Expression,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    name: String,
    typ: ValueType,
    body: Vec<Statement>,
}

#[derive(Debug)]
pub struct ExpressionStatement {
    typ: ValueType,
    expr: Expression,
}

#[derive(Debug, Clone)]
pub struct Expression {
    kind: ExpressionKind,
    typ: ValueType
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Primary(PrimaryExpression),
    Unary(UnaryExpression),
    Term(TermExpression),
    Comparison(ComparisonExpression),
    Binary(BinaryExpression),
}

#[derive(Debug, Clone)]
pub struct PrimaryExpression {
    value: TokenValue,
    typ: ValueType,
}

#[derive(Clone, Debug)]
pub struct UnaryExpression {
    left: PrimaryExpression,
    typ: ValueType,
    op: Option<Token>,
}

#[derive(Clone, Debug)]
pub struct TermExpression {
    right: Option<UnaryExpression>,
    left: Option<UnaryExpression>,
    typ: ValueType,
    op: Option<Token>,
}

#[derive(Clone, Debug)]
pub struct ComparisonExpression {
    right: Option<TermExpression>,
    left: Option<TermExpression>,
    typ: ValueType,
    op: Option<Token>,
}

#[derive(Clone, Debug)]
pub struct BinaryExpression {
    right: Option<ComparisonExpression>,
    left: Option<ComparisonExpression>,
    typ: ValueType,
    op: Option<Token>,
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

fn parse_primary_expression(i: &usize, toks: &Vec<Token>) -> Result<(PrimaryExpression, usize), String> {
    let mut i = *i;
    let t = toks[i].clone();
    i += 1;
    match t.value {
        TokenValue::String(_) => {
            Ok((PrimaryExpression {
                value: t.value.clone(),
                typ: ValueType::String,
            }, i))
        },
        TokenValue::Integer(_) => {
            Ok((PrimaryExpression {
                value: t.value.clone(),
                typ: ValueType::Integer,
            }, i))
        },
        TokenValue::Float(_) => {
            Ok((PrimaryExpression {
                value: t.value.clone(),
                typ: ValueType::Float,
            }, i))
        },
        TokenValue::Bool(_) => {
            Ok((PrimaryExpression {
                value: t.value.clone(),
                typ: ValueType::Bool,
            }, i))
        },
        TokenValue::Identifier(_) => {
            todo!()
        }
        _ => Err(error("Unexpected token found while parsing primary expression".to_string(), t.pos)),
    }
}

fn parse_unary_expression(i: &usize, toks: &Vec<Token>) -> Result<(UnaryExpression, usize), String> {
    let mut i = *i;
    let t = toks[i].clone();
    if t.value == TokenValue::Arithmetic("-".to_string()) || t.value == TokenValue::Arithmetic("+".to_string()) {
        i += 1;
        let (right, j) = parse_unary_expression(&i, &toks)?;

        Ok((UnaryExpression {
            left: PrimaryExpression {
                value: right.clone().left.value,
                typ: right.clone().left.typ,
            },
            typ: right.typ.clone(),
            op: Some(t.clone()),
        }, j))
    } else {
        let (left, ni) = parse_primary_expression(&i, &toks)?;
        Ok((UnaryExpression {
            left: left.clone(),
            typ: left.typ,
            op: None,
        }, i))
    }
}

fn parse_term_expression(i: &usize, toks: &Vec<Token>) -> Result<(Option<TermExpression>, usize), String> {
    let mut i = *i;
    let mut expr: Option<TermExpression> = None;
    let (left, j) = parse_unary_expression(&i, &toks)?;
    i = j;
    while
        i < toks.len() &&
            (toks[i].value == TokenValue::Arithmetic("*".to_string()) ||
                toks[i].value == TokenValue::Arithmetic("/".to_string()) ||
                toks[i].value == TokenValue::Arithmetic("%".to_string())) &&
            toks[i + 1].value != TokenValue::Punctuation(";".to_string()) {
        let op = expect(&i, &toks, TokenValue::Arithmetic("".to_string()))?;
        i += 1;
        let (right, k) = parse_unary_expression(&i, &toks)?;
        i = k;
        if left.typ != right.typ {
            return Err(error("Type mismatch".to_string(), toks[i].pos.clone()));
        }
        expr = Some(TermExpression {
            left: Some(left.clone()),
            right: Some(right.clone()),
            typ: expr.clone().unwrap().typ.clone(),
            op: Some(op),
        });
    }

    if expr.is_none() {
        return Ok((Some(TermExpression {
            left: Some(left.clone()),
            right: None,
            typ: left.typ.clone(),
            op: None,
        }), i));
    }

    Ok((expr, i))
}

fn parse_comparison_expression(i: &usize, toks: &Vec<Token>) -> Result<(Option<ComparisonExpression>, usize), String> {
    let mut i = *i;
    let mut expr: Option<ComparisonExpression> = None;
    let (left, j) = parse_term_expression(&i, &toks)?;
    i = j;
    while
        i < toks.len() &&
            (toks[i].value == TokenValue::Arithmetic("==".to_string()) ||
                toks[i].value == TokenValue::Arithmetic("!=".to_string()) ||
                toks[i].value == TokenValue::Arithmetic(">=".to_string()) ||
                toks[i].value == TokenValue::Arithmetic("<=".to_string()) ||
                toks[i].value == TokenValue::Arithmetic(">".to_string()) ||
                toks[i].value == TokenValue::Arithmetic("<".to_string())) &&
            toks[i + 1].value != TokenValue::Punctuation(";".to_string()) {
        let op = expect(&i, &toks, TokenValue::Arithmetic("".to_string()))?;
        i += 1;
        let (right, k) = parse_term_expression(&i, &toks)?;
        i = k;
        if left.clone().unwrap().typ != right.clone().unwrap().typ {
            return Err(error("Type mismatch".to_string(), toks[i].pos.clone()));
        }
        expr = Some(ComparisonExpression {
            left: left.clone(),
            right: right.clone(),
            typ: expr.clone().unwrap().typ.clone(),
            op: Some(op),
        });
    }

    if expr.is_none() {
        return Ok((Some(ComparisonExpression {
            left: left.clone(),
            right: None,
            typ: left.clone().unwrap().typ,
            op: None,
        }), i));
    }

    Ok((expr, i))
}

fn parse_expression(i: &usize, toks: &Vec<Token>) -> Result<(Expression, usize), String> {
    let mut i = *i;
    let mut expr: Option<Expression> = None;
    let (left, j) = parse_comparison_expression(&i, &toks)?;
    i = j;
    while
        i < toks.len() &&
            (toks[i].value == TokenValue::Arithmetic("+".to_string()) ||
                toks[i].value == TokenValue::Arithmetic("-".to_string())) &&
            toks[i + 1].value != TokenValue::Punctuation(";".to_string()) {
        let op = expect(&i, &toks, TokenValue::Arithmetic("".to_string()))?;
        i += 1;
        let (right, k) = parse_comparison_expression(&i, &toks)?;
        i = k;
        if left.clone().unwrap().typ != right.clone().unwrap().typ {
            return Err(error("Type mismatch".to_string(), toks[i].pos.clone()));
        }
        expr = Some(Expression {
            kind: ExpressionKind::Binary(BinaryExpression {
                left: Some(left.clone().unwrap()),
                right: Some(right.clone().unwrap()),
                typ: expr.clone().unwrap().typ.clone(),
                op: Some(op),
            }),
            typ: expr.unwrap().typ.clone(),
        });
    }

    if expr.is_none() {
        return Ok((Expression {
            kind: ExpressionKind::Comparison(left.clone().unwrap()),
            typ: left.clone().unwrap().typ,
        }, i));
    }

    Ok((expr.unwrap(), i))
}

fn parse_function_declaration(i: &usize, toks: &Vec<Token>) -> Result<(Statement, usize), String> {
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
            typ: return_type,
            body: todo!("return function body"),
        }),
        pos: toks[i].pos.clone(),
    }, i))
}

fn parse_variable_declaration(i: &usize, toks: &Vec<Token>) -> Result<(Statement, usize), String> {
    let mut i = *i;
    i += 1;
    let name = expect(&i, &toks, TokenValue::empty("identifier")?)?.value.as_string();
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
        return Err(error("Type mismatch".to_string(), toks[i].pos.clone()));
    }

    i = j;
    expect(&i, &toks, TokenValue::Punctuation(";".to_string()))?;

    Ok((Statement {
        kind: StatementKind::VariableDeclaration(VariableDeclaration {
            name,
            typ,
            expr,
        }),
        pos: toks[i].pos.clone(),
    }, i + 1))
}

fn parse_expression_statement(i: &usize, toks: &Vec<Token>) -> Result<(Statement, usize), String> {
    let mut i = *i;
    let (expr, j) = parse_expression(&i, toks)?;
    i = j;
    expect(&i, &toks, TokenValue::Punctuation(";".to_string()))?;

    Ok((Statement {
        kind: StatementKind::ExpressionStatement(ExpressionStatement {
            typ: expr.clone().typ,
            expr,
        }),
        pos: toks[i].pos.clone(),
    }, j))
}

fn parse_identifier(i: &usize, toks: &Vec<Token>) -> Result<(Statement, usize), String> {
    let mut i = *i;
    let t = toks[i].clone();
    let val = t.value;

    let stmt: Result<(Statement, usize), String> = match val {
        TokenValue::Identifier(ref s) => match s.as_str() {
            "fn" => parse_function_declaration(&i, toks),
            "let" => parse_variable_declaration(&i, toks),
            _ => Err(error(format!("Unknown identifier: '{}'", s), t.pos)),
        },
        _ => Err(error("Expected an identifier while parsing identifier".to_string(), t.pos)),
    };

    stmt
}

fn parse_statement(i: &usize, toks: &Vec<Token>) -> Result<(Statement, usize), String> {
    let mut i = *i;
    let pos = toks[i].pos.clone();

    while i < toks.len() {
        return match toks[i].value {
            TokenValue::Identifier(_) => Ok(parse_identifier(&i, toks)?),
            _ => Ok(parse_expression_statement(&i, toks)?),
        }
    }

    Err(error("Unexpected end of file".to_string(), pos))
}

pub fn parse(toks: Vec<Token>) -> Result<Vec<Statement>, String> {
    let mut ast: Vec<Statement> = Vec::new();
    let mut i = 0;

    while i < toks.len() {
        let (stmt, j) = parse_statement(&i, &toks)?;
        ast.push(stmt);
        i = j;
    }

    Ok(ast)
}