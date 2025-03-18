use crate::error;
use crate::lexer::{Token, TokenPos, TokenValue};

#[derive(Debug)]
pub struct Statement {
    kind: StatementKind,
    pos: TokenPos,
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
    value: TokenValue,
    expr: Expression,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    name: String,
    value: TokenValue,
    body: Vec<Statement>,
}

#[derive(Debug)]
pub struct ExpressionStatement {
    value: TokenValue,
    expr: Expression,
}

#[derive(Debug, Clone)]
pub struct Expression {
    kind: ExpressionKind,
    value: TokenValue,
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
}

#[derive(Clone, Debug)]
pub struct UnaryExpression {
    left: Option<PrimaryExpression>,
    value: TokenValue,
    op: Option<Token>,
}

#[derive(Clone, Debug)]
pub struct TermExpression {
    right: Option<UnaryExpression>,
    left: Option<UnaryExpression>,
    value: TokenValue,
    op: Option<Token>,
}

#[derive(Clone, Debug)]
pub struct ComparisonExpression {
    right: Option<TermExpression>,
    left: Option<TermExpression>,
    value: TokenValue,
    op: Option<Token>,
}

#[derive(Clone, Debug)]
pub struct BinaryExpression {
    right: Option<ComparisonExpression>,
    left: Option<ComparisonExpression>,
    value: TokenValue,
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

fn parse_type(tok: &Token) -> Result<TokenValue, String> {
    match tok.value {
        TokenValue::Identifier(ref s) => match s.as_str() {
            "int" => Ok(TokenValue::empty("integer")?),
            "str" => Ok(TokenValue::empty("string")?),
            "float" => Ok(TokenValue::empty("float")?),
            "bool" => Ok(TokenValue::empty("bool")?),
            _ => Err(error(format!("Unknown type: '{}'", s), tok.pos.clone())),
        },
        _ => Err(error("Expected an identifier while parsing type".to_string(), tok.pos.clone())),
    }
}

fn parse_primary_expression(i: &usize, toks: &Vec<Token>) -> Result<Option<PrimaryExpression>, String> {
    let mut i = *i;
    let t = toks[i].clone();
    match t.value {
        TokenValue::String(_) | TokenValue::Integer(_) | TokenValue::Float(_) | TokenValue::Bool(_) => {
            Ok(Some(PrimaryExpression {
                value: t.value.clone(),
            }))
        },
        TokenValue::Identifier(_) => {
            todo!()
        }
        _ => Err(error("Unexpected token found while parsing primary expression".to_string(), t.pos)),
    }
}

fn parse_unary_expression(i: &usize, toks: &Vec<Token>) -> Result<Option<UnaryExpression>, String> {
    let mut i = *i;
    let t = toks[i].clone();
    if t.value == TokenValue::Arithmetic("-".to_string()) || t.value == TokenValue::Arithmetic("+".to_string()) {
        i += 1;
        let right = parse_unary_expression(&i, &toks)?.unwrap();

        Ok(Some(UnaryExpression {
            left: Some(PrimaryExpression {
                value: right.clone().value.clone(),
            }),
            value: t.value.clone(),
            op: Some(t.clone()),
        }))
    } else {
        let left = parse_primary_expression(&i, &toks)?;
        Ok(Some(UnaryExpression {
            left: left.clone(),
            value: left.unwrap().value,
            op: None,
        }))
    }
}

fn parse_term_expression(i: &usize, toks: &Vec<Token>) -> Result<Option<TermExpression>, String> {
    let mut i = *i;
    let mut expr: Option<TermExpression> = None;
    let left = parse_unary_expression(&i, &toks)?.unwrap();
    while
        i < toks.len() &&
            (toks[i].value == TokenValue::Arithmetic("*".to_string()) ||
                toks[i].value == TokenValue::Arithmetic("/".to_string()) ||
                toks[i].value == TokenValue::Arithmetic("%".to_string())) &&
            toks[i + 1].value != TokenValue::Punctuation(";".to_string()) {
        let op = expect(&i, &toks, TokenValue::Arithmetic("".to_string()))?;
        i += 1;
        let right = parse_unary_expression(&i, &toks)?.unwrap();
        if left.value != right.value {
            return Err(error("Type mismatch".to_string(), toks[i].pos.clone()));
        }
        expr = Some(TermExpression {
            left: Some(left.clone()),
            right: Some(right.clone()),
            value: expr.clone().unwrap().value.clone(),
            op: Some(op),
        });
    }

    Ok(expr)
}

fn parse_comparison_expression(i: &usize, toks: &Vec<Token>) -> Result<Option<ComparisonExpression>, String> {
    let mut i = *i;
    let mut expr: Option<ComparisonExpression> = None;
    let left = parse_term_expression(&i, &toks)?.unwrap();
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
        let right = parse_term_expression(&i, &toks)?.unwrap();
        if left.value != right.value {
            return Err(error("Type mismatch".to_string(), toks[i].pos.clone()));
        }
        expr = Some(ComparisonExpression {
            left: Some(left.clone()),
            right: Some(right.clone()),
            value: expr.clone().unwrap().value.clone(),
            op: Some(op),
        });
    }

    Ok(expr.clone())
}

fn parse_expression(i: &usize, toks: &Vec<Token>) -> Result<(Expression, usize), String> {
    let mut i = *i;
    let mut expr: Option<Expression> = None;
    let left = parse_comparison_expression(&i, &toks)?.unwrap();
    while
        i < toks.len() &&
            (toks[i].value == TokenValue::Arithmetic("+".to_string()) ||
                toks[i].value == TokenValue::Arithmetic("-".to_string())) &&
            toks[i + 1].value != TokenValue::Punctuation(";".to_string()) {
        let op = expect(&i, &toks, TokenValue::Arithmetic("".to_string()))?;
        i += 1;
        let right = parse_comparison_expression(&i, &toks)?.unwrap();
        if left.value != right.value {
            return Err(error("Type mismatch".to_string(), toks[i].pos.clone()));
        }
        expr = Some(Expression {
            kind: ExpressionKind::Binary(BinaryExpression {
                left: Some(left.clone()),
                right: Some(right.clone()),
                value: expr.clone().unwrap().value.clone(),
                op: Some(op),
            }),
            value: expr.unwrap().value.clone(),
        });
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
            value: return_type,
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
    let value = parse_type(&expect(&i, &toks, TokenValue::empty("identifier")?)?)?;
    i += 1;
    expect(&i, &toks, TokenValue::Punctuation("=".to_string()))?;
    i += 1;
    let (expr, j) = parse_expression(&i, toks)?;
    i = j;
    expect(&i, &toks, TokenValue::Punctuation(";".to_string()))?;

    Ok((Statement {
        kind: StatementKind::VariableDeclaration(VariableDeclaration {
            name,
            value,
            expr,
        }),
        pos: toks[i].pos.clone(),
    }, j))
}

fn parse_expression_statement(i: &usize, toks: &Vec<Token>) -> Result<(Statement, usize), String> {
    let mut i = *i;
    let (expr, j) = parse_expression(&i, toks)?;
    i = j;
    expect(&i, &toks, TokenValue::Punctuation(";".to_string()))?;

    Ok((Statement {
        kind: StatementKind::ExpressionStatement(ExpressionStatement {
            value: todo!("need to auto infer type"),
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