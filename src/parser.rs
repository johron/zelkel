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
pub struct Expression {
    right: Vec<Expression>,
    left: Vec<Expression>,
    op: Token,
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

fn parse_expression(i: &usize, toks: &Vec<Token>) -> Result<(Expression, usize), String> {
    let mut i = *i;
    let mut expr = Expression {
        right: Vec::new(),
        left: Vec::new(),
        op: Token::empty(),
    };



    Ok((expr, i))
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