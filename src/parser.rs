use crate::lexer::{Token, TokenPos};

#[derive(Debug)]
pub struct Statement {
    expr: Expression,
    pos: TokenPos,
}

#[derive(Debug)]
pub struct Expression {
    right: Box<Expression>,
    left: Box<Expression>,
    op: Token,
}

pub fn parse(toks: Vec<Token>) -> Result<Box<Statement>, String> {

}