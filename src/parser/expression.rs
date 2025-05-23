use crate::lexer::Token;
use crate::parser::{Expression, Scope};

pub fn parse_expression(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Expression, usize), String> {
    todo!("Implement parse_expression function");
}