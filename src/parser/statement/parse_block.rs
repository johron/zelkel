use nom::{IResult, Parser, branch::alt, combinator::{cut, map}, multi::many0};

use crate::{ast::ast::{Block, Statement}, expect_token, parser::{expr::parse_expression::parse_expression_statement, parser::{TokenSlice, parse_function_item}, statement::{parse_class::parse_class, parse_function::parse_function_declaration, parse_variable::{self, parse_variable, parse_variable_item}}}};

pub fn parse_block(input: TokenSlice) -> IResult<TokenSlice, Block> {
    let (input, _) = expect_token!(input, LBrace)?;

    // cut, many0 alt

    let (input, block) = cut(map(many0(parse_stmts), |stmts| Block { stmts })).parse(input)?;
    

    let (input, _) = expect_token!(input, RBrace)?;

    Ok((
        input,
        block,
    ))
} 

fn parse_stmts(input: TokenSlice) -> IResult<TokenSlice, Statement> {
    alt((
        |i| parse_class(i),
        |i| parse_function_item(i),
        |i| parse_expression_statement(i),
        |i| parse_variable_item(i),
    )).parse(input)
}

