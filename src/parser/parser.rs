use nom::branch::alt;
use nom::{IResult, Parser};
use nom::combinator::{all_consuming, map};
use nom::error::{Error, ErrorKind};
use nom::multi::many0;

use crate::ast::ast::{Item, Program};
use crate::lexer::token::{Token, Tokens};
use crate::parser::parse_class::parse_class;
use crate::parser::parse_function_declaration::parse_function_declaration;
use crate::parser::parse_require::parse_require;

pub(crate) type TokenSlice<'source> = Tokens<'source>;

pub fn parse_program(input: TokenSlice) -> IResult<TokenSlice, Program> {
    all_consuming(
        map(many0(parse_head_item), |items| Program { items })
    ).parse(input)
}
//fn any_token(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, Token<'_>> {
//    match input.tokens.split_first() {
//        Some((first, rest)) => Ok((Tokens::new(rest), *first)),
//        None => Err(nom::Err::Error(Error::new(input, ErrorKind::Eof))),
//    }
//}

fn parse_head_item(input: TokenSlice) -> IResult<TokenSlice, Item> {
    alt((
        |i| parse_require(i),
        |i| parse_class(i),
        |i| parse_function_item(i)
    )).parse(input)
}

fn parse_function_item(input: TokenSlice) -> IResult<TokenSlice, Item> {
    let (input, func) = parse_function_declaration(input)?;
    Ok((input, Item::Function(func)))
}

pub fn match_token<'a>(
    check: impl Fn(&Token<'a>) -> bool
) -> impl Fn(TokenSlice<'a>) -> IResult<TokenSlice<'a>, Token<'a>> {
    move |input: TokenSlice<'a>| {
        match input.tokens.split_first() {
            Some((tok, rest)) if check(tok) => {
                Ok((Tokens::new(rest), *tok))
            }
            _ => Err(nom::Err::Error(Error::new(
                input,
                ErrorKind::Tag,
            ))),
        }
    }
}
