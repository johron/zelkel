use logos_nom_bridge::Tokens;
use nom::branch::alt;
use crate::lexer2::Token;
use nom::{IResult, Parser};
use nom::multi::many0;

use crate::ast::ast::{Item, Program};
use crate::lexer::token::Tokens;
use crate::parser::parse_class::parse_class;
use crate::parser::parse_function::parse_function;
use crate::parser::parse_require::parse_require;

pub type TokenSlice<'a> = &'a [Token];

type Input<'source> = Tokens<'source, Token>;

pub fn parse_program(input: Input<'_>) -> IResult<Input<'_>, Program> {
    let (input, items) = many0(parse_head_item).parse(input)?;

    Ok((input, Program { items }))
}

//pub fn parse_program(input: &[Token]) -> IResult<&[Token], Program> {
//    let (input, items) = many0(parse_item).parse(input)?;
//
//    Ok((input, Program { items }))
//}

// pub(crate) fn parse_program(input: &[Token]) -> IResult<&[Token], Vec<Item>> {
//     let mut input = input;
//     let mut items = Vec::new();
//
//     while let Ok((next_input, item)) = parse_head_item(input) {
//         items.push(item);
//         input = next_input;
//
//         if input.is_empty() {
//             break;
//         }
//     }
//
//     Ok((input, items))
// }

fn parse_head_item(input: TokenSlice) -> IResult<TokenSlice, Item> {
    alt((parse_require, parse_class, parse_function_item)).parse(input)
}

fn parse_function_item(input: TokenSlice) -> IResult<TokenSlice, Item> {
    let (input, func) = parse_function(input)?;
    Ok((input, Item::Function(func)))
}

pub fn match_token(expected: Token) -> impl Fn(TokenSlice) -> IResult<TokenSlice, Token> {
    move |input: TokenSlice| {
        match input.split_first() {
            Some((tok, rest)) if *tok == expected => Ok((rest, tok.clone())),
            _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
        }
    }
}