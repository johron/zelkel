use nom::branch::alt;
use crate::lexer::Token;
use nom::IResult;
use nom::multi::many0;
use crate::ast::ast::{Item, Program};
use crate::parser::parse_class::parse_class;
use crate::parser::parse_function::parse_function;
use crate::parser::parse_require::parse_require;

pub type Tokens<'a> = &'a [Token];


fn parse_item(input: Tokens) -> IResult<Tokens, Item> {
    alt((parse_require, parse_class, parse_function))(input)
}

pub fn parse_program(input: Tokens) -> IResult<Tokens, Program> {
    let (input, items) = many0(parse_item)(input)?;

    Ok((input, Program { items }))
}

pub fn match_token(expected: Token) -> impl Fn(Tokens) -> IResult<Tokens, Token> {
    move |input: Tokens| {
        match input.split_first() {
            Some((tok, rest)) if *tok == expected => Ok((rest, tok.clone())),
            _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
        }
    }
}

pub fn many0_custom<I, O, F>(mut f: F, input: I) -> IResult<I, Vec<O>>
where
    F: FnMut(I) -> IResult<I, O>,
    I: Copy,
{
    let mut results = vec![];
    let mut current = input;
    loop {
        match f(current) {
            Ok((new_input, o)) => {
                results.push(o);
                current = new_input;
            }
            Err(nom::Err::Error(_)) => break,
            Err(e) => return Err(e),
        }
    }
    Ok((current, results))
}
