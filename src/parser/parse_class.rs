use nom::IResult;
use crate::ast::ast::{Class, Item};
use crate::lexer::Token;
use crate::parser::parse_field::parse_field;
use crate::parser::parse_function::parse_function;
use crate::parser::parse_ident::parse_ident;
use crate::parser::parser::{match_token, Tokens, many0_custom};

pub fn parse_class(input: Tokens) -> IResult<Tokens, Item> {
    let (input, dynamic) = match match_token(Token::Static)(input) {
        Ok((i, _)) => Ok((i, false)),
        Err(_) => Ok((input, true)),
    }?;

    let (input, _) = match_token(Token::Class)(input)?;

    let (input, public) = match match_token(Token::Bang)(input) {
        Ok((i, _)) => Ok((i, true)),
        Err(_) => Ok((input, false)),
    }?;

    let (input, name) = parse_ident(input)?;
    let (input, _) = match_token(Token::LBrace)(input)?;

    // TODO: This might make the order strict, which I don't want. Now: Class: Fields -> Methods. I want: Class: (Fields | Methods)*
    let (input, fields) = many0_custom(parse_field, input)?;
    let (input, methods) = many0_custom(parse_function, input)?;

    let (input, _) = match_token(Token::RBrace)(input)?;

    Ok((
        input,
        Item::Class(Class {
            name,
            fields,
            methods,
            dynamic,
            public,
        }),
    ))
}