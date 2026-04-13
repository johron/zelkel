use nom::IResult;
use nom::multi::many0;
use nom::Parser;
use crate::ast::ast::{Class, Item};
use crate::lexer::Token;
use crate::parser::parse_field::parse_field;
use crate::parser::parse_function::parse_function;
use crate::parser::parse_ident::parse_ident;
use crate::parser::parser::{match_token, TokenSlice};

pub fn parse_class(input: TokenSlice) -> IResult<TokenSlice, Item> {
    let (input, dynamic) = match match_token(Token::Static)(input.clone()) {
        Ok((i, _)) => Ok((i, false)),
        Err(_) => Ok((input, true)),
    }?;

    let (input, _) = match_token(Token::Class)(input)?;

    // After matching 'class', we're committed to parsing a class. Any errors from here on are hard failures.
    let (input, public) = match match_token(Token::Bang)(input.clone()) {
        Ok((i, _)) => Ok((i, true)),
        Err(_) => Ok((input, false)),
    }?;

    let (input, name) = parse_ident(input)?;
    let (input, _) = match_token(Token::LBrace)(input)?;

    // TODO: This might make the order strict, which I don't want. Now: Class: Fields -> Methods. I want: Class: (Fields | Methods)*
    let (input, fields) = (|i| many0(parse_field).parse(i))(input)?;
    let (input, methods) = (|i| many0(parse_function).parse(i))(input)?;

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