use nom::IResult;
use nom::multi::many0;
use crate::ast::ast::{Class, Item};
use crate::lexer::Token;
use crate::parser::parse_field::parse_field;
use crate::parser::parse_ident::parse_ident;
use crate::parser::parser::{match_token, Tokens};

fn parse_class(input: Tokens) -> IResult<Tokens, Item> {
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

    let (input, fields) = many0(parse_field)(input)?;

    let (input, _) = match_token(Token::RBrace)(input)?;

    Ok((
        input,
        Item::Class(Class {
            name,
            fields,
            methods: vec![],
            dynamic,
            public,
        }),
    ))
}