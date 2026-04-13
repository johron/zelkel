use nom::IResult;
use crate::lexer::Token;
use crate::parser::parser::{TokenSlice};

pub fn parse_ident(input: TokenSlice) -> IResult<TokenSlice, String> {
    match input.split_first() {
        Some((Token::Ident(name), rest)) => Ok((rest, name.clone())),
        _ => Err(nom::Err::Error(
            nom::error::Error::new(input, nom::error::ErrorKind::Tag),
        )),
    }
}