use nom::IResult;
use crate::lexer::Token;
use crate::parser::parser::Tokens;

pub fn parse_ident(input: Tokens) -> IResult<Tokens, String> {
    match input.split_first() {
        Some((Token::Ident(name), rest)) => Ok((rest, name.clone())),
        _ => Err(nom::Err::Error(
            nom::error::Error::new(input, nom::error::ErrorKind::Tag),
        )),
    }
}