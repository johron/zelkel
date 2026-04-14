use nom::IResult;
use crate::lexer::token::{Token, Tokens};
use crate::parser::parser::{TokenSlice};

pub fn parse_identifier(input: TokenSlice) -> IResult<TokenSlice, String> {
    match input.tokens.split_first() {
        Some((Token::Ident(name, ..), rest)) => {
            let next_input = Tokens::new(rest);
            Ok((next_input, name.to_string()))
        }
        _ => Err(nom::Err::Error(
            nom::error::Error::new(input, nom::error::ErrorKind::Tag),
        )),
    }
}
