use nom::IResult;
use crate::ast::ast::Literal;
use crate::lexer::token::{Token, Tokens};
use crate::parser::parser::{TokenSlice};

pub fn parse_integer(input: TokenSlice) -> IResult<TokenSlice, Literal> {
    match input.tokens.split_first() {
        Some((Token::Int(val, ..), rest)) => {
            let next_input = Tokens::new(rest);
            Ok((next_input, Literal::Integer(val.clone())))
        }
        _ => Err(nom::Err::Error(
            nom::error::Error::new(input, nom::error::ErrorKind::Tag),
        )),
    }
}
