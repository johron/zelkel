use nom::IResult;
use crate::ast::ast::Type;
use crate::expect_token;
use crate::lexer::token::Token;
use crate::parser::parse_ident::parse_ident;
use crate::parser::parser::{match_token, TokenSlice};

pub fn parse_type(input: TokenSlice) -> IResult<TokenSlice, Type> {
    if let Ok((input, _)) = expect_token!(input.clone(), Star) {
        let (input, inner) = parse_type(input)?;
        return Ok((input, Type::Pointer(Box::new(inner))));
    }

    let (input, name) = parse_ident(input)?;
    Ok((input, Type::Ident(name)))
}