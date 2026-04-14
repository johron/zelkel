use nom::IResult;
use crate::ast::ast::Field;
use crate::expect_token;
use crate::lexer::token::Token;
use crate::parser::parse_ident::parse_ident;
use crate::parser::parse_type::parse_type;
use crate::parser::parser::{match_token, TokenSlice};

pub fn parse_field(input: TokenSlice) -> IResult<TokenSlice, Field> {
    let (input, dynamic) = match expect_token!(input.clone(), Static) {
        Ok((i, _)) => Ok((i, false)),
        Err(_) => Ok((input, true)),
    }?;

    let (input, _) = expect_token!(input, Val)?;

    let (input, mutable) = match expect_token!(input.clone(), Mut) {
        Ok((i, _)) => Ok((i, true)),
        Err(_) => Ok((input, false)),
    }?;

    let (input, public) = match expect_token!(input.clone(), Bang) {
        Ok((i, _)) => Ok((i, true)),
        Err(_) => Ok((input, false)),
    }?;

    let (input, name) = parse_ident(input)?;
    let (input, _) = expect_token!(input, Colon)?;
    let (input, ty) = parse_type(input)?;
    let (input, _) = expect_token!(input, Semi)?;

    Ok((input, Field { name, ty, public, mutable, dynamic }))
}