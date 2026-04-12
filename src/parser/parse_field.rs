use nom::IResult;
use crate::ast::ast::Field;
use crate::lexer::Token;
use crate::parser::parse_ident::parse_ident;
use crate::parser::parse_type::parse_type;
use crate::parser::parser::{match_token, Tokens};

pub fn parse_field(input: Tokens) -> IResult<Tokens, Field> {
    let (input, dynamic) = match match_token(Token::Static)(input) {
        Ok((i, _)) => Ok((i, false)),
        Err(_) => Ok((input, true)),
    }?;

    let (input, _) = match_token(Token::Val)(input)?;

    let (input, mutable) = match match_token(Token::Mut)(input) {
        Ok((i, _)) => Ok((i, true)),
        Err(_) => Ok((input, false)),
    }?;

    let (input, public) = match match_token(Token::Bang)(input) {
        Ok((i, _)) => Ok((i, true)),
        Err(_) => Ok((input, false)),
    }?;

    let (input, name) = parse_ident(input)?;
    let (input, _) = match_token(Token::Colon)(input)?;
    let (input, ty) = parse_type(input)?;
    let (input, _) = match_token(Token::Semi)(input)?;

    Ok((input, Field { name, ty, public, mutable, dynamic }))
}