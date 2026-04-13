use nom::IResult;
use nom::combinator::cut;
use crate::ast::ast::Function;
use crate::lexer::Token;
use crate::parser::parse_ident::parse_ident;
use crate::parser::parser::{match_token, TokenSlice};

pub fn parse_function(input: TokenSlice) -> IResult<TokenSlice, Function> {
    let (input, dynamic) = match match_token(Token::Static)(input.clone()) {
        Ok((i, _)) => Ok((i, false)),
        Err(_) => Ok((input, true)),
    }?;

    let (input, _) = match_token(Token::Fn)(input)?;

    let (input, public) = match match_token(Token::Bang)(input.clone()) {
        Ok((i, _)) => Ok((i, true)),
        Err(_) => Ok((input, false)),
    }?;

    let (input, name) = parse_ident(input)?;

    Ok((input, Function { name, public, dynamic }))
}