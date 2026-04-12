use nom::IResult;
use crate::ast::ast::Type;
use crate::lexer::Token;
use crate::parser::parse_ident::parse_ident;
use crate::parser::parser::{match_token, Tokens};

pub fn parse_type(input: Tokens) -> IResult<Tokens, Type> {
    if let Ok((input, _)) = match_token(Token::Star)(input) {
        let (input, inner) = parse_type(input)?;
        return Ok((input, Type::Pointer(Box::new(inner))));
    }

    let (input, name) = parse_ident(input)?;
    Ok((input, Type::Ident(name)))
}