use nom::IResult;
use crate::ast::ast::Item;
use crate::lexer::Token;
use crate::parser::parse_ident::parse_ident;
use crate::parser::parser::{match_token, Tokens};

pub fn parse_require(input: Tokens) -> IResult<Tokens, Item> {
    let (input, _) = match_token(Token::Require)(input)?;
    let (input, name) = parse_ident(input)?;
    let (input, _) = match_token(Token::Semi)(input)?;

    Ok((input, Item::Require(name)))
}