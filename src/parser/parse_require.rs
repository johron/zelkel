use nom::IResult;
use crate::ast::ast::Item;
use crate::{expect_token, is_token};
use crate::parser::parse_ident::parse_ident;
use crate::parser::parser::{match_token, TokenSlice};

pub fn parse_require(input: TokenSlice) -> IResult<TokenSlice, Item> {
    let (input, _) = expect_token!(input, Require)?;
    let (input, name) = parse_ident(input)?;
    let (input, _) = expect_token!(input, Semi)?;

    Ok((input, Item::Require(name)))
}