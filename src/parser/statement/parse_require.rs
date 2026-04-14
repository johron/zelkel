use nom::Parser;
use nom::combinator::cut;
use nom::IResult;
use crate::ast::ast::Statement;
use crate::{expect_token};
use crate::parser::parser::{TokenSlice};
use crate::parser::path::parse_path::parse_path;

pub fn parse_require(input: TokenSlice) -> IResult<TokenSlice, Statement> {
    let (input, _) = expect_token!(input, Require)?;
    let (input, require) = cut(parse_path).parse(input)?;
    let (input, _) = expect_token!(input, Semi)?;

    Ok((input, Statement::Require(require)))
}