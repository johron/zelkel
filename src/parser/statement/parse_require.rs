use nom::Parser;
use nom::combinator::cut;
use nom::IResult;
use crate::ast::ast::Statement;
use crate::{expect_token, is_tok};
use crate::parser::parser::{TokenSlice};
use crate::parser::path::parse_path::parse_path;

pub fn parse_require(input: TokenSlice) -> IResult<TokenSlice, Statement> {
    let (input, _) = expect_token!(input, Require)?;
    let (input, (require, _)) = cut((
        parse_path,
        is_tok!(Semi)
    )).parse(input)?;

    Ok((input, Statement::Require(require)))
}