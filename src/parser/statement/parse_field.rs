use nom::Parser;
use nom::combinator::{cut, map, opt};
use nom::error::context;
use nom::IResult;
use crate::ast::ast::Field;
use crate::{expect_token, is_tok};
use crate::lexer::token::Token;
use crate::parser::literal::parse_identifier::parse_identifier;
use crate::parser::literal::parse_type::parse_type;
use crate::parser::parser::{match_token, TokenSlice};
use crate::parser::expr::parse_expression::parse_expression;

pub fn parse_field(input: TokenSlice) -> IResult<TokenSlice, Field> {
    let (input, dynamic) = match expect_token!(input.clone(), Static) {
        Ok((i, _)) => Ok((i, false)),
        Err(_) => Ok((input, true)),
    }?;

    let (input, _) = expect_token!(input, Val)?;

    let (input, (mutable, public, name, _, field_type, _, expr, _)) = cut((
        map(opt(is_tok!(Mut)), |o| o.is_some()),
        map(opt(is_tok!(Bang)), |o| o.is_some()),
        context("field name", parse_identifier),
        is_tok!(Colon),
        context("field type", parse_type),
        // = [expr];

        // map(opt(many0))
        is_tok!(Eq),
        context("expression", parse_expression),

        is_tok!(Semi),
    )).parse(input)?;
    println!("{:?}", expr);

    Ok((input, Field { name, field_type, public, mutable, dynamic }))
}