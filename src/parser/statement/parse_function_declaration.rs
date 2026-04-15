use nom::Parser;
use nom::IResult;
use crate::ast::ast::Function;
use crate::parser::literal::parse_identifier::parse_identifier;
use crate::parser::parser::{TokenSlice};

use nom::combinator::{cut, map, opt};
use nom::error::context;
use crate::{expect_token, is_tok};
use crate::parser::literal::parse_type::parse_type;

pub fn parse_function_declaration(input: TokenSlice) -> IResult<TokenSlice, Function> {
    let (input, is_static) = match expect_token!(input.clone(), Static) {
        Ok((i, _)) => (i, true),
        Err(_) => (input, false),
    };
    let dynamic = !is_static;

    let (input, _) = expect_token!(input, Fn)?;

    let (input, (public, name, _, _, _, return_type, _)) = cut((
        map(opt(is_tok!(Bang)), |o| o.is_some()),
        context("function name", parse_identifier),
        is_tok!(LParen),
        is_tok!(RParen),
        is_tok!(Arrow),
        context("return type", parse_type),
        is_tok!(LBrace),
    )).parse(input)?;

    let (input, _) = expect_token!(input, RBrace)?;

    Ok((input, Function { name, public, dynamic, return_type }))
}
