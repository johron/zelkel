use nom::Parser;
use nom::IResult;
use crate::ast::ast::Function;
use crate::parser::parse_ident::parse_ident;
use crate::parser::parser::{TokenSlice};

use nom::combinator::{cut, map, opt};
use nom::error::context;
use crate::{expect_token, is_tok};

pub fn parse_function(input: TokenSlice) -> IResult<TokenSlice, Function> {
    // 1. Check for 'static' (Optional)
    let (input, is_static) = match expect_token!(input.clone(), Static) {
        Ok((i, _)) => (i, true),
        Err(_) => (input, false),
    };
    let dynamic = !is_static;

    // 2. Look for 'fn'. If this fails, alt() will try the next parser (like parse_class).
    let (input, _) = expect_token!(input, Fn)?;

    let (input, (public, name, _, _, _, type_name, _, _)) = cut((
        map(opt(is_tok!(Bang)), |o| o.is_some()),
        context("function name", parse_ident),
        is_tok!(LParen),
        is_tok!(RParen),
        is_tok!(Arrow),
        context("return type", parse_ident),
        is_tok!(LBrace),
        is_tok!(RBrace),
    )).parse(input)?;


    Ok((input, Function { name, public, dynamic }))
}
