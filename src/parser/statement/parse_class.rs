use nom::combinator::{cut, map, opt};
use nom::error::context;
use nom::IResult;
use nom::multi::many0;
use nom::Parser;
use crate::ast::ast::{Class, Statement};
use crate::{expect_token, is_tok, is_token};
use crate::parser::statement::parse_field::parse_field;
use crate::parser::statement::parse_function_declaration::parse_function_declaration;
use crate::parser::literal::parse_identifier::parse_identifier;
use crate::parser::literal::parse_type::parse_type;
use crate::parser::parser::{match_token, TokenSlice};

pub fn parse_class(input: TokenSlice) -> IResult<TokenSlice, Statement> {
    let (input, dynamic) = match match_token(is_token!(Static))(input.clone()) {
        Ok((i, _)) => Ok((i, false)),
        Err(_) => Ok((input, true)),
    }?;

    let (input, _) = expect_token!(input, Class)?;

    let (input, (public, name, _)) = cut((
        map(opt(is_tok!(Bang)), |o| o.is_some()),
        context("class name", parse_identifier),
        is_tok!(LBrace),
    )).parse(input)?;

    // TODO: This might make the order strict, which I don't want. Now: Class: Fields -> Methods. I want: Class: (Fields | Methods)*
    let (input, fields) = (|i| many0(parse_field).parse(i))(input)?;
    let (input, methods) = (|i| many0(parse_function_declaration).parse(i))(input)?;

    let (input, _) = expect_token!(input, RBrace)?;

    Ok((
        input,
        Statement::ClassDeclaration(Class {
            name,
            fields,
            methods,
            dynamic,
            public,
        }),
    ))
}