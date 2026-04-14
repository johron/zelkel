use nom::IResult;
use nom::multi::many0;
use nom::Parser;
use crate::ast::ast::{Class, Statement};
use crate::{expect_token, is_token};
use crate::parser::statement::parse_field::parse_field;
use crate::parser::statement::parse_function_declaration::parse_function_declaration;
use crate::parser::literal::parse_identifier::parse_identifier;
use crate::parser::parser::{match_token, TokenSlice};

pub fn parse_class(input: TokenSlice) -> IResult<TokenSlice, Statement> {
    let (input, dynamic) = match match_token(is_token!(Static))(input.clone()) {
        Ok((i, _)) => Ok((i, false)),
        Err(_) => Ok((input, true)),
    }?;

    let (input, _) = expect_token!(input, Class)?;

    let (input, public) = match expect_token!(input.clone(), Class) {
        Ok((i, _)) => Ok((i, true)),
        Err(_) => Ok((input, false)),
    }?;

    let (input, name) = parse_identifier(input)?;
    let (input, _) = expect_token!(input, LBrace)?;

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