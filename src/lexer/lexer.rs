use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, digit1, multispace0, none_of, char},
    combinator::{map, recognize},
    multi::{many0},
    sequence::{delimited, pair},
    IResult, Parser,
};
use nom::sequence::{preceded, terminated};
use crate::lexer::token::Token;

use nom::combinator::all_consuming;

pub fn lexer(input: &str) -> IResult<&str, Vec<Token>> {
    all_consuming(
        terminated(
            many0(preceded(multispace0, |i| alt_tokens(i, input.clone()))), // i, src
            multispace0
        )
    ).parse(input)
}

fn alt_tokens<'a>(input: &'a str, src: &str) -> IResult<&'a str, Token<'a>> {
    let offset = src.len() - input.len();

    alt((
        |i| lex_integer(i, offset),
        |i| lex_symbol(i, offset),
        |i| lex_ident_or_keyword(i, offset),
        |i| lex_string(i, offset),
    )).parse(input)
}

fn lex_integer(input: &str, offset: usize) -> IResult<&str, Token> {
    map(digit1, |s: &str| Token::Int(s.parse().unwrap(), offset)).parse(input)
}

fn lex_symbol(input: &str, offset: usize) -> IResult<&str, Token> {
    alt((
        map(tag("->"), |_| Token::Arrow(offset)),
        map(tag("::"), |_| Token::DoubleColon(offset)),
        map(tag("+"), |_| Token::Plus(offset)),
        map(tag("-"), |_| Token::Minus(offset)),
        map(tag("*"), |_| Token::Star(offset)),
        map(tag("/"), |_| Token::Slash(offset)),
        map(tag("="), |_| Token::Eq(offset)),
        map(tag("&"), |_| Token::Ampersand(offset)),
        map(tag("!"), |_| Token::Bang(offset)),
        map(tag(";"), |_| Token::Semi(offset)),
        map(tag(":"), |_| Token::Colon(offset)),
        map(tag(","), |_| Token::Comma(offset)),
        map(tag("."), |_| Token::Dot(offset)),
        map(tag("{"), |_| Token::LBrace(offset)),
        map(tag("}"), |_| Token::RBrace(offset)),
        map(tag("("), |_| Token::LParen(offset)),
        map(tag(")"), |_| Token::RParen(offset)),
    )).parse(input)
}

fn lex_ident_or_keyword(input: &str, offset: usize) -> IResult<&str, Token> {
    let mut identifier = recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_"))))
    ));

    let (rest, id) = identifier.parse(input)?;

    let token = match id {
        "fn" => Token::Fn(offset),
        "class" => Token::Class(offset),
        "static" => Token::Static(offset),
        "mut" => Token::Mut(offset),
        "val" => Token::Val(offset),
        "path" => Token::Require(offset),
        "return" => Token::Return(offset),
        "require" => Token::Require(offset),
        _ => Token::Ident(id, offset),
    };

    Ok((rest, token))
}

fn lex_string(input: &str, offset: usize) -> IResult<&str, Token> {
    map(
        delimited(char('"'), many0(none_of("\"\\")), char('"')),

        |_| Token::Str(input, offset) // Note: Simplified for context; typically use 'recognize'
    ).parse(input)
}