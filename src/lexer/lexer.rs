use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, digit1, multispace0, none_of, char},
    combinator::{map, recognize},
    multi::{many0},
    sequence::{delimited, pair},
    IResult, Parser,
};
use nom::sequence::preceded;
use crate::lexer::token::Token;

pub fn lexer(input: &str) -> IResult<&str, Vec<Token>> {
    many0(preceded(
        multispace0,
        alt((
            lex_integer,
            lex_symbol,
            lex_ident_or_keyword,
            lex_string,
        ))
    )).parse(input)
}


fn skip_ws(input: &str) -> IResult<&str, &str> {
    multispace0(input)
}

fn lex_integer(input: &str) -> IResult<&str, Token> {
    // Instead of map(...)(input), use .parse(input)
    map(digit1, |s: &str| Token::Int(s.parse().unwrap())).parse(input)
}

fn lex_symbol(input: &str) -> IResult<&str, Token> {
    alt((
        map(tag("->"), |_| Token::Arrow),
        map(tag("+"), |_| Token::Plus),
        map(tag("-"), |_| Token::Minus),
        map(tag("*"), |_| Token::Star),
        map(tag("/"), |_| Token::Slash),
        map(tag("="), |_| Token::Eq),
        map(tag("&"), |_| Token::Ampersand),
        map(tag(";"), |_| Token::Semi),
        map(tag(":"), |_| Token::Colon),
        map(tag(","), |_| Token::Comma),
        map(tag("."), |_| Token::Dot),
        map(tag("{"), |_| Token::LBrace),
        map(tag("}"), |_| Token::RBrace),
        map(tag("("), |_| Token::LParen),
        map(tag(")"), |_| Token::RParen),
    )).parse(input)
}

fn lex_ident_or_keyword(input: &str) -> IResult<&str, Token> {
    let mut identifier = recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_"))))
    ));

    let (rest, id) = identifier.parse(input)?;

    let token = match id {
        "fn" => Token::Fn,
        "static" => Token::Static,
        "mut" => Token::Mut,
        "val" => Token::Val,
        "require" => Token::Require,
        "return" => Token::Return,
        _ => Token::Ident(id),
    };

    Ok((rest, token))
}

fn lex_string(input: &str) -> IResult<&str, Token> {
    // Simple string: double quotes containing anything except quotes or backslashes
    map(
        delimited(char('"'), many0(none_of("\"\\")), char('"')),

        |_| Token::Str(input) // Note: Simplified for context; typically use 'recognize'
    ).parse(input)
}