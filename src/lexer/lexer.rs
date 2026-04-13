use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, digit1, multispace0},
    combinator::recognize,
    multi::{many0, many1},
    sequence::{pair, terminated},
    IResult,
};
use nom::combinator::map;
use crate::lexer::token::Token;

fn skip_ws(input: &str) -> IResult<&str, &str> {
    multispace0(input)
}

fn lex_integer(input: &str) -> IResult<&str, Token> {
    // This returns a function, which is then immediately called with (input)
    map(digit1, |s: &str| Token::Int(s.parse().unwrap()))(input)
}