use crate::lexer::Token;
use nom::IResult;
pub type Tokens<'a> = &'a [Token];

pub fn match_token(expected: Token) -> impl Fn(Tokens) -> IResult<Tokens, Token> {
    move |input: Tokens| {
        match input.split_first() {
            Some((tok, rest)) if *tok == expected => Ok((rest, tok.clone())),
            _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
        }
    }
}