use nom::IResult;
use crate::ast::ast::Function;
use crate::lexer::Token;
use crate::parser::parse_ident::parse_ident;
use crate::parser::parser::{match_token, Tokens};

fn parse_dyn_function(input: Tokens) -> IResult<Tokens, Function> {
    let (input, dynamic) = match match_token(Token::Static)(input) {
        Ok((i, _)) => Ok((i, false)),
        Err(_) => Ok((input, true)),
    }?;
    
    let (input, _) = match_token(Token::Fn)(input)?;

    let (input, public) = match match_token(Token::Bang)(input) {
        Ok((i, _)) => Ok((i, true)),
        Err(_) => Ok((input, false)),
    }?;

    let (input, name) = parse_ident(input)?;

    Ok((input, Function { name, public, dynamic }))
}