use nom::IResult;
use crate::ast::ast::{Require};
use crate::expect_token;
use crate::lexer::token::Token;
use crate::parser::literal::parse_identifier::parse_identifier;
use crate::parser::parser::{TokenSlice};

pub fn parse_path(input: TokenSlice) -> IResult<TokenSlice, Require> {
    // find identifier
    // if double colon or dot, find next identifier. If colon then new module or class, if dot then it's function

    let (input, mod_name) = parse_identifier(input)?;

    match input.tokens.first() {
        Some(Token::DoubleColon(_)) => {
            let (input, _) = expect_token!(input, DoubleColon)?;
            let (input, path) = parse_path(input)?;
            Ok((input, Require::Module(mod_name, Box::new(path))))
        }
        Some(Token::Dot(_)) => {
            let (input, _) = expect_token!(input, Dot)?;
            let (input, ident) = parse_identifier(input)?;
            Ok((input, Require::Module(mod_name, Box::new(
                Require::Identifier(ident)
            ))))
        }
        _ => Ok((input, Require::EndingModule(mod_name))), // Just a module without submodule or function
    }

}
