use nom::IResult;
use crate::ast::ast::Type;
use crate::{expect_token, is_tok};
use crate::lexer::token::Token;
use crate::parser::literal::parse_identifier::parse_identifier;
use crate::parser::literal::parse_integer::parse_integer;
use crate::parser::parser::{match_token, TokenSlice};

pub fn parse_type(input: TokenSlice) -> IResult<TokenSlice, Type> {
    if let Ok((input, _)) = expect_token!(input.clone(), Star) {
        let (input, inner) = parse_type(input)?;
        return Ok((input, Type::Pointer(Box::new(inner))));
    }

    let (input, name) = parse_identifier(input)?;
    
    if let Ok((input, _)) = expect_token!(input.clone(), LBrace) {
        if let Ok(_) = expect_token!(input.clone(), RBrace) {
        } else {
            let (input, n) = parse_integer(input)?;
            let (input, _) = expect_token!(input, RBrace)?;
            return Ok((input, Type::SizeIdent(name, n.to_usize())))
        }
    }
    
    Ok((input, Type::Ident(name)))
}