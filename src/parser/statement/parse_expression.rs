use crate::ast::ast::{Expression, Literal, Operator};
use crate::parser::literal::parse_identifier::parse_identifier;
use crate::parser::parser::{TokenSlice};

use nom::{IResult, Parser};
use nom::branch::alt;
use nom::combinator::map;
use nom::sequence::delimited;
use crate::is_tok;
use crate::lexer::token::{Token, Tokens};
use crate::parser::literal::parse_integer::parse_integer;
use crate::parser::literal::parse_type::parse_type;

fn get_precedence(t: &Token) -> i8 {
    match t {
        Token::Plus(..) | Token::Minus(..) => 1,
        Token::Star(..) | Token::Slash(..) => 2,
        _ => -1,
    }
}

pub fn parse_expression(input: TokenSlice) -> IResult<TokenSlice, Expression> {
    parse_expr_precedence(input, 0)
}

fn parse_expr_precedence(input: TokenSlice, min_precedence: i8) -> IResult<TokenSlice, Expression> {
    let (mut input, mut left) = parse_atom(input)?;

    loop {
        let (peek_input, token) = match input.tokens.split_first() {
            Some((t, rest)) => (rest, t),
            None => break,
        };

        let prec = get_precedence(token);
        if prec < min_precedence {
            break;
        }

        input = Tokens::new(peek_input);
        let op = match token {
            Token::Plus(..) => Operator::Addition,
            Token::Minus(..) => Operator::Subtraction,
            Token::Star(..) => Operator::Multiplication,
            Token::Slash(..) => Operator::Division,
            _ => unreachable!(),
        };

        // 3. Recursively parse the right side with higher precedence
        let (next_input, right) = parse_expr_precedence(input, prec + 1)?;
        input = next_input;

        // Construct the binary tree node
        left = Expression::Binary {
            left: Box::new(left),
            right: Box::new(right),
            op,
            ty: None, // TODO: Type eval in ast walk semantic eval
        };
    }

    Ok((input, left))
}

fn parse_atom(input: TokenSlice) -> IResult<TokenSlice, Expression> {
    alt((
        map(parse_integer, |n| Expression::Literal { val: n, ty: None }),
        map(parse_identifier, |s| Expression::Literal { val: Literal::Variable(s), ty: None }), // TODO: this needs work, especially with the type, we don't know the type at this time, we will know it at ast walk semantic eval.
        delimited(is_tok!(LParen), parse_expression, is_tok!(RParen))
    )).parse(input)
}
