use crate::error;
use crate::lexer::{Token, TokenValue};
use crate::parser::{expect, BinaryExpression, ComparisonExpression, Expression, ExpressionKind, PrimaryExpression, Scope, TermExpression, UnaryExpression, ValueType};

pub fn parse_primary_expression(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Expression, usize), String> {
    let mut i = *i;
    let tok = &toks[i];
    let expr = match &tok.value {
        TokenValue::Integer(_) | TokenValue::Float(_) | TokenValue::String(_) | TokenValue::Bool(_) => {
            let typ = match &tok.value {
                TokenValue::Integer(_) => ValueType::PrimitiveInteger,
                TokenValue::Float(_) => ValueType::PrimitiveFloat,
                TokenValue::String(_) => ValueType::PrimitiveString,
                TokenValue::Bool(_) => ValueType::PrimitiveBool,
                _ => unreachable!(),
            };
            Expression {
                kind: ExpressionKind::Primary(PrimaryExpression {
                    value: tok.value.clone().into(),
                    typ: typ.clone(),
                    pos: tok.pos.clone(),
                }),
                typ,
                pos: tok.pos.clone(),
            }
        },
        TokenValue::Punctuation(p) => {
            if p == "(" {
                i += 1;
                let (expr, j) = parse_expression(&i, toks, scope_stack)?;
                i = j;
                expect(&i, &toks, TokenValue::Punctuation(")".to_string()))?;
                expr
            } else {
                return Err(error("Expected a primary expression".to_string(), tok.pos.clone()));
            }
        }
        _ => return Err(error("Expected a primary expression".to_string(), tok.pos.clone())),
    };
    i += 1;
    Ok((expr, i))
}

pub fn parse_unary_expression(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Expression, usize), String> {
    let mut i = *i;
    let tok = &toks[i];
    if let TokenValue::Arithmetic(_) = &tok.value {
        i += 1;
        let (expr, j) = parse_primary_expression(&mut i, toks, scope_stack)?;

        if expr.typ == ValueType::PrimitiveInteger || expr.typ == ValueType::PrimitiveFloat {
            return Ok((Expression {
                kind: ExpressionKind::Unary(Box::from(UnaryExpression {
                    left: expr.kind,
                    typ: expr.typ.clone(),
                    op: tok.clone().value.into(),
                    pos: tok.pos.clone(),
                })),
                typ: expr.typ,
                pos: tok.pos.clone(),
            }, j));
        }

        return Err(error(format!("Type mismatch: expected {:?} or {:?}, but found {:?}", ValueType::PrimitiveInteger, ValueType::PrimitiveFloat, expr.typ), toks[i].pos.clone()));
    }
    parse_primary_expression(&mut i, toks, scope_stack)
}

pub fn parse_term_expression(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Expression, usize), String> {
    let mut i = *i;
    let (mut expr, j) = parse_unary_expression(&mut i, toks, scope_stack)?;
    i = j;
    while i < toks.len() {
        let tok = &toks[i];
        if let TokenValue::Arithmetic(op) = &tok.value {
            if op == "*" || op == "/" {
                i += 1;
                let (right, h) = parse_unary_expression(&mut i, toks, scope_stack)?;
                i = h;

                if expr.typ != right.typ {
                    return Err(error(format!("Type mismatch: expected {:?}, but found {:?}", expr.typ, right.typ), toks[i].pos.clone()));
                }

                expr = Expression {
                    kind: ExpressionKind::Term(Box::from(TermExpression {
                        left: expr.kind,
                        right: right.kind,
                        typ: expr.typ.clone(),
                        op: tok.clone().value.into(),
                        pos: tok.pos.clone(),
                    })),
                    typ: expr.typ,
                    pos: tok.pos.clone(),
                };
            } else {
                break;
            }
        } else {
            break;
        }
    }
    Ok((expr, i))
}

pub fn parse_binary_expression(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Expression, usize), String> {
    let mut i = *i;
    let (mut expr, j) = parse_term_expression(&mut i, toks, scope_stack)?;
    i = j;
    while i < toks.len() {
        let tok = &toks[i];
        if let TokenValue::Arithmetic(op) = &tok.value {
            if op == "+" || op == "-" || op == "%" {
                i += 1;
                let (right, h) = parse_term_expression(&mut i, toks, scope_stack)?;
                i = h;

                if expr.typ != right.typ {
                    return Err(error(format!("Type mismatch: expected {:?}, but found {:?}", expr.typ, right.typ), toks[i].pos.clone()));
                }

                expr = Expression {
                    kind: ExpressionKind::Binary(Box::from(BinaryExpression {
                        left: expr.kind,
                        right: right.kind,
                        typ: expr.typ.clone(),
                        op: tok.clone().value.into(),
                        pos: tok.pos.clone(),
                    })),
                    typ: expr.typ,
                    pos: tok.pos.clone(),
                };
            } else {
                break;
            }
        } else {
            break;
        }
    }
    Ok((expr, i))
}

pub fn parse_comparison_expression(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Expression, usize), String> {
    let mut i = *i;
    let (mut expr, j) = parse_binary_expression(&mut i, toks, scope_stack)?;
    i = j;
    while i < toks.len() {
        let tok = &toks[i];
        if let TokenValue::Arithmetic(op) = &tok.value {
            if op == "==" || op == "!=" || op == "<" || op == ">" || op == "<=" || op == ">=" {
                i += 1;
                let (right, h) = parse_binary_expression(&mut i, toks, scope_stack)?;
                i = h;

                if expr.typ != right.typ {
                    return Err(error(format!("Type mismatch: expected {:?}, but found {:?}", expr.typ, right.typ), toks[i].pos.clone()));
                }

                expr = Expression {
                    kind: ExpressionKind::Comparison(Box::from(ComparisonExpression {
                        left: expr.kind,
                        right: right.kind,
                        typ: expr.typ.clone(),
                        op: tok.clone().value.into(),
                        pos: tok.pos.clone(),
                    })),
                    typ: expr.typ,
                    pos: tok.pos.clone(),
                };
            } else {
                break;
            }
        } else {
            break;
        }
    }
    Ok((expr, i))
}

pub fn parse_expression(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Expression, usize), String> {
    parse_comparison_expression(i, toks, scope_stack)
}