use crate::error;
use crate::lexer::{Token, TokenValue};
use crate::parser::{expect, BinaryExpression, ComparisonExpression, Expression, ExpressionKind, InstantiationExpression, PrimaryExpression, Scope, TermExpression, UnaryExpression, Value, ValueType};

pub fn parse_primary_expression(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Expression, usize), String> {
    let mut i = *i;
    let tok = &toks[i];
    let expr = match &tok.value {
        TokenValue::Integer(_) | TokenValue::Float(_) | TokenValue::String(_) | TokenValue::Bool(_) => {
            Expression {
                kind: ExpressionKind::Primary(PrimaryExpression {
                    value: tok.value.clone().into(),
                    typ: match &tok.value {
                        TokenValue::Integer(_) => ValueType::PrimitiveInteger,
                        TokenValue::Float(_) => ValueType::PrimitiveFloat,
                        TokenValue::String(_) => ValueType::PrimitiveString,
                        TokenValue::Bool(_) => ValueType::PrimitiveBool,
                        _ => unreachable!(),
                    },
                    pos: tok.pos.clone(),
                }),
                typ: match &tok.value {
                    TokenValue::Integer(_) => ValueType::PrimitiveInteger,
                    TokenValue::Float(_) => ValueType::PrimitiveFloat,
                    TokenValue::String(_) => ValueType::PrimitiveString,
                    TokenValue::Bool(_) => ValueType::PrimitiveBool,
                    _ => unreachable!(),
                },
                pos: tok.pos.clone(),
            }
        }
        TokenValue::Identifier(s) => {
            i += 1;
            if s == "Self" {
                expect(&i, &toks, TokenValue::Punctuation(".".to_string()), true)?;
                i += 1;
                let current_class = scope_stack.last().unwrap().current_class.clone().unwrap();
                let class = scope_stack.last().unwrap().classes.get(current_class.as_str()).unwrap();
                let name = expect(&i, &toks, TokenValue::empty("identifier")?, false)?.value.as_string();

                if class.functions.get(name.as_str()).is_some() {
                    i += 1;
                    expect(&i, &toks, TokenValue::Punctuation("(".to_string()), true)?;
                    i += 1;
                    let mut args: Vec<Expression> = Vec::new();
                    while i < toks.len() {
                        let tok = &toks[i];
                        if let TokenValue::Punctuation(p) = &tok.value {
                            if p == ")" {
                                break;
                            }
                        }
                        let mut scope = scope_stack.clone();
                        let (expr, j) = parse_expression(&i, toks, &mut scope)?;
                        i = j;
                        args.push(expr);
                        if let Ok(_) = expect(&i, &toks, TokenValue::Punctuation(",".to_string()), true) {
                            i += 1;
                        }
                    }
                    expect(&i, &toks, TokenValue::Punctuation(")".to_string()), true)?;
                    Expression {
                        kind: ExpressionKind::Primary(PrimaryExpression {
                            value: Value::Function(tok.value.as_string()),
                            typ: class.functions.get(name.clone().as_str()).unwrap().typ.clone(),
                            pos: tok.pos.clone(),
                        }),
                        typ: class.functions.get(name.as_str()).unwrap().typ.clone(),
                        pos: tok.pos.clone(),
                    }
                } else if class.variables.get(name.as_str()).is_some() {
                    Expression {
                        kind: ExpressionKind::Primary(PrimaryExpression {
                            value: Value::Variable(name.clone(), true),
                            typ: class.variables.get(name.clone().as_str()).unwrap().typ.clone(),
                            pos: tok.pos.clone(),
                        }),
                        typ: class.variables.get(name.as_str()).unwrap().typ.clone(),
                        pos: tok.pos.clone(),
                    }
                } else {
                    return Err(error(format!("Tried to reference an undeclared variable or function '{}'", name), tok.pos.clone()));
                }
            } else if s == "None" {
                i -= 1;
                Expression {
                    kind: ExpressionKind::Primary(PrimaryExpression {
                        value: Value::None,
                        typ: ValueType::None,
                        pos: tok.pos.clone(),
                    }),
                    typ: ValueType::None,
                    pos: tok.pos.clone(),
                }
            } else if let Ok(_) = expect(&i, &toks, TokenValue::Punctuation("(".to_string()), true) {
                i += 1;
                if let Some(f) = scope_stack.last().unwrap().classes.get(scope_stack.last().unwrap().current_class.as_ref().unwrap()).unwrap().functions.get(s) {
                    let mut args: Vec<Expression> = Vec::new();
                    while i < toks.len() {
                        let tok = &toks[i];
                        if let TokenValue::Punctuation(p) = &tok.value {
                            if p == ")" {
                                break;
                            }
                        }
                        let mut scope = scope_stack.clone();
                        let (expr, j) = parse_expression(&i, toks, &mut scope)?;
                        i = j;
                        args.push(expr);
                        if let Ok(_) = expect(&i, &toks, TokenValue::Punctuation(",".to_string()), true) {
                            i += 1;
                        }
                    }
                    expect(&i, &toks, TokenValue::Punctuation(")".to_string()), true)?;
                    Expression {
                        kind: ExpressionKind::Primary(PrimaryExpression {
                            value: Value::Function(tok.value.as_string()),
                            typ: f.clone().typ,
                            pos: tok.pos.clone(),
                        }),
                        typ: f.clone().typ,
                        pos: tok.pos.clone(),
                    }
                } else {
                    return Err(error(format!("Tried to call an undeclared function '{}'", s), tok.pos.clone()));
                }
            } else if s == "new" {
                // check if the next token is a class name that is declared
                let current_class = scope_stack.last().unwrap().current_class.clone().unwrap();
                let class_name = expect(&i, &toks, TokenValue::empty("identifier")?, false)?.value.as_string();
                i += 1;
                if scope_stack.last().unwrap().classes.get(class_name.as_str()).is_some() && current_class != class_name {
                    expect(&i, &toks, TokenValue::Punctuation("(".to_string()), true)?;
                    i += 1;
                    let mut args: Vec<Expression> = Vec::new();
                    while i < toks.len() {
                        let tok = &toks[i];
                        if let TokenValue::Punctuation(p) = &tok.value {
                            if p == ")" {
                                break;
                            }
                        }
                        let mut scope = scope_stack.clone();
                        let (expr, j) = parse_expression(&i, toks, &mut scope)?;
                        i = j;
                        args.push(expr);
                        if let Ok(_) = expect(&i, &toks, TokenValue::Punctuation(",".to_string()), true) {
                            i += 1;
                        }
                    }
                    expect(&i, &toks, TokenValue::Punctuation(")".to_string()), true)?;
                    Expression {
                        kind: ExpressionKind::Instantiation(InstantiationExpression {
                            class: class_name.clone(),
                            args,
                            pos: tok.pos.clone(),
                        }),
                        typ: ValueType::Class(class_name.clone()),
                        pos: tok.pos.clone(),
                    }
                } else {
                    return Err(error(format!("Tried to instantiate an undeclared class '{}'", class_name), tok.pos.clone()));
                }
            } else if let Some(v) = scope_stack.last().unwrap().variables.get(s) {
                let res = if let Ok(_) = expect(&i, &toks, TokenValue::Punctuation(".".to_string()), true) {
                    i += 1;
                    let sub = expect(&i, &toks, TokenValue::empty("identifier")?, false)?.value.as_string();
                    i += 1;
                    todo!("implement member access, very hard, help, help, help");
                } else {
                    i -= 1;
                    Expression {
                        kind: ExpressionKind::Primary(PrimaryExpression {
                            value: Value::Variable(tok.value.as_string(), false),
                            typ: v.typ.clone(),
                            pos: tok.pos.clone(),
                        }),
                        typ: v.typ.clone(),
                        pos: tok.pos.clone(),
                    }
                };
                res
            } else {
                return Err(error(format!("Tried to reference an undeclared variable '{}'", s), tok.pos.clone()));
            }
        },
        TokenValue::Punctuation(p) => {
            if p == "(" {
                i += 1;
                let (expr, j) = parse_expression(&i, toks, scope_stack)?;
                i = j;
                expect(&i, &toks, TokenValue::Punctuation(")".to_string()), true)?;
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