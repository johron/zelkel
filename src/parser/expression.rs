use crate::error;
use crate::lexer::{Token, TokenValue};
use crate::parser::{expect, expect_unstrict, BinaryExpression, ComparisonExpression, Expression, ExpressionKind, InstantiationExpression, PrimaryExpression, Scope, TermExpression, UnaryExpression, Value, ValueType};

fn parse_instantiation_expression(i: usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>, expected_type: &ValueType) -> Result<(Expression, usize), String> {
    let mut i = i;
    let begin = toks[i].pos.clone();
    
    expect(&i, &toks, TokenValue::Identifier("new".to_string()))?;
    i += 1;
    let name = expect_unstrict(&i, &toks, TokenValue::empty("identifier")?)?.value.as_string();
    if name == "Root" {
        return Err(error("Instantiation of 'Root' class is not allowed".to_string(), toks[i].pos.clone()));
    }

    i += 1;
    expect(&i, &toks, TokenValue::Punctuation("(".to_string()))?;
    i += 1;
    
    let mut args = Vec::new();
    let mut count = 0;
    while i < toks.len() && toks[i].value != TokenValue::Punctuation(")".to_string()) {
        let arg_type = scope_stack.last().unwrap().classes.get(name.as_str()).unwrap().functions.get("_").unwrap().args.values().nth(count).unwrap().typ.clone();
        let (arg, j) = parse_expression(&i, toks, scope_stack, &arg_type)?;
        args.push(arg);
        i = j;
        if i < toks.len() && toks[i].value == TokenValue::Punctuation(",".to_string()) {
            i += 1;
        }
        count += 1;
    }
    
    expect(&i, &toks, TokenValue::Punctuation(")".to_string()))?;

    let class = Expression {
        kind: ExpressionKind::Instantiation(InstantiationExpression {
            class: name.clone(),
            args,
            pos: begin.clone(),
        }),
        typ: ValueType::Class(name.clone()),
        pos: begin.clone(),
    };

    Ok((class, i))
}

pub fn parse_primary_expression(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>, expected_type: &ValueType) -> Result<(Expression, usize), String> {
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
                let (expr, j) = parse_expression(&i, toks, scope_stack, expected_type)?;
                i = j;
                expect(&i, &toks, TokenValue::Punctuation(")".to_string()))?;
                expr
            } else {
                return Err(error("Expected a primary expression".to_string(), tok.pos.clone()));
            }
        },
        TokenValue::Identifier(s) => {
            if s == "new" {
                let (expr, j) = parse_instantiation_expression(i, toks, scope_stack, expected_type)?;
                i = j;
                expr
            } else if s == "this" {
                expect(&i, &toks, TokenValue::Identifier("this".to_string()))?;
                i += 1;
                expect(&i, &toks, TokenValue::Punctuation(".".to_string()))?;
                i += 1;
                let member = expect_unstrict(&i, &toks, TokenValue::empty("identifier")?)?.value.as_string(); // TODO: multiple members should be allowed, e.g. this.member1.member2...

                let current_class = scope_stack.last().unwrap().current_class.clone();
                if current_class.variables.contains_key(&member) {
                    let var = current_class.variables.get(&member).unwrap();
                    Expression {
                        kind: ExpressionKind::Primary(PrimaryExpression {
                            value: Value::ClassVariable(member.clone()),
                            typ: var.typ.clone(),
                            pos: tok.pos.clone(),
                        }),
                        typ: var.typ.clone(),
                        pos: tok.pos.clone(),
                    }
                } else if current_class.functions.contains_key(&member) {
                    let func = current_class.functions.get(&member).unwrap();
                    expect(&i, &toks, TokenValue::Punctuation("(".to_string()))?;
                    i += 1;
                    let mut args = Vec::new();
                    while i < toks.len() && toks[i].value != TokenValue::Punctuation(")".to_string()) {
                        let (arg, j) = parse_expression(&i, toks, scope_stack, expected_type)?;
                        args.push(arg);
                        i = j;
                        if i < toks.len() && toks[i].value == TokenValue::Punctuation(",".to_string()) {
                            i += 1;
                        }
                    }
                    expect(&i, &toks, TokenValue::Punctuation(")".to_string()))?;

                    if args.len() != func.args.len() {
                        return Err(error(format!("Function '{}' expects {} arguments, but got {}", member, func.args.len(), args.len()), tok.pos.clone()));
                    }
                    for (arg, expected) in args.iter().zip(&func.args) {
                        if arg.typ != expected.1.typ {
                            return Err(error(format!("Type mismatch: expected {:?}, but found {:?}", expected.1.typ, arg.typ), tok.pos.clone()));
                        }
                    }

                    Expression {
                        kind: ExpressionKind::Primary(PrimaryExpression {
                            value: Value::ClassFunction(member.clone()),
                            typ: func.typ.clone(),
                            pos: tok.pos.clone(),
                        }),
                        typ: func.typ.clone(),
                        pos: tok.pos.clone(),
                    }
                } else {
                    return Err(error(format!("'this' does not have a member named '{}'", member), tok.pos.clone()));
                }
            } else if scope_stack.last().unwrap().variables.contains_key(s) {
                let var = scope_stack.last().unwrap().variables.get(s).unwrap();
                Expression {
                    kind: ExpressionKind::Primary(PrimaryExpression {
                        value: Value::Variable(s.clone()),
                        typ: var.typ.clone(),
                        pos: tok.pos.clone(),
                    }),
                    typ: var.typ.clone(),
                    pos: tok.pos.clone(),
                }
            } else if scope_stack.last().unwrap().functions.contains_key(s) {
                todo!("Function calls are not yet implemented"); // Should only allow function calls that return a value
            } else {
                return Err(error("Expected a primary expression".to_string(), tok.pos.clone()));
            }
        }
        _ => return Err(error("Expected a primary expression".to_string(), tok.pos.clone())),
    };
    i += 1;
    Ok((expr, i))
}

pub fn parse_unary_expression(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>, expected_type: &ValueType) -> Result<(Expression, usize), String> {
    let mut i = *i;
    let tok = &toks[i];
    if let TokenValue::Arithmetic(_) = &tok.value {
        i += 1;
        let (expr, j) = parse_primary_expression(&mut i, toks, scope_stack, expected_type)?;

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
    parse_primary_expression(&mut i, toks, scope_stack, expected_type)
}

pub fn parse_term_expression(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>, expected_type: &ValueType) -> Result<(Expression, usize), String> {
    let mut i = *i;
    let (mut expr, j) = parse_unary_expression(&mut i, toks, scope_stack, expected_type)?;
    i = j;
    while i < toks.len() {
        let tok = &toks[i];
        if let TokenValue::Arithmetic(op) = &tok.value {
            if op == "*" || op == "/" {
                i += 1;
                let (right, h) = parse_unary_expression(&mut i, toks, scope_stack, expected_type)?;
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

pub fn parse_binary_expression(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>, expected_type: &ValueType) -> Result<(Expression, usize), String> {
    let mut i = *i;
    let (mut expr, j) = parse_term_expression(&mut i, toks, scope_stack, expected_type)?;
    i = j;
    while i < toks.len() {
        let tok = &toks[i];
        if let TokenValue::Arithmetic(op) = &tok.value {
            if op == "+" || op == "-" || op == "%" {
                i += 1;
                let (right, h) = parse_term_expression(&mut i, toks, scope_stack, expected_type)?;
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

pub fn parse_comparison_expression(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>, expected_type: &ValueType) -> Result<(Expression, usize), String> {
    let mut i = *i;
    let (mut expr, j) = parse_binary_expression(&mut i, toks, scope_stack, expected_type)?;
    i = j;
    while i < toks.len() {
        let tok = &toks[i];
        if let TokenValue::Arithmetic(op) = &tok.value {
            if op == "==" || op == "!=" || op == "<" || op == ">" || op == "<=" || op == ">=" {
                i += 1;
                let (right, h) = parse_binary_expression(&mut i, toks, scope_stack, expected_type)?;
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

fn convert_primitive_to_class_instantiation(mut expr: Expression, expected_type: &ValueType) -> Result<Expression, String> {
    // No må eg finne ut hvilke klasse som skal instansieres basert på expected_type
    // og så lage en InstantiationExpression med den klassen og den primitive verdien som argument.
    match expected_type {
        ValueType::Class(class_name) => {
            if (class_name == "Integer" && expr.typ == ValueType::PrimitiveInteger)
                || (class_name == "Float" && expr.typ == ValueType::PrimitiveFloat)
                || (class_name == "String" && expr.typ == ValueType::PrimitiveString)
                || (class_name == "Bool" && expr.typ == ValueType::PrimitiveBool)
            {
                expr.kind = ExpressionKind::Instantiation(InstantiationExpression {
                    class: class_name.clone(),
                    args: vec![expr.clone()],
                    pos: expr.pos.clone(),
                });
                expr.typ = ValueType::Class(class_name.clone());
                Ok(expr)
            } else {
                Err(error(format!("Cannot convert primitive type {:?} to class instantiation of {:?}", expr.typ, expected_type), expr.pos.clone()))
            }
        },
        _ => {
            Err(error(format!("Cannot convert primitive type to class instantiation: expected {:?}", expected_type), expr.pos.clone()))
        }
    }
}

pub fn parse_expression(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>, expected_type: &ValueType) -> Result<(Expression, usize), String> {
    let cmp = parse_comparison_expression(i, toks, scope_stack, &expected_type);
    if cmp.is_ok() {
        let (expr, j) = cmp?;
        let typ = expr.typ.clone();
        if typ != *expected_type && expected_type != &ValueType::None {
            return match typ {
                ValueType::PrimitiveInteger | ValueType::PrimitiveFloat | ValueType::PrimitiveString | ValueType::PrimitiveBool => {
                    let convert = convert_primitive_to_class_instantiation(expr.clone(), expected_type);
                    if convert.is_ok() {
                        Ok((convert?, j))
                    } else {
                        Err(error(format!("Type mismatch: expected {:?}, but found {:?}", expected_type, typ), toks[*i].pos.clone()))
                    }
                }
                _ => Err(error(format!("Type mismatch: expected {:?}, but found {:?}", expected_type, typ), toks[*i].pos.clone()))
            }
        }
        Ok((expr, j))
    } else {
        cmp
    }
}