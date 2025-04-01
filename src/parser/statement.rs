use std::collections::HashMap;
use crate::error;
use crate::lexer::{Token, TokenValue};
use crate::parser::{enter_scope, exit_scope, expect, parse_type, ClassDeclaration, ClassOptions, ConditionalStatement, ExpressionStatement, FunctionDeclaration, FunctionOptions, Scope, Statement, StatementKind, ValueType, VariableDeclaration, VariableOptions, VariableReassignment};
use crate::parser::expression::parse_expression;

fn parse_if_statement(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Statement, usize, Vec<Scope>), String> { // Conditional Statement
    let mut i = *i;
    let begin = i;
    let mut scope_stack = scope_stack.clone();
    i += 1;

    let (condition, j) = parse_expression(&i, &toks, &mut scope_stack)?;
    i = j;

    expect(&i, &toks, TokenValue::Punctuation("{".to_string()), true)?;
    i += 1;
    scope_stack = enter_scope(&mut scope_stack);
    let (body, j, mut scope) = parse_body(&i, &toks, &mut scope_stack)?;
    i = j;
    expect(&i, &toks, TokenValue::Punctuation("}".to_string()), true)?;
    i += 1;
    scope_stack = exit_scope(&mut scope);

    let else_body: Option<Vec<Statement>>;
    if let Ok(_) = expect(&i, &toks, TokenValue::Identifier("else".to_string()), true) {
        i += 1;
        if let Ok(_) = expect(&i, &toks, TokenValue::Identifier("if".to_string()), true) {
            let (else_if_stmt, j, scope) = parse_if_statement(&i, &toks, &mut scope_stack)?;
            scope_stack = scope;
            i = j;
            else_body = Some(vec![else_if_stmt]);
        } else {
            expect(&i, &toks, TokenValue::Punctuation("{".to_string()), true)?;
            i += 1;
            scope_stack = enter_scope(&mut scope_stack);
            let (else_body_inner, j, mut scope) = parse_body(&i, &toks, &mut scope_stack)?;
            i = j;
            expect(&i, &toks, TokenValue::Punctuation("}".to_string()), true)?;
            i += 1;
            scope_stack = exit_scope(&mut scope);
            else_body = Some(else_body_inner);
        }
    } else {
        else_body = None;
    }

    Ok((Statement {
        kind: StatementKind::ConditionalStatement(ConditionalStatement {
            condition,
            body,
            else_body,
            pos: toks[begin].pos.clone(),
        }),
        pos: toks[begin].pos.clone(),
    }, i, scope_stack))
}

fn parse_body(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Vec<Statement>, usize, Vec<Scope>), String> {
    let mut i = *i;
    let mut scope_stack = scope_stack.clone();
    let mut body: Vec<Statement> = Vec::new();
    let current_class = scope_stack.last().unwrap().current_class.clone().unwrap();

    while i < toks.len() {
        let tok = &toks[i];
        if let TokenValue::Punctuation(p) = &tok.value {
            if p == "}" {
                break;
            } else if p == "{" {
                i += 1;
                scope_stack = enter_scope(&mut scope_stack);
                let (nested_body, j, mut scope) = parse_body(&i, toks, &mut scope_stack)?;
                i = j;
                scope_stack = exit_scope(&mut scope);
                body.push(Statement {
                    kind: StatementKind::Block(nested_body),
                    pos: tok.pos.clone(),
                });
                continue;
            }
        }

        let (stmt, j, scope) = match &tok.value {
            TokenValue::Identifier(s) => match s.as_str() {
                "val" => parse_variable_declaration(&i, toks, &mut scope_stack, false),
                "if" => parse_if_statement(&i, &toks, &mut scope_stack),
                "Self" => parse_class_statement(&i, toks, &mut scope_stack),
                _ => {
                    if let Some(_) = scope_stack.last().unwrap().variables.get(s) {
                        parse_variable_reassignment(&i, toks, &mut scope_stack)
                    } else if let Some(_) = scope_stack.last().unwrap().classes.get(current_class.as_str()).unwrap().functions.get(s) {
                        parse_expression_statement(&i, toks, &mut scope_stack)
                    } else if let Some(_) = scope_stack.last().unwrap().classes.get(s) {
                        parse_class_statement(&i, toks, &mut scope_stack)
                    } else {
                        Err(error(format!("Unknown identifier: '{}', maybe undeclared variable or function", s), toks[i].clone().pos))
                    }
                },
            },
            _ => Err(error("Expected an identifier while parsing identifier".to_string(), toks[i].clone().pos)),
        }?;

        i = j;
        body.push(stmt);
        scope_stack = scope;
    }
    Ok((body, i, scope_stack))
}

fn parse_class_body(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Vec<Statement>, usize, Vec<Scope>), String> {
    let mut i = *i;
    let mut scope_stack = scope_stack.clone();
    let mut body: Vec<Statement> = Vec::new();

    while i < toks.len() {
        let tok = &toks[i];
        if let TokenValue::Punctuation(p) = &tok.value {
            if p == "}" {
                break;
            }
        }

        let (stmt, j, scope) = match &tok.value {
            TokenValue::Identifier(s) => match s.as_str() {
                "fn" => {
                    parse_function_declaration(&i, toks, &mut scope_stack)
                },
                "val" => {
                    parse_variable_declaration(&i, toks, &mut scope_stack, true)
                },
                _ => return Err(error(format!("Unknown identifier: '{}', maybe undeclared variable or function", s), toks[i].clone().pos.clone())),
            },
            _ => return Err(error("Expected an identifier while parsing identifier".to_string(), toks[i].clone().pos)),
        }?;

        i = j;
        body.push(stmt);
        scope_stack = scope;
    }

    Ok((body, i, scope_stack))
}
pub(crate) fn parse_class_declaration(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Statement, usize, Vec<Scope>), String> {
    let mut i = *i;
    let begin = i;
    let mut scope_stack = scope_stack.clone();
    i += 1;

    let public = if let Ok(_) = expect(&i, &toks, TokenValue::Identifier("pub".to_string()), true) {
        i += 1;
        true
    } else {
        false
    };

    let name = expect(&i, &toks, TokenValue::empty("identifier")?, false)?.value.as_string();
    if scope_stack.last().unwrap().classes.iter().any(|c| c.0 == &name) {
        return Err(error(format!("Class '{}' already declared", name), toks[i].pos.clone()));
    }

    if name == "Self" {
        return Err(error("Class name cannot be 'Self'".to_string(), toks[i].pos.clone()));
    }

    i += 1;
    let extends: Option<String> = if let Ok(_) = expect(&i, &toks, TokenValue::Punctuation(":".to_string()), true) {
        i += 1;
        let inherit = expect(&i, &toks, TokenValue::empty("identifier")?, false)?.value.as_string();
        if scope_stack.last().unwrap().classes.get(&inherit.clone()).is_none() {
            return Err(error(format!("Cannot inherit from an undeclared class '{}'", inherit), toks[i].pos.clone()));
        }
        if inherit == name {
            return Err(error(format!("Class '{}' cannot inherit from itself", name), toks[i].pos.clone()));
        }
        i += 1;
        Some(inherit)
    } else {
        None
    };

    expect(&i, &toks, TokenValue::Punctuation("{".to_string()), true)?;
    i += 1;
    scope_stack.last_mut().unwrap().classes.insert(name.clone(), ClassOptions {
        functions: HashMap::new(),
        variables: HashMap::new(),
        public,
    });
    scope_stack = enter_scope(&mut scope_stack);
    scope_stack.last_mut().unwrap().current_class = Some(name.clone());

    let (body, j, mut scope) = parse_class_body(&i, &toks, &mut scope_stack)?;
    i = j;
    scope_stack = exit_scope(&mut scope);
    expect(&i, &toks, TokenValue::Punctuation("}".to_string()), true)?;
    i += 1;

    Ok((Statement {
        kind: StatementKind::ClassDeclaration(ClassDeclaration {
            name,
            extends,
            body,
            pos: toks[begin].pos.clone(),
        }),
        pos: toks[begin].pos.clone(),
    }, i, scope_stack))
}

fn parse_declaration_arguments(i: &usize, toks: &Vec<Token>, scope_stack: &Vec<Scope>, constructor: bool) -> Result<(Vec<VariableOptions>, usize, Vec<Scope>), String> {
    let mut i = *i;
    let mut args: Vec<VariableOptions> = Vec::new();
    let mut first = true;
    let mut scope_stack = scope_stack.clone();
    let current_class = scope_stack.last().unwrap().current_class.clone();

    while i < toks.len() {
        if first && constructor {
            first = false;
            expect(&i, toks, TokenValue::Identifier("Self".to_string()), true)?;
            i += 1;
            args.push(VariableOptions {
                mutable: false,
                typ: ValueType::Class(current_class.clone().unwrap()),
            });
            if let Ok(_) = expect(&i, &toks, TokenValue::Punctuation(",".to_string()), true) {
                i += 1;
                continue;
            } else {
                break;
            }
        }
        let tok = &toks[i];
        if let TokenValue::Identifier(_) = &tok.value {
            i += 1;
            expect(&i, &toks, TokenValue::Punctuation(":".to_string()), true)?;
            i += 1;
            let type_ident = expect(&i, &toks, TokenValue::empty("identifier")?, false)?;
            let typ = parse_type(&type_ident, &scope_stack)?;
            i += 1;
            args.push(VariableOptions {
                mutable: false,
                typ: typ.clone(),
            });
            scope_stack.last_mut().unwrap().variables.insert(tok.value.as_string(), VariableOptions {
                mutable: false,
                typ: typ.clone(),
            });
            if let Ok(_) = expect(&i, &toks, TokenValue::Punctuation(",".to_string()), true) {
                i += 1;
            } else {
                break;
            }
        } else {
            break;
        }
    }
    Ok((args, i, scope_stack))
}

pub(crate) fn parse_function_declaration(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Statement, usize, Vec<Scope>), String> {
    let mut i = *i;
    let begin = i;
    let mut scope_stack = scope_stack.clone();
    let current_class = scope_stack.last().unwrap().current_class.clone().unwrap();
    i += 1;

    let public = if let Ok(_) = expect(&i, &toks, TokenValue::Identifier("pub".to_string()), true) {
        i += 1;
        true
    } else {
        false
    };

    let name = expect(&i, &toks, TokenValue::empty("identifier")?, false)?.value.as_string();
    if scope_stack.last().unwrap().classes.get(&current_class).unwrap().functions.get(&name).is_some() {
        return Err(error(format!("Function '{}' already declared", name.clone()), toks[i].pos.clone()));
    }

    i += 1;
    expect(&i, &toks, TokenValue::Punctuation("(".to_string()), true)?;
    i += 1;
    let constructor = if name == "_" { true } else { false };
    let (args, j, scope) = parse_declaration_arguments(&i, &toks, &scope_stack, constructor)?;
    scope_stack = scope;
    i = j;
    expect(&i, &toks, TokenValue::Punctuation(")".to_string()), true)?;
    i += 1;

    let typ: ValueType;
    if let Ok(_) = expect(&i, &toks, TokenValue::Punctuation("->".to_string()), true) {
        i += 1;
        typ = parse_type(&expect(&i, &toks, TokenValue::empty("identifier")?, false)?, &scope_stack)?;
        i += 1;
    } else {
        typ = ValueType::None;
    }
    expect(&i, &toks, TokenValue::Punctuation("{".to_string()), true)?;
    i += 1;
    scope_stack = enter_scope(&mut scope_stack);
    let (body, j, mut scope) = parse_body(&i, &toks, &mut scope_stack)?;
    i = j;
    expect(&i, &toks, TokenValue::Punctuation("}".to_string()), true)?;
    i += 1;
    scope_stack = exit_scope(&mut scope);

    scope_stack.last_mut().unwrap().classes.get_mut(current_class.as_str()).unwrap().functions.insert(name.clone(), FunctionOptions {
        args: args.clone(),
        typ: typ.clone(),
        public,
    });

    Ok((Statement {
        kind: StatementKind::FunctionDeclaration(FunctionDeclaration {
            name,
            typ,
            body,
            pos: toks[begin].pos.clone(),
        }),
        pos: toks[begin].pos.clone(),
    }, i, scope_stack.clone()))
}

pub(crate) fn parse_variable_declaration(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>, class: bool) -> Result<(Statement, usize, Vec<Scope>), String> {
    let mut i = *i;
    let begin = i;
    let mut scope_stack = scope_stack.clone();
    let current_class = scope_stack.last().unwrap().current_class.clone().unwrap();
    i += 1;

    let mutable = if let Ok(_) = expect(&i, &toks, TokenValue::Identifier("mut".to_string()), true) {
        i += 1;
        true
    } else {
        false
    };

    let name = expect(&i, &toks, TokenValue::empty("identifier")?, false)?.value.as_string();
    if scope_stack.last().unwrap().variables.iter().any(|v| v.0 == &name) {
        return Err(error(format!("Variable '{}' already declared", name), toks[i].pos.clone()));
    }

    i += 1;
    expect(&i, &toks, TokenValue::Punctuation(":".to_string()), true)?;
    i += 1;
    let type_ident = expect(&i, &toks, TokenValue::empty("identifier")?, false)?;
    let typ = parse_type(&type_ident, &scope_stack)?;
    i += 1;
    if typ == ValueType::None {
        return Err(error("Type cannot be None".to_string(), toks[i].pos.clone()));
    }

    expect(&i, &toks, TokenValue::Punctuation("=".to_string()), true)?;
    i += 1;
    let (expr, j) = parse_expression(&i, toks, &mut scope_stack)?;
    if typ != expr.typ && expr.typ != ValueType::None {
        return Err(error(format!("Type mismatch: expected {:?}, but found {:?}", typ, expr.typ), toks[i].pos.clone()));
    }

    i = j;
    expect(&i, &toks, TokenValue::Punctuation(";".to_string()), true)?;
    i += 1;

    if class {
        scope_stack.last_mut().unwrap().classes.get_mut(current_class.as_str()).unwrap().variables.insert(name.clone(), VariableOptions {
            mutable,
            typ: typ.clone(),
        });
    } else {
        scope_stack.last_mut().unwrap().variables.insert(name.clone(), VariableOptions {
            mutable,
            typ: typ.clone(),
        });
    }

    Ok((Statement {
        kind: StatementKind::VariableDeclaration(VariableDeclaration {
            name,
            typ,
            expr,
            class,
            pos: toks[begin].pos.clone(),
        }),
        pos: toks[begin].pos.clone(),
    }, i, scope_stack.clone()))
}

pub(crate) fn parse_expression_statement(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Statement, usize, Vec<Scope>), String> {
    let mut i = *i;
    let begin = i;
    let (expr, j) = parse_expression(&i, toks, scope_stack)?;
    i = j;
    expect(&i, &toks, TokenValue::Punctuation(";".to_string()), true)?;
    i += 1;

    Ok((Statement {
        kind: StatementKind::ExpressionStatement(ExpressionStatement {
            typ: expr.clone().typ,
            expr,
            pos: toks[begin].pos.clone(),
        }),
        pos: toks[begin].pos.clone(),
    }, i, scope_stack.clone()))
}

pub(crate) fn parse_variable_reassignment(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Statement, usize, Vec<Scope>), String> {
    let mut i = *i;
    let begin = i;

    let var_name = if let TokenValue::Identifier(name) = &toks[i].value {
        name.clone()
    } else {
        return Err(error("Expected an identifier for variable assignment".to_string(), toks[i].pos.clone()));
    };
    i += 1;

    expect(&i, &toks, TokenValue::Punctuation("=".to_string()), true)?;
    i += 1;

    let (expr, j) = parse_expression(&i, toks, scope_stack)?;
    i = j;

    if let Some(var) = scope_stack.last_mut().unwrap().variables.get_mut(&var_name) {
        if !var.mutable {
            return Err(error(format!("Variable '{}' is not mutable", var_name), toks[begin].pos.clone()));
        }
        if var.typ != expr.typ {
            return Err(error(format!("Type mismatch: expected {:?}, but found {:?}", var.typ, expr.typ), toks[begin].pos.clone()));
        }
    } else {
        return Err(error(format!("Variable '{}' not declared", var_name), toks[begin].pos.clone()));
    }

    expect(&i, &toks, TokenValue::Punctuation(";".to_string()), true)?;
    i += 1;

    Ok((Statement {
        kind: StatementKind::VariableReassignment(VariableReassignment {
            name: var_name,
            typ: expr.typ.clone(),
            expr,
            class: false,
            pos: toks[begin].pos.clone(),
        }),
        pos: toks[begin].pos.clone(),
    }, i, scope_stack.clone()))
}

fn parse_class_statement(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Statement, usize, Vec<Scope>), String> {
    let mut i = *i;
    let begin = i;

    let ident = expect(&i, &toks, TokenValue::Identifier("".to_string()), false)?.value.as_string();
    i += 1;

    let class_name = if ident == "Self" {
        scope_stack.last().unwrap().current_class.clone().unwrap()
    } else {
        ident.clone()
    };

    if ident == scope_stack.last().unwrap().current_class.clone().unwrap() {
        return Err(error("Cannot reference the class itself in a class expression".to_string(), toks[begin].pos.clone()));
    }

    let class = if let Some(class) = scope_stack.last().unwrap().classes.get(&class_name) {
        class
    } else {
        return Err(error(format!("Class '{}' not declared", class_name), toks[begin].pos.clone()));
    };

    expect(&i, &toks, TokenValue::Punctuation(".".to_string()), true)?;
    i += 1;

    let var = expect(&i, &toks, TokenValue::empty("identifier")?, false)?.value.as_string();
    i += 1;
    let v = class.variables.get(&var).ok_or_else(|| error("Tried to reassign undeclared class variable".to_string(), toks[begin].pos.clone()))?;
    expect(&i, &toks, TokenValue::Punctuation("=".to_string()), true)?;
    i += 1;

    if !v.mutable {
        return Err(error(format!("Variable '{}' is not mutable", var), toks[begin].pos.clone()));
    }

    let (expr, j) = parse_expression(&i, &toks, &mut scope_stack.clone())?;
    i = j;
    if v.typ != expr.typ && expr.typ != ValueType::None {
        return Err(error(format!("Type mismatch: expected {:?}, but found {:?}", v.typ, expr.typ), toks[begin].pos.clone()));
    }

    expect(&i, &toks, TokenValue::Punctuation(";".to_string()), true)?;
    i += 1;

    Ok((Statement {
        kind: StatementKind::VariableReassignment(VariableReassignment {
            name: var.clone(),
            typ: v.typ.clone(),
            expr,
            class: true,
            pos: toks[begin].pos.clone(),
        }),
        pos: toks[begin].pos.clone(),
    }, i, scope_stack.clone()))
}