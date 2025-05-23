use crate::parser::expression::parse_expression;
use std::collections::HashMap;
use crate::error;
use crate::lexer::{Token, TokenValue};
use crate::parser::{enter_scope, exit_scope, expect, expect_unstrict, parse_type, ClassDeclaration, ClassOptions, Expression, ExpressionKind, FunctionDeclaration, PrimaryExpression, Scope, Statement, StatementKind, Value, ValueType, VariableDeclaration, VariableOptions, RESERVED};
use crate::parser::ExpressionKind::Primary;

pub(crate) fn parse_class_declaration(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Statement, usize, Vec<Scope>), String> {
    let mut i = *i;
    let begin = i;
    let mut scope_stack = scope_stack.clone();
    i += 1;

    let public = if let Ok(_) = expect(&i, &toks, TokenValue::Punctuation("!".to_string())) {
        i += 1;
        true
    } else {
        false
    };

    let name = expect_unstrict(&i, &toks, TokenValue::empty("identifier")?)?.value.as_string();
    if scope_stack.last().unwrap().classes.iter().any(|c| c.0 == &name) {
        return Err(error(format!("Class '{}' already declared", name), toks[i].pos.clone()));
    }

    if RESERVED.contains(&name.as_str()) {
        return Err(error(format!("Cannot name class '{}' a reserved keyword", name), toks[i].pos.clone()));
    }

    i += 1;
    let extends: Option<String> = if let Ok(_) = expect(&i, &toks, TokenValue::Punctuation(":".to_string())) {
        i += 1;
        let inherit = expect_unstrict(&i, &toks, TokenValue::empty("identifier")?)?.value.as_string();
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

    expect(&i, &toks, TokenValue::Punctuation("{".to_string()))?;
    i += 1;
    scope_stack = enter_scope(&mut scope_stack);
    scope_stack.last_mut().unwrap().current_class = Some(name.clone());

    let mut variables: Vec<VariableDeclaration> = vec![];
    let mut functions: Vec<FunctionDeclaration> = vec![];
    while i < toks.len() {
        if toks[i].value == TokenValue::Punctuation("}".to_string()) {
            break;
        } else if toks[i].value == TokenValue::Identifier("val".to_string()) {
            i += 1;
            let (var, j) = parse_variable_declaration(&i, &toks, &mut scope_stack)?;
            variables.push(var);
            i = j;
        } else if toks[i].value == TokenValue::Identifier("fn".to_string()) {
            i += 1;
            let (func, j, mut new_scope) = parse_function_declaration(&i, &toks, &mut scope_stack)?;
            functions.push(func);
            i = j;
            scope_stack = new_scope;
        } else {
            break;
        }
    }

    expect(&i, &toks, TokenValue::Punctuation("}".to_string()))?;
    i += 1;

    Ok((Statement {
        kind: StatementKind::ClassDeclaration(ClassDeclaration {
            name,
            extends,
            variables,
            functions,
            pos: toks[begin].pos.clone(),
        }),
        pos: toks[begin].pos.clone(),
    }, i, scope_stack))
}

fn parse_variable_declaration(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(VariableDeclaration, usize), String> {
    let mut i = *i;
    let begin = i;

    let public = if let Ok(_) = expect(&i, &toks, TokenValue::Punctuation("!".to_string())) {
        i += 1;
        true
    } else {
        false
    };

    let mutable = if let Ok(_) = expect(&i, &toks, TokenValue::Identifier("mut".to_string())) {
        i += 1;
        true
    } else {
        false
    };

    let name = expect_unstrict(&i, &toks, TokenValue::empty("identifier")?)?.value.as_string();
    if scope_stack.last().unwrap().variables.iter().any(|v| v.0 == &name) {
        return Err(error(format!("Variable '{}' already declared", name), toks[i].pos.clone()));
    }

    if RESERVED.contains(&name.as_str()) {
        return Err(error(format!("Cannot name variable '{}' a reserved keyword", name), toks[i].pos.clone()));
    }

        i += 1;
    expect(&i, &toks, TokenValue::Punctuation(":".to_string()))?;
    i += 1;

    let typ = parse_type(&expect_unstrict(&i, &toks, TokenValue::empty("identifier")?)?, scope_stack)?;
    i += 1;

    let expr: Option<Expression> = if let Ok(_) = expect(&i, &toks, TokenValue::Punctuation("=".to_string())) {
        i += 1;
        let (expr, j) = parse_expression(&i, toks, scope_stack)?;
        i = j;
        if typ != expr.typ && expr.typ != ValueType::None {
            return Err(error(format!("Type mismatch: expected {:?}, but found {:?}", typ, expr.typ), toks[i].pos.clone()));
        }
        Some(expr)
    } else {
        None
    };

    expect(&i, &toks, TokenValue::Punctuation(";".to_string()))?;
    i += 1;

    scope_stack.last_mut().unwrap().variables.insert(name.clone(), VariableOptions {
        mutable,
        public,
        typ: typ.clone(),
    });

    Ok((VariableDeclaration {
        name,
        typ,
        expr,
        pos: toks[begin].pos.clone(),
    }, i))
}

fn parse_function_declaration(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(FunctionDeclaration, usize, Vec<Scope>), String> {
    let mut i = *i;
    let begin = i;

    let public = if let Ok(_) = expect(&i, &toks, TokenValue::Punctuation("!".to_string())) {
        i += 1;
        true
    } else {
        false
    };

    let name = expect_unstrict(&i, &toks, TokenValue::empty("identifier")?)?.value.as_string();
    if scope_stack.last().unwrap().variables.iter().any(|v| v.0 == &name) {
        return Err(error(format!("Function '{}' already declared", name), toks[i].pos.clone()));
    }
    i += 1;

    if RESERVED.contains(&name.as_str()) {
        return Err(error(format!("Cannot name function '{}' a reserved keyword", name), toks[i].pos.clone()));
    }

    expect(&i, &toks, TokenValue::Punctuation("(".to_string()))?;
    i += 1;
    let mut args: Vec<(String, ValueType)> = vec![];
    while i < toks.len() {
        if toks[i].value == TokenValue::Punctuation(")".to_string()) {
            break;
        } else if toks[i].value == TokenValue::Punctuation(",".to_string()) {
            i += 1;
            continue;
        }

        let arg_name = expect_unstrict(&i, &toks, TokenValue::empty("identifier")?)?.value.as_string();
        if scope_stack.last().unwrap().variables.iter().any(|v| v.0 == &arg_name) {
            return Err(error(format!("Argument '{}' already declared", arg_name), toks[i].pos.clone()));
        }
        i += 1;

        if RESERVED.contains(&arg_name.as_str()) {
            return Err(error(format!("Cannot name argument '{}' a reserved keyword", arg_name), toks[i].pos.clone()));
        }

        expect(&i, &toks, TokenValue::Punctuation(":".to_string()))?;
        i += 1;

        let arg_type = parse_type(&expect_unstrict(&i, &toks, TokenValue::empty("identifier")?)?, scope_stack)?;
        i += 1;

        args.push((arg_name, arg_type));
    }

    expect(&i, &toks, TokenValue::Punctuation(")".to_string()))?;
    i += 1;
    let typ = if let Ok(_) = expect(&i, &toks, TokenValue::Punctuation("->".to_string())) {
        i += 1;
        let t = parse_type(&expect_unstrict(&i, &toks, TokenValue::empty("identifier")?)?, scope_stack)?;
        i += 1;
        t
    } else {
        ValueType::None
    };
    expect(&i, &toks, TokenValue::Punctuation("{".to_string()))?;
    i += 1;

    let mut scope_stack = enter_scope(scope_stack);
    todo!("parse function body");

    expect(&i, &toks, TokenValue::Punctuation("}".to_string()))?;
    i += 1;
    scope_stack = exit_scope(&mut scope_stack);

    Ok((FunctionDeclaration {
        name,
        args,
        typ,
        public,
        pos: toks[begin].pos.clone(),
        body: vec![],
    }, i, scope_stack))
}