use crate::parser::expression::parse_expression;
use std::collections::HashMap;
use crate::error;
use crate::lexer::{Token, TokenValue};
use crate::parser::{enter_scope, exit_scope, expect, expect_unstrict, parse_type, ClassDeclaration, ClassOptions, Expression, ExpressionKind, FunctionDeclaration, PrimaryExpression, Scope, Statement, StatementKind, Value, ValueType, VariableDeclaration, VariableOptions};
use crate::parser::ExpressionKind::Primary;

pub(crate) fn parse_class_declaration(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Statement, usize, Vec<Scope>), String> {
    let mut i = *i;
    let begin = i;
    let mut scope_stack = scope_stack.clone();
    i += 1;

    let public = if let Ok(_) = expect(&i, &toks, TokenValue::Identifier("pub".to_string())) {
        i += 1;
        true
    } else {
        false
    };

    let name = expect_unstrict(&i, &toks, TokenValue::empty("identifier")?)?.value.as_string();
    if scope_stack.last().unwrap().classes.iter().any(|c| c.0 == &name) {
        return Err(error(format!("Class '{}' already declared", name), toks[i].pos.clone()));
    }

    if name == "Self" {
        return Err(error("Class name cannot be 'Self'".to_string(), toks[i].pos.clone()));
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

    let (variables, j) = parse_class_variables(&i, &toks, &mut scope_stack)?;
    i = j;

    let (functions, h, mut scope) = parse_class_functions(&i, &toks, &mut scope_stack)?;
    i = h;
    scope_stack = exit_scope(&mut scope);
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

fn parse_class_variables(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Vec<VariableDeclaration>, usize), String> {
    let mut i = *i;
    let mut variables: Vec<VariableDeclaration> = vec![];

    while i < toks.len() {
        if toks[i].value == TokenValue::Punctuation("}".to_string()) {
            break;
        } else if toks[i].value == TokenValue::Identifier("val".to_string()) {
            i += 1;
            let (var, j) = parse_variable_declaration(&i, &toks, scope_stack)?;
            variables.push(var);
            i = j;
        } else {
            break;
        }

    }

    Ok((variables, i))
}

fn parse_class_functions(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Vec<FunctionDeclaration>, usize, Vec<Scope>), String> {
    todo!("Implement parse_class_functions function");
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