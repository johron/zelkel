use std::collections::HashMap;
use crate::error;
use crate::lexer::{Token, TokenValue};
use crate::parser::{enter_scope, exit_scope, expect, expect_unstrict, ClassDeclaration, ClassOptions, Scope, Statement, StatementKind};

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
    scope_stack.last_mut().unwrap().classes.insert(name.clone(), ClassOptions {
        functions: HashMap::new(),
        variables: HashMap::new(),
        public,
    });
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

fn parse_class_variables(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(HashMap<String, ClassOptions>, usize), String> {
    let mut i = *i;
    let mut variables = HashMap::new();

    while i < toks.len() && toks[i].value != TokenValue::Punctuation("}".to_string()) {
        if toks[i].value == TokenValue::Punctuation("}".to_string()) {
            break;
        }
        if toks[i].value == TokenValue::Punctuation(";".to_string()) {
            i += 1;
            continue;
        }
        let var_name = expect_unstrict(&i, &toks, TokenValue::empty("identifier")?)?.value.as_string();
        if variables.contains_key(&var_name) {
            return Err(error(format!("Variable '{}' already declared", var_name), toks[i].pos.clone()));
        }
        i += 1;

        let var_type = expect_unstrict(&i, &toks, TokenValue::empty("identifier")?)?.value.as_string();
        if var_type != "int" && var_type != "float" && var_type != "string" && var_type != "bool" {
            return Err(error(format!("Invalid variable type '{}'", var_type), toks[i].pos.clone()));
        }
        i += 1;

        variables.insert(var_name.clone(), ClassOptions {
            functions: HashMap::new(),
            variables: HashMap::new(),
            public: false,
        });
    }

    Ok((variables, i))
}