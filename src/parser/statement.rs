use crate::parser::expression::parse_expression;
use std::collections::HashMap;
use crate::error;
use crate::lexer::{Token, TokenValue};
use crate::parser::{enter_scope, exit_scope, expect, expect_unstrict, parse_type, ClassDeclaration, ClassOptions, Expression, ExpressionKind, ExpressionStatement, FunctionDeclaration, FunctionOptions, PrimaryExpression, Scope, Statement, StatementKind, Value, ValueType, VariableDeclaration, VariableOptions, RESERVED};

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
    scope_stack.last_mut().unwrap().current_class = ClassOptions {
        public,
        extends: extends.clone(),
        variables: HashMap::new(),
        functions: HashMap::new(),
    };

    let mut variables: Vec<VariableDeclaration> = Vec::new();
    let mut variable_options: Vec<VariableOptions> = Vec::new();
    let mut functions: Vec<FunctionDeclaration> = Vec::new();
    let mut function_options: Vec<FunctionOptions> = Vec::new();
    
    while i < toks.len() {
        if toks[i].value == TokenValue::Punctuation("}".to_string()) {
            break;
        } else if toks[i].value == TokenValue::Identifier("val".to_string()) {
            i += 1;
            let (var, opt, j, new_scope) = parse_variable_declaration(&i, &toks, &mut scope_stack, true)?;
            variables.push(var.clone());
            variable_options.push(opt.clone());
            i = j;
            scope_stack = new_scope;
        } else if toks[i].value == TokenValue::Identifier("fn".to_string()) {
            i += 1;
            let (func, opt, j, new_scope) = parse_function_declaration(&i, &toks, &mut scope_stack, true)?;
            functions.push(func.clone());
            function_options.push(opt.clone());
            i = j;
            scope_stack = new_scope;
        } else {
            break;
        }
    }

    expect(&i, &toks, TokenValue::Punctuation("}".to_string()))?;
    i += 1;

    scope_stack = exit_scope(&mut scope_stack);
    
    let mut vars: HashMap<String, VariableOptions> = HashMap::new();
    let mut funcs: HashMap<String, FunctionOptions> = HashMap::new();
    
    for (var, opt) in variables.iter().zip(variable_options.iter()) {
        vars.insert(var.name.clone(), opt.clone());
    }
    
    for (func, opt) in functions.iter().zip(function_options.iter()) {
        funcs.insert(func.name.clone(), opt.clone());
    }
    
    scope_stack.last_mut().unwrap().classes.insert(name.clone(), ClassOptions {
        public,
        extends: extends.clone(),
        variables: vars,
        functions: funcs,
    });

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

fn parse_variable_declaration(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>, class: bool) -> Result<(VariableDeclaration, VariableOptions, usize, Vec<Scope>), String> {
    let mut i = *i;
    let begin = i;
    let mut scope_stack = scope_stack.clone();

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

    let typ = parse_type(&expect_unstrict(&i, &toks, TokenValue::empty("identifier")?)?, &scope_stack)?;
    i += 1;

    let expr: Option<Expression> = if let Ok(_) = expect(&i, &toks, TokenValue::Punctuation("=".to_string())) {
        i += 1;
        let (expr, j) = parse_expression(&i, toks, &mut scope_stack)?;
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

    let decl = VariableDeclaration {
        name: name.clone(),
        typ: typ.clone(),
        expr: expr.clone(),
        pos: toks[begin].pos.clone(),
    };
    
    let options = VariableOptions {
        mutable,
        public,
        typ: typ.clone(),
    };
    
    if class {
        scope_stack.last_mut().unwrap().current_class.variables.insert(name.clone(), options.clone());
    } else {
        scope_stack.last_mut().unwrap().variables.insert(name.clone(), options.clone());
    }
    
    Ok((decl, options, i, scope_stack))
}

fn parse_function_declaration(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>, class: bool) -> Result<(FunctionDeclaration, FunctionOptions, usize, Vec<Scope>), String> {
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
    let mut args: HashMap<String, VariableOptions> = HashMap::new();
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

        args.insert(arg_name, VariableOptions {
            mutable: false, // Arguments are not mutable by default
            public: false, // Arguments are not public by default
            typ: arg_type,
        });
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
    let (body, j, new_scope) = parse_function_body(&i, toks, &mut scope_stack)?;
    i = j;
    scope_stack = new_scope;
    
    expect(&i, &toks, TokenValue::Punctuation("}".to_string()))?;
    i += 1;
    scope_stack = exit_scope(&mut scope_stack);
    
    let options = FunctionOptions {
        public,
        args: args.clone(),
        typ: typ.clone(),
    };
    
    let decl = FunctionDeclaration {
        name: name.clone(),
        args: args.clone(),
        typ: typ.clone(),
        public,
        pos: toks[begin].pos.clone(),
        body,
    };
    
    if class {
        scope_stack.last_mut().unwrap().current_class.functions.insert(name.clone(), options.clone());
    } else {
        scope_stack.last_mut().unwrap().functions.insert(name.clone(), options.clone());
    }

    Ok((decl, options, i, scope_stack))
}

fn parse_function_body(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Vec<Statement>, usize, Vec<Scope>), String> {
    let mut i = *i;
    let mut body: Vec<Statement> = vec![];

    while i < toks.len() {
        if toks[i].value == TokenValue::Punctuation("}".to_string()) {
            break;
        }

        if toks[i].value == TokenValue::Identifier("val".to_string()) {
            i += 1;
            let (var, _, j, new_scope) = parse_variable_declaration(&i, toks, scope_stack, false)?;
            body.push(Statement {
                kind: StatementKind::VariableDeclaration(var.clone()),
                pos: var.pos.clone(),
            });
            println!("{:?}", new_scope);
            *scope_stack = new_scope;
            i = j;
        } else if toks[i].value == TokenValue::Identifier("fn".to_string()) {
            i += 1;
            let (func, _, j, new_scope) = parse_function_declaration(&i, toks, scope_stack, false)?;
            body.push(Statement {
                kind: StatementKind::FunctionDeclaration(func.clone()),
                pos: func.pos.clone(),
            });
            println!("{:?}", new_scope);
            *scope_stack = new_scope;
            i = j;
        } else {
            let (expr, j) = parse_expression(&i, toks, scope_stack)?;
            body.push(Statement {
                kind: StatementKind::ExpressionStatement(ExpressionStatement {
                    expr: expr.clone(),
                    pos: toks[i].pos.clone(),
                    typ: expr.typ.clone(),
                }),
                pos: toks[i].pos.clone(),
            });
            i = j;
            expect(&i, &toks, TokenValue::Punctuation(";".to_string()))?;
            i += 1;
        }
    }

    Ok((body, i, scope_stack.clone()))
}