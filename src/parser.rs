use std::collections::HashMap;
use crate::error;
use crate::lexer::{Token, TokenPos, TokenValue};

#[derive(Debug, Clone)]
pub struct Statement {
    kind: StatementKind,
    pos: TokenPos,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub enum ValueType {
    #[default]
    PrimitiveInteger,
    PrimitiveFloat,
    PrimitiveString,
    PrimitiveBool,
    Class(String),
    None,
}

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i32),
    Float(f32),
    String(String),
    Bool(bool),
    Variable(String),
    Function(String),
    Class(String),
    Arithmetic(String),
}

impl From<TokenValue> for Value {
    fn from(token_value: TokenValue) -> Self {
        match token_value {
            TokenValue::Integer(i) => Value::Integer(i),
            TokenValue::Float(f) => Value::Float(f),
            TokenValue::String(s) => Value::String(s),
            TokenValue::Bool(b) => Value::Bool(b),
            TokenValue::Arithmetic(s) => Value::Arithmetic(s),
            _ => unimplemented!("Conversion for this TokenValue variant is not implemented"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    VariableDeclaration(VariableDeclaration),
    VariableReassignment(VariableReassignment),
    FunctionDeclaration(FunctionDeclaration),
    ClassDeclaration(ClassDeclaration),
    ExpressionStatement(ExpressionStatement),
    Block(Vec<Statement>),
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    name: String,
    typ: ValueType,
    expr: Expression,
}

#[derive(Debug, Clone)]
pub struct VariableReassignment {
    name: String,
    typ: ValueType,
    expr: Expression,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    name: String,
    typ: ValueType,
    body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct ClassDeclaration {
    name: String,
    extends: Option<String>,
    body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    typ: ValueType,
    expr: Expression,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Primary(PrimaryExpression),
    Unary(Box<UnaryExpression>),
    Term(Box<TermExpression>),
    Binary(Box<BinaryExpression>),
    Comparison(Box<ComparisonExpression>),
}

#[derive(Debug, Clone)]
pub struct Expression {
    kind: ExpressionKind,
    typ: ValueType
}

#[derive(Debug, Clone)]
pub struct PrimaryExpression {
    value: Value,
    typ: ValueType,
}

#[derive(Debug, Clone)]
pub struct UnaryExpression {
    left: ExpressionKind,
    typ: ValueType,
    op: Value,
}

#[derive(Debug, Clone)]
pub struct TermExpression {
    left: ExpressionKind,
    right: ExpressionKind,
    typ: ValueType,
    op: Value,
}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    left: ExpressionKind,
    right: ExpressionKind,
    typ: ValueType,
    op: Value,
}

#[derive(Debug, Clone)]
pub struct ComparisonExpression {
    left: ExpressionKind,
    right: ExpressionKind,
    typ: ValueType,
    op: Value,
}

#[derive(Debug, Clone)]
pub struct VariableOptions {
    pub mutable: bool,
    pub typ: ValueType,
}

#[derive(Debug, Clone)]
pub struct FunctionOptions {
    pub args: Vec<VariableOptions>,
    pub typ: ValueType,
    pub public: bool,
}

#[derive(Debug, Clone)]
pub struct ClassOptions {
    pub functions: HashMap<String, FunctionOptions>,
    pub variables: HashMap<String, VariableOptions>,
    pub public: bool,
}

#[derive(Clone, Debug)]
pub struct Scope {
    variables: HashMap<String, VariableOptions>, // Function-nested variables
    classes: HashMap<String, ClassOptions>,
    current_class: Option<String>,
}

fn expect(i: &usize, toks: &Vec<Token>, value: TokenValue, strict: bool) -> Result<Token, String> {
    let i = *i;
    if i >= toks.len() {
        return Err(error("Unexpected end of file".to_string(), toks.last().unwrap().pos.clone()));
    }

    if toks[i].value == value && strict {
        return Ok(toks[i].clone());
    }

    if std::mem::discriminant(&toks[i].value) == std::mem::discriminant(&value) && !strict {
        return Ok(toks[i].clone());
    }

    Err(error(format!("Expected {:?} but got {:?}", value, toks[i].value), toks[i].pos.clone()))
}

fn enter_scope(scope: &mut Vec<Scope>) -> Vec<Scope> {
    let parent_scope = scope.last().cloned().unwrap_or(Scope {
        variables: HashMap::new(),
        classes: HashMap::new(),
        current_class: None,
    });
    scope.push(parent_scope);
    scope.clone()
}

fn exit_scope(scope: &mut Vec<Scope>) -> Vec<Scope> {
    scope.pop();
    scope.clone()
}

fn parse_type(tok: &Token, scope_stack: &Vec<Scope>) -> Result<ValueType, String> {
    let current_class = scope_stack.last().unwrap().current_class.clone().unwrap();
    match tok.value {
        TokenValue::Identifier(ref s) => match s.as_str() {
            "__prim_int" => Ok(ValueType::PrimitiveInteger),
            "__prim_str" => Ok(ValueType::PrimitiveString),
            "__prim_float" => Ok(ValueType::PrimitiveFloat),
            "__prim_bool" => Ok(ValueType::PrimitiveBool),
            _ => {
                if let Some(_) = scope_stack.last().unwrap().classes.get(s) {
                    Ok(ValueType::Class(s.clone()))
                } else if s == "Self" {
                    todo!("I have to do some more here with the functions and variables inside the current class, see readme");
                } else if let Some(f) = scope_stack.last().unwrap().classes.get(current_class.as_str()).unwrap().functions.get(s.as_str()) {
                    Ok(f.typ.clone())
                } else if let Some(v) = scope_stack.last().unwrap().variables.get(s) {
                    Ok(v.typ.clone())
                } else {
                    Err(error(format!("Unknown type: '{}'", s), tok.pos.clone()))
                }
            }
        },
        _ => Err(error("Expected an identifier while parsing type".to_string(), tok.pos.clone())),
    }
}

fn parse_primary_expression(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Expression, usize), String> {
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
                }),
                typ: match &tok.value {
                    TokenValue::Integer(_) => ValueType::PrimitiveInteger,
                    TokenValue::Float(_) => ValueType::PrimitiveFloat,
                    TokenValue::String(_) => ValueType::PrimitiveString,
                    TokenValue::Bool(_) => ValueType::PrimitiveBool,
                    _ => unreachable!(),
                },
            }
        }
        TokenValue::Identifier(s) => {
            i += 1;
            if let Ok(_) = expect(&i, &toks, TokenValue::Punctuation("(".to_string()), true) {
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
                        }),
                        typ: f.clone().typ,
                    }
                } else {
                    return Err(error(format!("Tried to call an undeclared function '{}'", s), tok.pos.clone()));
                }
            } else {
                if let Some(v) = scope_stack.last().unwrap().variables.get(s) {
                    Expression {
                        kind: ExpressionKind::Primary(PrimaryExpression {
                            value: Value::Variable(tok.value.as_string()),
                            typ: v.typ.clone(),
                        }),
                        typ: v.typ.clone(),
                    }
                } else {
                    return Err(error(format!("Tried to reference an undeclared variable '{}'", s), tok.pos.clone()));
                }
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

fn parse_unary_expression(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Expression, usize), String> {
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
                })),
                typ: expr.typ,
            }, j));
        }

        return Err(error(format!("Type mismatch: expected {:?} or {:?}, but found {:?}", ValueType::PrimitiveInteger, ValueType::PrimitiveFloat, expr.typ), toks[i].pos.clone()));
    }
    parse_primary_expression(&mut i, toks, scope_stack)
}

fn parse_term_expression(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Expression, usize), String> {
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
                    })),
                    typ: expr.typ,
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

fn parse_binary_expression(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Expression, usize), String> {
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
                    })),
                    typ: expr.typ,
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

fn parse_comparison_expression(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Expression, usize), String> {
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
                    })),
                    typ: expr.typ,
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

fn parse_expression(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Expression, usize), String> {
    parse_comparison_expression(i, toks, scope_stack)
}

fn parse_function_body(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Vec<Statement>, usize, Vec<Scope>), String> {
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
                let (nested_body, j, mut scope) = parse_function_body(&i, toks, &mut scope_stack)?;
                i = j;
                scope_stack = exit_scope(&mut scope);
                body.push(Statement {
                    kind: StatementKind::Block(nested_body),
                    pos: tok.pos.clone(),
                });
                continue;
            }
        }

        let stmt: Result<(Statement, usize, Vec<Scope>), String> = match &tok.value {
            TokenValue::Identifier(s) => match s.as_str() {
                "val" => parse_variable_declaration(&i, toks, &mut scope_stack),
                _ => {
                    if let Some(_) = scope_stack.last().unwrap().variables.get(s) {
                        let (st, j, scope) = parse_variable_reassignment(&i, toks, &mut scope_stack)?;
                        Ok((st, j, scope))
                    } else if let Some(_) = scope_stack.last().unwrap().classes.get(current_class.as_str()).unwrap().functions.get(s) {
                        let (st, j, scope) = parse_expression_statement(&i, toks, &mut scope_stack)?;
                        Ok((st, j, scope))
                    } else if let Some(_) = scope_stack.last().unwrap().classes.get(s) {
                        let (st, j, scope) = parse_class_expression(&i, toks, &mut scope_stack)?;
                        Ok((st, j, scope))
                    } else {
                        return Err(error(format!("Unknown identifier: '{}', maybe undeclared variable or function", s), toks[i].clone().pos));
                    }
                },
            },
            _ => Err(error("Expected an identifier while parsing identifier".to_string(), toks[i].clone().pos)),
        };

        if let Ok((stmt, j, scope)) = stmt {
            i = j;
            body.push(stmt);
            scope_stack = scope;
        } else {
            return Err(error(format!("Error parsing statement: {:?}", stmt), toks[i].pos.clone()));
        }
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

        let stmt: Result<(Statement, usize, Vec<Scope>), String> = match &tok.value {
            TokenValue::Identifier(s) => match s.as_str() {
                "fn" => parse_function_declaration(&i, toks, &mut scope_stack),
                "val" => parse_variable_declaration(&i, toks, &mut scope_stack),
                _ => return Err(error(format!("Unknown identifier: '{}', maybe undeclared variable or function", s), toks[i].clone().pos.clone())),
            },
            _ => return Err(error("Expected an identifier while parsing identifier".to_string(), toks[i].clone().pos)),
        };

        if let Ok((stmt, j, scope)) = stmt {
            i = j;
            scope_stack = scope;
            body.push(stmt);
        } else {
            return Err(error(format!("Error parsing statement: {:?}", stmt), toks[i].pos.clone()));
        }
    }

    Ok((body, i, scope_stack))
}
fn parse_class_declaration(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Statement, usize, Vec<Scope>), String> {
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
        }),
        pos: toks[begin].pos.clone(),
    }, i, scope_stack))
}

fn parse_declaration_arguments(i: &usize, toks: &Vec<Token>, scope_stack: &Vec<Scope>, constructor: bool) -> Result<(Vec<VariableOptions>, usize), String> {
    let mut i = *i;
    let mut args: Vec<VariableOptions> = Vec::new();
    let mut first = true;
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
                typ,
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
    Ok((args, i))
}

fn parse_function_declaration(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Statement, usize, Vec<Scope>), String> {
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
    let (args, j) = parse_declaration_arguments(&i, &toks, &scope_stack, constructor)?;
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
    let (body, j, mut scope) = parse_function_body(&i, &toks, &mut scope_stack)?;
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
        }),
        pos: toks[begin].pos.clone(),
    }, i, scope_stack.clone()))
}

fn parse_variable_declaration(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Statement, usize, Vec<Scope>), String> {
    let mut i = *i;
    let begin = i;
    let mut scope_stack = scope_stack.clone();
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
    expect(&i, &toks, TokenValue::Punctuation("=".to_string()), true)?;
    i += 1;
    let (expr, j) = parse_expression(&i, toks, &mut scope_stack)?;
    if typ != expr.typ {
        return Err(error(format!("Type mismatch: expected {:?}, but found {:?}", typ, expr.typ), toks[i].pos.clone()));
    }

    i = j;
    expect(&i, &toks, TokenValue::Punctuation(";".to_string()), true)?;
    i += 1;

    scope_stack.last_mut().unwrap().variables.insert(name.clone(), VariableOptions {
        mutable,
        typ: typ.clone(),
    });

    Ok((Statement {
        kind: StatementKind::VariableDeclaration(VariableDeclaration {
            name,
            typ,
            expr,
        }),
        pos: toks[begin].pos.clone(),
    }, i, scope_stack))
}

fn parse_expression_statement(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Statement, usize, Vec<Scope>), String> {
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
        }),
        pos: toks[begin].pos.clone(),
    }, i, scope_stack.clone()))
}

fn parse_variable_reassignment(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Statement, usize, Vec<Scope>), String> {
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
        }),
        pos: toks[begin].pos.clone(),
    }, i, scope_stack.clone()))
}

fn parse_class_expression(i: &usize, toks: &Vec<Token>, scope_stack: &mut Vec<Scope>) -> Result<(Statement, usize, Vec<Scope>), String> {
    let mut i = *i;
    let begin = i;

    // Needs:
    // 1. Assign values that the class object has example: `Class.x = x;`
    // 2. Reference the class, and call its functions and variables
    // 3. May have to do some stuff for constructors with `Self` keyword, idk

    let class_name = toks[i].clone().value.as_string();
    scope_stack.last_mut().unwrap().current_class = Some(class_name.clone());

    let class = scope_stack.last_mut().unwrap().classes.get_mut(&class_name).unwrap().clone();


    todo!("Implement class expression parsing");
}

pub fn parse(toks: Vec<Token>) -> Result<Vec<Statement>, String> {
    let mut ast: Vec<Statement> = Vec::new();
    let mut i = 0;

    let mut scope_stack: Vec<Scope> = Vec::new();
    scope_stack.push(Scope { variables: HashMap::new(), classes: HashMap::new(), current_class: None });

    while i < toks.len() {
        if toks[i].value != TokenValue::Identifier("class".to_string()) {
            Err(error("Expected a class declaration".to_string(), toks[i].pos.clone()))?;
        }
        let (stmt, j, scope) = parse_class_declaration(&i, &toks, &mut scope_stack)?;
        scope_stack = scope;
        ast.push(stmt);
        i = j;
    }

    Ok(ast)
}