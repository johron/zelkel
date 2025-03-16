use crate::{error};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenValue {
    Identifier(String),
    String(String),
    Integer(i32),
    Float(f32),
    Bool(bool),
    Arithmetic(String),
    Punctuation(char),
}

impl TokenValue {
    pub fn empty(tok: &str) -> Result<TokenValue, String> {
        match tok {
            "identifier" => Ok(TokenValue::Identifier("".to_owned())),
            "string" => Ok(TokenValue::String("".to_owned())),
            "integer" => Ok(TokenValue::Integer(0)),
            "float" => Ok(TokenValue::Float(0.0)),
            "bool" => Ok(TokenValue::Bool(false)),
            "arithmetic" => Ok(TokenValue::Arithmetic("".to_owned())),
            "punctuation" => Ok(TokenValue::Punctuation(' ')),
            _ => Err(error(format!("Unknown token: {}", tok), TokenPos { path: "".to_string(), line: 0, col: 0 })),
        }
    }

    pub fn as_string(&self) -> String {
        match self {
            TokenValue::Identifier(s) => s.clone(),
            TokenValue::String(s) => s.clone(),
            TokenValue::Integer(i) => i.to_string(),
            TokenValue::Float(f) => f.to_string(),
            TokenValue::Bool(b) => b.to_string(),
            TokenValue::Arithmetic(s) => s.clone(),
            TokenValue::Punctuation(c) => c.to_string(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct TokenPos {
    pub path: String,
    pub line: usize,
    pub col: usize,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub value: TokenValue,
    pub pos: TokenPos,
}

impl Token {
    pub fn empty() -> Token {
        Token { value: TokenValue::Identifier("".to_owned()), pos: TokenPos { path: "".to_string(), line: 0, col: 0 } }
    }
}

fn could_be(c: char, s: &str) -> bool {
    s.chars().any(|x| x == c)
}

pub fn lex(input: String, path: String) -> Result<Vec<Token>, String> {
    let mut toks: Vec<Token> = Vec::new();
    let mut pos = TokenPos { path, line: 1, col: 0 };
    let mut i = 0;

    while i < input.len() {
        let c = input.chars().nth(i).unwrap();
        let mut token = Token { value: TokenValue::Identifier("".to_owned()), pos: pos.clone() };

        if c.is_alphabetic() {
            let mut value = String::new();
            while i < input.len() && (input.chars().nth(i).unwrap().is_alphabetic() || input.chars().nth(i).unwrap() == '_') {
                value.push(input.chars().nth(i).unwrap());
                i += 1;
                pos.col += 1;
            }
            token.value = TokenValue::Identifier(value);
        } else if c.is_digit(10) {
            let mut value = String::new();
            while i < input.len() && input.chars().nth(i).unwrap().is_digit(10) {
                value.push(input.chars().nth(i).unwrap());
                i += 1;
                pos.col += 1;
            }
            token.value = TokenValue::Integer(value.parse().unwrap());
        } else if c == '"' {
            let mut value = String::new();
            i += 1;
            pos.col += 1;
            while i < input.len() && input.chars().nth(i).unwrap() != '"' {
                value.push(input.chars().nth(i).unwrap());
                i += 1;
                pos.col += 1;
            }
            i += 1;
            pos.col += 1;
            token.value = TokenValue::String(value);
        } else if could_be(c, "+-*/%=><!") {
            token.value = TokenValue::Arithmetic(c.to_string());
            i += 1;
            pos.col += 1;
        } else if could_be(c, "(){}[],.;:") {
            token.value = TokenValue::Punctuation(c);
            i += 1;
            pos.col += 1;
        } else if c == '\n' {
            pos.line += 1;
            pos.col = 0;
            i += 1;
            pos.col += 1;
            continue;
        } else if c.is_whitespace() {
            i += 1;
            pos.col += 1;
            continue;
        } else {
            return Err(error(format!("Unexpected character '{}'", c), pos));
        }

        toks.push(token);
    }

    Ok(toks)
}