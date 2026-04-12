use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+")]
#[logos(skip(r"//[^\n]*", allow_greedy = true))]
pub enum Token {
    // keywords
    #[token("class")]
    Class,

    #[token("fn")]
    Fn,

    #[token("static")]
    Static,

    #[token("mut")]
    Mut,
    
    #[token("val")]
    Val,

    #[token("require")]
    Require,

    #[token("return")]
    Return,

    // visibility
    #[token("!")]
    Bang,

    // punctuation
    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token(";")]
    Semi,

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[token("->")]
    Arrow,

    #[token(".")]
    Dot,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Star,

    #[token("/")]
    Slash,

    #[token("=")]
    Eq,

    #[token("&")]
    Ampersand,

    // literals
    #[regex(r"[0-9]+", |lex| lex.slice().parse().ok())]
    Int(i64),

    #[regex(r#""([^"\\]|\\.)*""#, |lex| lex.slice().to_string())]
    Str(String),

    // identifiers
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Ident(String),
}