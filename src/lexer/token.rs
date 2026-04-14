use nom::{Input, Needed};
use std::iter::{Copied, Enumerate};
use std::slice::Iter;

#[derive(PartialEq, Copy, Debug, Clone, Eq)]
pub enum Token<'a> {
    Fn(usize),
    Class(usize),
    Static(usize),
    Mut(usize),
    Val(usize),
    Require(usize),
    Return(usize),
    LBrace(usize),
    RBrace(usize),
    LParen(usize),
    RParen(usize),
    Semi(usize),
    DoubleColon(usize),
    Colon(usize),
    Comma(usize),
    Arrow(usize),
    Dot(usize),
    Plus(usize),
    Minus(usize),
    Star(usize),
    Slash(usize),
    Eq(usize),
    Ampersand(usize),
    Bang(usize),
    Int(i64, usize),
    Str(&'a str, usize),
    Ident(&'a str, usize),
}

impl<'a> Token<'a> {
    pub fn offset(&self) -> usize {
        match self {
            Token::Ident(_, o) | Token::Fn(o) | Token::Static(o) => *o,
            Token::Class(o) | Token::Mut(o) | Token::Val(o) | Token::Require(o) | Token::Return(o) => *o,
            Token::LBrace(o) | Token::RBrace(o) | Token::LParen(o) | Token::RParen(o) | Token::Semi(o) | Token::Colon(o) | Token::Comma(o) => *o,
            Token::Arrow(o) | Token::Dot(o) | Token::Plus(o) | Token::Minus(o) | Token::Star(o) | Token::Slash(o) | Token::Eq(o) | Token::Ampersand(o) | Token::Bang(o) => *o,
            Token::Int(_, o) => *o, Token::Str(_, o) => *o, Token::DoubleColon(o) => *o,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tokens<'a> {
    pub(crate) tokens: &'a [Token<'a>],
    start: usize,
    end: usize,
}

impl<'a> Tokens<'a> {
    pub fn new(tokens: &'a [Token<'a>]) -> Self {
        Self {
            tokens,
            start: 0,
            end: tokens.len(),
        }
    }
}

impl<'a> Input for Tokens<'a> {
    type Item = Token<'a>;
    type Iter = Copied<Iter<'a, Token<'a>>>;
    type IterIndices = Enumerate<Self::Iter>;

    fn input_len(&self) -> usize {
        self.tokens.len()
    }

    fn take(&self, index: usize) -> Self {
        Tokens::new(&self.tokens[..index])
    }

    fn take_from(&self, index: usize) -> Self {
        Tokens::new(&self.tokens[index..])
    }

    fn take_split(&self, index: usize) -> (Self, Self) {
        let (prefix, suffix) = self.tokens.split_at(index);
        (Tokens::new(suffix), Tokens::new(prefix))
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.tokens.iter().position(|&t| predicate(t))
    }

    fn iter_elements(&self) -> Self::Iter {
        self.tokens.iter().copied()
    }

    fn iter_indices(&self) -> Self::IterIndices {
        self.tokens.iter().copied().enumerate()
    }

    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        if self.tokens.len() >= count {
            Ok(count)
        } else {
            Err(Needed::new(count - self.tokens.len()))
        }
    }
}
