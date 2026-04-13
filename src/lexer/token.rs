use nom::{Input, Needed};
use std::iter::Enumerate;
use std::ops::{Range, RangeFrom, RangeFull, RangeTo};
use std::slice::Iter;
use std::ops::Index;

#[derive(PartialEq, Copy, Debug, Clone)]
pub enum Token<'a> {
    Fn,
    Static,
    Mut,
    Val,
    Require,
    Return,
    RBrace,
    LParen,
    RParen,
    Semi,
    Colon,
    Comma,
    Arrow,
    Dot,
    Plus,
    Minus,
    Star,
    Slash,
    Eq,
    Ampersand,
    Int(i64),
    Str(&'a str),
    Ident(&'a str),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tokens<'a> {
    tokens: &'a [Token<'a>],
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
    type Iter = Iter<'a, Token<'a>>;
    type IterIndices = Enumerate<Self::Iter>;

    fn input_len(&self) -> usize {
        self.tokens.len()
    }

    // Searches for the first token matching the predicate and returns its index
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.tokens.iter().position(|&t| predicate(t))
    }

    // Returns an iterator over the tokens
    fn iter_elements(&self) -> Self::Iter {
        self.tokens.iter()
    }

    // Returns an iterator over the (index, token) pairs
    fn iter_indices(&self) -> Self::IterIndices {
        self.tokens.iter().enumerate()
    }

    // Standard slice-based indexing for nom
    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        if self.tokens.len() >= count {
            Ok(count)
        } else {
            Err(Needed::new(count - self.tokens.len()))
        }
    }

    // Essential slicing methods
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
}
