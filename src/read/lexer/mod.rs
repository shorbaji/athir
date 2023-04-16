//! Lexical analyzer
//! 
//! This module implements the lexical analyzer for Athir
//! 
//! # Example
//! 
//! use athir::lexer::{Lexer, Source};
//!  
//! let source = Source::new(std::io::stdin().lines().map(|line| line.unwrap()));
//!
//! let lex = Lexer::new(source);
//! for token in lex {
//!     println!("token: {:?}", token);
//! }
//! 
//! Known issues:
//! - Though curly and square braces are reserved for future use we do not ascribe any meaning to them
//! - Only simple comments support (no nested comments or comments with datum)

#![warn(missing_docs)]

#[cfg(test)]
mod tests;

mod delimited;
mod number;
mod source;
mod token;

use std::iter::Peekable;
use delimited::DelimitedLexer;
pub use token::Token;
pub use source::Source;

pub struct Lexer<T> {
    inner: Peekable<std::vec::IntoIter<Token>>,
    source: T,
}

impl<T> Lexer<T> where T: Iterator<Item=String>{
    pub fn new(source: T) -> Self {
        Self {
            inner: vec!().into_iter().peekable(),
            source,
        }
    }

    fn refresh(&mut self) -> Option<Peekable<std::vec::IntoIter<Token>>> {
        self.source.next().map(|line| {
            DelimitedLexer::new(line.as_str()).collect::<Vec<Token>>().into_iter().peekable()
        })
    }

    pub fn peek(&mut self) -> Option<&Token> {
        if let None = self.inner.peek() {
            self.inner = self.refresh()?;
        }
        self.inner.peek()
    }
}

impl<T> Iterator for Lexer<T> 
    where T: Iterator<Item=String> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.next() {
            Some(token) => Some(token),
            None => {
                self.refresh();
                self.inner.next()
            }
        }
    }
}
