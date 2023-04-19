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
mod token;

use std::iter::Peekable;
use delimited::DelimitedLexer;
pub use token::Token;

/// Lexer
/// 
/// This struct is the public interface to the lexical analyzer.
/// It is an abstraction over the DelimitedLexer struct which is an abstraction over the Logos lexer.
/// It holds a Peekable iterator over the tokens produced by the DelimitedLexer 
/// It also holds a source which is an iterator over strings of input.

pub struct Lexer<T> where T: Iterator<Item=String> {
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

    /// refresh
    /// 
    /// This function:
    /// - gets a new string of input from the source
    /// - lexically analyzes the string with a DelimitedLexer
    /// - collects the tokens into a vector
    /// - converts the vector into an iterator
    /// - wraps the iterator in a Peekable iterator
    /// - returns the iterator

    fn refresh(&mut self) -> Option<Peekable<std::vec::IntoIter<Token>>> {
        self.source.next().map(|line| {
            DelimitedLexer::new(line.as_str()).collect::<Vec<Token>>().into_iter().peekable()
        })
    }

    /// peek
    /// 
    /// if no more tokens are available from the current Peekable iterator
    /// then this function calls refresh to get a new one
    ///
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

    /// next
    /// 
    /// if no more tokens are available from the current Peekable iterator
    /// then this function calls refresh to get a new one
    fn next(&mut self) -> Option<Self::Item> {
        if let None = self.inner.peek() {
            self.inner = self.refresh()?;
        }
        self.inner.next()
    }
}
