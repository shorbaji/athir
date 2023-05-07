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

pub trait Lexer {
    fn read_line(&mut self) -> Option<String>;
    fn get_tokens(&mut self) -> &mut Peekable<std::vec::IntoIter<Token>>;
    fn set_tokens(&mut self, tokens: Peekable<std::vec::IntoIter<Token>>);
    /// refresh
    /// 
    /// This function:
    /// - gets a new string of input from the source
    /// - lexically analyzes the string with a DelimitedLexer
    /// - collects the tokens into a vector
    /// - converts the vector into an iterator
    /// - wraps the iterator in a Peekable iterator
    /// - returns the iterator

    fn refresh(&mut self) -> std::result::Result<Peekable<std::vec::IntoIter<Token>>, std::io::Error> {
        match self.read_line() {
            Some(line) => {
                let lexer = DelimitedLexer::new(&line);
                let tokens = lexer.collect::<Vec<Token>>();
                Ok(tokens.into_iter().peekable())
            },
            None => Err(std::io::Error::new(std::io::ErrorKind::UnexpectedEof, "No more input")),
        }
    }

    /// peek
    /// 
    /// if no more tokens are available from the current Peekable iterator
    /// then this function calls refresh to get a new one
    ///
    fn peek(&mut self) -> Option<Token> {
        while self.get_tokens().peek().is_none() {
            match self.refresh() {
                Ok(inner) => self.set_tokens(inner),
                Err(_) => return None,
            }
        }

        match self.get_tokens().peek() {
            Some(token) => Some(token.clone()),
            None => None,
        }
    }

    fn get_next_token(&mut self) -> Option<Token> {
        while self.get_tokens().peek().is_none() {
            match self.refresh() {
                Ok(inner) => self.set_tokens(inner),
                Err(_) => return None,
            }
        }

        self.get_tokens().next()
    }

    fn peek_next_token(&mut self) -> Option<Token> {
        while self.get_tokens().peek().is_none() {
            match self.refresh() {
                Ok(inner) => self.set_tokens(inner),
                Err(_) => return None,
            }
        }

        self.get_tokens().peek().cloned()
    }
}
