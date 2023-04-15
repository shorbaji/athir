//! Lexical analyzer
//! 
//! This module implements the lexical analyzer for Athir
//! 
//! # Example
//! 
//! use athir::lexer::Lexer;
//!  
//! let lex = Lexer::new(&line);
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
mod token;
mod number;

use std::{iter::Peekable};
use logos::{Lexer as LogosLexer, Logos};

pub use token::Token;

#[derive(Debug, Clone)]
/// This struct implements the lexical analyzer for Athir
/// This is currently based on the [Logos](https://docs.rs/logos/0.13.0/logos/) crate 
/// Rust regexes, on which Logos is based, do not support lookahead
/// So we have to do some extra work to ensure tokens are terminated by a delimiter
pub struct Lexer<'a> {
    inner: Peekable<LogosLexer<'a, Token>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        //! This function returns a new lexer for the given input.
        Self {
            inner: Token::lexer(input).peekable(),
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        //! This function returns the next token in the lexer.
        //! 
        //! Boolean, character, directive, dot, identifier (without vertical lines), number must
        //! be terminated by a delimiter.
        //! 

        match self.inner.peek()? {
            // If the next lexeme is a boolean, character, directive, dot, identifier (without vertical lines), number
            // then we need to check if the lexeme after it starts with a delimiter.
            Token::Boolean(_)
            | Token::Character(_)
            | Token::Dot
            | Token::Directive 
            | Token::Identifier(_)
            | Token::Number(_) => {
                let lexeme = self.inner.next()?;
                let next = self.inner.peek();

                match next {
                    None => Some(lexeme),
                    Some(next) => match next {
                        Token::Error
                        | Token::Whitespace
                        | Token::ParenClose
                        | Token::ParenOpen
                        | Token::String(_)
                        | Token::Comment
                        | Token::VerticalLineIdentifier(_)=> {
                            Some(lexeme)
                        },
                        _ => {
                            self.inner.next();
                            Some(Token::Error)
                        }
                    }
                }
            },
            Token::VerticalLineIdentifier(id) => {
                let token = Token::Identifier(id.clone());
                self.inner.next();
                Some(token)
            },
            Token::Whitespace => {
                while let Some(Token::Whitespace) = self.inner.peek() {
                    self.inner.next();
                }
                self.inner.next()
            },
            _ => self.inner.next(),    
        }
    }
}

