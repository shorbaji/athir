use super::Token;
use std::iter::Peekable;
use logos::{Lexer, Logos};
#[derive(Debug, Clone)]

/// DelimitedLexer
/// 
/// An abstraction over the lexical analyzer created by[Logos](https://docs.rs/logos/0.13.0/logos/) crate 
/// Rust regexes, on which Logos is based, do not support lookahead
/// so we have to do some extra work to ensure tokens are terminated by a delimiter
pub struct DelimitedLexer<'a> {
    inner: Peekable<Lexer<'a, Token>>,
}

impl<'a> DelimitedLexer<'a> {
    /// This function returns a new lexer for the given input.
    /// It creates a Logos lexer and wraps it in a Peekable iterator.
    pub fn new(input: &'a str) -> Self {
        Self {
            inner: Token::lexer(input).peekable(),
        }
    }
}

impl Iterator for DelimitedLexer<'_> {
    type Item = Token;

    /// This function returns the next token in the lexer.
    /// 
    /// Boolean, character, directive, dot, identifier (without vertical lines), number must
    /// be terminated by a delimiter.
    /// 
    fn next(&mut self) -> Option<Self::Item> {

        // If the next token is a boolean, character, directive, dot, identifier (without vertical lines), number
        // then we need to check if the lexeme after it starts with a delimiter.
        match self.inner.peek()? {
            Ok(Token::Boolean(_))
            | Ok(Token::Character(_))
            | Ok(Token::Dot)
            | Ok(Token::Directive)
            | Ok(Token::Identifier(_))
            | Ok(Token::Number(_)) => {
                let token = self.inner.next()?; // Consume the token

                match self.inner.peek() {
                    Some(next) => match next {
                        Err(_)
                        | Ok(Token::Whitespace)
                        | Ok(Token::ParenRight)
                        | Ok(Token::ParenLeft)
                        | Ok(Token::String(_))
                        | Ok(Token::Comment)
                        | Ok(Token::VerticalLineIdentifier(_))=> Some(token.unwrap()),
                        _ => { self.inner.next(); None
                        }
                    },
                    None => Some(token.unwrap()),
                }
            },
            Ok(Token::VerticalLineIdentifier(id)) => {
                let token = Token::Identifier(id.clone());
                self.inner.next();
                Some(token)
            },
            Ok(Token::Whitespace) => {
                while let Some(Ok(Token::Whitespace)) = self.inner.peek() {
                    self.inner.next();
                }
                match self.inner.next() {
                    Some(Ok(token)) => Some(token),
                    _ => None,
                }
            },
            _ => match self.inner.next() {
                Some(Ok(token)) => Some(token),
                _ => None,
            }    
        }
    }
}    
