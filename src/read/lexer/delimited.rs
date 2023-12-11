use super::Token;
use logos::{Lexer, Logos};
use std::iter::Peekable;
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
            Token::Boolean(_)
            | Token::Character(_)
            | Token::Dot
            | Token::Directive
            | Token::Identifier(_)
            | Token::Number(_) => {
                let token = self.inner.next()?; // Consume the token

                match self.inner.peek() {
                    Some(next) => match next {
                        Token::Error
                        | Token::Whitespace
                        | Token::ParenRight
                        | Token::ParenLeft
                        | Token::String(_)
                        | Token::Comment
                        | Token::VerticalLineIdentifier(_) => Some(token),
                        _ => {
                            self.inner.next();
                            Some(Token::Error)
                        }
                    },
                    None => Some(token),
                }
            }
            Token::VerticalLineIdentifier(id) => {
                let token = Token::Identifier(id.clone());
                self.inner.next();
                Some(token)
            }
            Token::Whitespace => {
                while let Some(Token::Whitespace) = self.inner.peek() {
                    self.inner.next();
                }
                self.inner.next()
            }
            _ => self.inner.next(),
        }
    }
}
