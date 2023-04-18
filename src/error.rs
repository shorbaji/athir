
use std::fmt;

use crate::read::Token;

#[derive(Debug)]
pub enum ErrorKind {
    UnexpectedToken{unexpected: Token, expected: &'static str} ,
    DefinitionsBeforeExpressionsinLambda,
    EllipsisNotValidPatternIdentifier,
    EndOfInput,
    EvalError,
    // EmptyBodyinLambda,
    // NotImplemented,
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
}

impl<'a> fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Error: {:?}", self.kind)
    }
}

impl Error {
    pub fn new(kind: ErrorKind) -> Self {
        Self {
            kind: kind,
        }
    }
}

impl<'a> std::error::Error for Error {}
