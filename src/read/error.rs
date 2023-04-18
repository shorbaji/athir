
use std::fmt;

#[derive(Debug)]
pub enum SyntaxErrorKind {
    UnexpectedToken{unexpected: String, expected: &'static str} ,
    DefinitionsBeforeExpressionsinLambda,
    EllipsisNotValidPatternIdentifier,
    EndOfInput,
    UnexpectedEOF,
    // EmptyBodyinLambda,
    // NotImplemented,
}

#[derive(Debug)]
pub struct SyntaxError {
    pub kind: SyntaxErrorKind,
}

impl<'a> fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Error: {:?}", self.kind)
    }
}

impl SyntaxError {
    pub fn new(kind: SyntaxErrorKind) -> Self {
        Self {
            kind: kind,
        }
    }
}

impl<'a> std::error::Error for SyntaxError {}
