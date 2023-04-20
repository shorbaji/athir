
use std::fmt;

#[derive(Debug, Clone)]
pub enum SyntaxErrorKind {
    UnexpectedToken{depth: usize, unexpected: String, expected: String} ,
    DefinitionsBeforeExpressionsinLambda,
    EOF,
}

#[derive(Debug, Clone)]
pub struct SyntaxError {
    pub kind: SyntaxErrorKind,
    pub child: Option<Box<SyntaxError>>,
}

pub fn unexpected(depth: usize, unexpected: String, expected: String, child: Option<Box<SyntaxError>>) -> SyntaxError {
    SyntaxError {
        kind: SyntaxErrorKind::UnexpectedToken{depth, unexpected, expected},
        child: child,
    }
}

impl<'a> fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Error: {:?}", self)
    }
}

impl<'a> std::error::Error for SyntaxError {}
