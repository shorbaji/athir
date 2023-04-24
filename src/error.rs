
use std::fmt;

#[derive(Debug, Clone)]
pub enum Error {
    EvalError(String),
    UnexpectedToken{depth: usize, unexpected: String, expected: String} ,
    DefinitionsBeforeExpressionsinLambda,
    EOF,
}

pub fn unexpected(depth: usize, unexpected: String, expected: String,) -> Error {
    Error::UnexpectedToken{depth, unexpected, expected}
}

impl<'a> fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Error: {:?}", self)
    }
}

impl<'a> std::error::Error for Error {}
