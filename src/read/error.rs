
use std::fmt;

#[derive(Debug)]
pub enum SyntaxError {
    UnexpectedToken{unexpected: String, expected: &'static str} ,
    DefinitionsBeforeExpressionsinLambda,
    EOF,
}

impl<'a> fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Error: {:?}", self)
    }
}

impl<'a> std::error::Error for SyntaxError {}
