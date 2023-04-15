
pub mod lexer;
pub mod parser;

use parser::{Parser, Node};
use crate::error::Error;

pub struct Reader<'a> {
    parser: Parser<'a>,
}

impl<'a> Reader<'a> {
    pub fn new(s: &'a str) -> Self {
        Self {
            parser: Parser::new(s),
        }
    }

    pub fn read(&mut self) -> Result<Box<Node>, Error> {
        self.parser.parse()
    }
}
