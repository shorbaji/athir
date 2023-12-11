use std::iter::Peekable;
use crate::read::lexer::{Token, Lexer};
use crate::read::Reader;

#[derive(Debug, Clone)]
pub enum PortKind {
    Stdin,
    Stdout,
    String,

}
#[derive(Debug, Clone)]
pub struct Port {
    buffer: String,
    tokens: Peekable<std::vec::IntoIter<Token>>,
    kind: PortKind,
}

impl Lexer for Port {
    fn read_line(&mut self) -> Option<String> {
        Port::read_line(self)
    }

    fn get_tokens(&mut self) -> &mut Peekable<std::vec::IntoIter<Token>> {
        &mut self.tokens
    }

    fn set_tokens(&mut self, tokens: Peekable<std::vec::IntoIter<Token>>) {
        self.tokens = tokens;
    }
}

impl Reader for Port {
    fn get_next_token(&mut self) -> Option<Token> {
        Lexer::get_next_token(self)
    }

    fn peek_next_token(&mut self) -> Option<Token> {
        Lexer::peek_next_token(self)
    }
}

impl Port {
    pub fn new(kind: PortKind) -> Self {
        Self {
            buffer: String::new(),
            tokens: vec!().into_iter().peekable(), 
            kind,
        }
    }

    pub fn open_input_string(s: String) -> Self {
        Self {
            buffer: s,
            tokens: vec!().into_iter().peekable(), 
            kind: PortKind::String,
        }
    }

    pub fn stdin() -> Self {
        Self::new(PortKind::Stdin)
    }

    pub fn stdout() -> Self {
        Self::new(PortKind::Stdout)
    }

    pub fn peek_char(&mut self) -> Option<char> {

        if self.buffer.is_empty() {
            if let PortKind::Stdin = self.kind {
                match std::io::stdin().read_line(&mut self.buffer) {
                    Ok(_) => {},
                    Err(_) => return None,
                }
            }
        }

        self.buffer.chars().next()
    }

    pub fn read_char(&mut self) -> Option<char> {
        let c = self.peek_char();

        if c.is_some() {
            self.buffer.remove(0);
        }
        c
    }

    pub fn read_line(&mut self) -> Option<String> {
        let _ = self.peek_char();
        match self.buffer.lines().next() {
            Some(s) => {
                let result = String::from(s);
                self.buffer = self.buffer[s.len()..].to_string(); // we need to remove the line we just read
                if !self.buffer.is_empty() {
                    self.buffer.remove(0); // we need to remove the newline character
                }
                Some(result)
            },
            None => None,
        }
    }

}

impl From<&str> for Port {
    fn from(s: &str) -> Self {
        Self::open_input_string(s.to_string())
    }
}

impl From<String> for Port {
    fn from(s: String) -> Self {
        Self::open_input_string(s)
    }
}
