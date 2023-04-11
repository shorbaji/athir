
use crate::lexer::Lexer;
use crate::lexer::Token;
use std::iter::Peekable;

#[derive(Debug)]
enum ExpressionKind {
    ProcedureCall,
    Definition,
}

#[derive(Debug)]
enum Kind {
    Expression(ExpressionKind),
    Literal(Token),
    Identifier(Token),
}

#[derive(Debug)]
struct Node {
    kind: Kind,
    children: Vec<Box<Node>>,
}

#[derive(Debug)]
pub struct ParseTree {
    root: Box<Node>,
}

impl ParseTree {
    fn new(nt: Kind) -> Self {
        Self {
            root: Box::new(Node {
                kind: nt,
                children: Vec::new(),
            }),
        }
    }
}

type ParseError = &'static str;

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            lexer: Lexer::new(input).peekable(),
        }
    }

    fn peek(&mut self) -> Option<&Token> {
        self.lexer.peek()
    }

    pub fn parse(&mut self) -> Result<ParseTree, ParseError> { 
        Ok(ParseTree { root: self.expr()? })
    }

    fn expr(&mut self) -> Result<Box<Node>, ParseError> {
        let token = self.peek()
                    .ok_or("Error at end of input")
                    ?;

        let mut children: Vec<Box<Node>> = Vec::new();

        match token {
            Token::Boolean(_)
            | Token::Character(_)
            | Token::String(_)
            | Token::Number(_) => self.literal(), 
            Token::Identifier(_) => self.identifier(),
            Token::ParenOpen => self.compound_expr(),
            _ => return Err("Unexpected token"),
        }
    }

    fn identifier(&mut self) -> Result<Box<Node>, ParseError> {
        Ok(Box::new(Node {
            kind: Kind::Identifier(self.lexer.next().unwrap()),
            children: vec![],
        }))
    }

    fn literal(&mut self) -> Result<Box<Node>, ParseError> {
        Ok(Box::new(Node {
            kind: Kind::Literal(self.lexer.next().unwrap()),
            children: vec![],
        }))
    }

    fn compound_expr(&mut self) -> Result<Box<Node>, ParseError> {
        let mut children: Vec<Box<Node>> = Vec::new();

        let _parenopen = self.lexer.next(); // consume ParenOpen

        let next = self.peek().ok_or("Error at end of input")?;

        match next {
            Token::Identifier(s) if matches!(s.as_str(), "define") => self.definition(),
            Token::Identifier(s) if matches!(s.as_str(), "lambda") => {
                Err("don't know how to parse these yet")
            },
            Token::Identifier(s) if matches!(s.as_str(), "if") => {
                Err("don't know how to parse these yet")
            },
            Token::Identifier(s) if matches!(s.as_str(), "set!") => {
                Err("don't know how to parse these yet")
            },
            Token::Identifier(s) if matches!(s.as_str(), "quote") => {
                Err("don't know how to parse these yet")
            },
            _ => {
                while let Some(token) = self.peek() {
                    if token == &Token::ParenClose {
                        self.lexer.next(); // consume ParenClose
                        break;
                    } else {
                        children.push(self.expr()?);
                    }
                }

                Ok(Box::new(Node {
                    kind: Kind::Expression(ExpressionKind::ProcedureCall),
                    children: children,
                }))
            }
        }
    }

    fn definition(&mut self) -> Result<Box<Node>, ParseError> {
        let _keyword = self.lexer.next(); // consume define keyword

        let next = self.peek().ok_or("Error at end of input")?;

        match next {
            Token::Identifier(s) => self.variable_definition(),
            Token::ParenOpen => self.function_definition(),
            _ => Err("syntax error: malformed define"),
        }
    }

    fn variable_definition(&mut self) -> Result<Box<Node>, ParseError> {
        let identifier = self.identifier()?; // consume identifier

        let expr = self.expr()
                    .or(Err("syntax error: malformed define : expected expr"))?;

        let next = self.peek().ok_or("Error at end of input")?;

        match next {
            Token::ParenClose => {
                self.lexer.next(); // consume ParenClose
                Ok(Box::new(Node {
                    kind: Kind::Expression(ExpressionKind::Definition),
                    children: vec!(identifier, expr),
                }))
            },
            _ => Err("syntax error: malformed define : expected close parenthesis after expresssion"),
        }
    }

    fn function_definition(&mut self) -> Result<Box<Node>, ParseError> {
        let _parenopen = self.lexer.next(); // consume ParenOpen
        Err("don't know how to define functions yet")
    }

}
