
use crate::lexer::Lexer;
use crate::lexer::token::Token;
use logos::Logos;
use std::iter::Peekable;

#[derive(Debug)]
enum Kind {
    Expression,
    Literal,
    Identifier,
}

#[derive(Debug)]
struct Node {
    kind: Kind,
    children: Vec<Node>,
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

        match token {
            Token::Boolean(_)
            | Token::Character(_)
            | Token::String(_)
            | Token::Number => self.literal(),
            Token::Identifier(_) => self.identifier(),
            _ => Err("Unexpected token"),
        }
    }

    fn identifier(&mut self) -> Result<Box<Node>, ParseError> {
        Ok(Box::new(Node {
            kind: Kind::Identifier,
            children: vec![],
        }))
    }

    fn literal(&mut self) -> Result<Box<Node>, ParseError> {
        Ok(Box::new(Node {
            kind: Kind::Literal,
            children: vec![],
        }))
    }

}

// struct Parser {
//     stack: Vec
// }
// #[derive(Debug)]
// pub enum Expr {
//     Literal(Lexeme),
//     Identifier(Lexeme),
// }

// pub fn parse(input: &str) -> Option<Expr> {
//     let mut lex = Lexeme::delimited_lexer(input).peekable();
//     expr(lex)
// }

// fn expr(mut lexer: Peekable<DelimitedLexer>) -> Option<Expr> {

//     let lexeme = lexer.peek()?;

//     match lexeme {
//         Lexeme::Boolean
//         | Lexeme::Character
//         | Lexeme::String
//         | Lexeme::Number => Some(Expr::Literal(literal(lexer)?)),
//         Lexeme::Identifier =>  Some(Expr::Identifier(identifier(lexer)?)),
//         _ => None
//     }
// }

// type Literal = Lexeme;

// impl Literal {
//     fn from(lexeme: Lexeme) -> Self {
//         lexeme
//     }
// }

// fn literal(mut lexer: Peekable<DelimitedLexer>) -> Option<Lexeme> {
//     Some(Literal::from(lexer.next()?))
// }

// type Identifier = Lexeme;

// fn identifier(mut lexer: Peekable<DelimitedLexer>) -> Option<Identifier> {
//     Some(Identifier::from(lexer.next()?))
// }

// fn compound(mut lexer: Peekable<DelimitedLexer>) {
//     let car = expr(lexer).unwrap();

//     let next = lexer.peek()?;

//     match lexer.peek()? {
//         Lexeme::ParenClose => {
//             lexer.next();
//             return;
//         },
//         _ => {
//             return;
//         }
//     }

// }
