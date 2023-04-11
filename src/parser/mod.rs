
use crate::lexer::Lexer;
use crate::lexer::Token;
use logos::Logos;
use std::iter::Peekable;

#[derive(Debug)]
enum Expression {
    Literal,
    Identifier,
    Compound,
}

#[derive(Debug)]
enum Kind {
    Expression(Expression),
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

        let (variant, children) = match token {
            Token::Boolean(_)
            | Token::Character(_)
            | Token::String(_)
            | Token::Number(_) => { (Expression::Literal, vec!(self.literal()?)) }
            Token::Identifier(_) => { (Expression::Identifier, vec!(self.identifier()?)) }
            Token::ParenOpen => {
                self.lexer.next();
                println!("PEEK: {:?}", self.peek());
                while let Some(token) = self.peek() {
                    if token == &Token::ParenClose {
                        self.lexer.next();
                        break;
                    } else {
                        children.push(self.expr()?);
                    }
                }
                (Expression::Compound, children)
            }
            _ => return Err("Unexpected token"),
        };

        Ok(Box::new(Node {
            kind: Kind::Expression(variant),
            children: children,
        }))
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
