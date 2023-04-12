
use crate::lexer::Lexer;
use crate::lexer::Token;
use std::iter::Peekable;
use std::iter::from_fn;

#[derive(Debug)]
enum Kind {
    Expression,
    ProcedureCall,
    Operands,
    Operator,
    VariableDefinition,
    FunctionDefinition,
    Conditional,
    Lambda,
    Body,
    Assignment,
    Literal,
    Identifier,
    Formals,
    DefFormals
}

#[derive(Debug)]
enum Node {
    Inner(Kind, Vec<Box<Node>>),
    Leaf(Kind, Token),
}

// impl Node {
//     fn inner(kind: Kind, children: Vec<Box<Node>>) -> Self {
//         Self::Inner(kind, children)
//     }
    
//     fn leaf(kind: Kind, token: Token) -> Self {
//         Self::Leaf(kind, token)
//     }
// }

#[derive(Debug)]
pub struct ParseTree {
    root: Box<Node>
}

type ParseError = &'static str;

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self { Self { lexer: Lexer::new(input).peekable() } }
    
    pub fn parse(&mut self) -> Result<ParseTree, ParseError> { Ok(ParseTree { root: self.expr()? }) }
    
    fn peek(&mut self) -> Option<&Token> { self.lexer.peek() }
    
    fn node(&mut self, kind: Kind, children: Vec<Box<Node>>) -> Result<Box<Node>, ParseError> {
        Ok(Box::new(Node::Inner(kind, children)))
    }
    
    fn leaf(&mut self, kind: Kind) -> Result<Box<Node>, ParseError> {
        Ok(Box::new(Node::Leaf(kind, self.lexer.next().unwrap())))
    }
}

impl<'a> Parser<'a> {
    fn keyword(&mut self, keyword: &str) -> Result<(), ParseError> {
        let token = self.peek().ok_or("Error at end of input")?;
        
        match token {
            Token::Identifier(s) if keyword == s => {
                self.lexer.next(); // consume keyword
                Ok(())
            },
            _ => Err("Unexpected token. Expected keyword"),
        }
        
    }
    
    fn paren_open(&mut self) -> Result<(), ParseError> {
        match self.peek().ok_or("Error at end of input")? {
            Token::ParenOpen => {
                self.lexer.next(); // consume ParenOpen
                Ok(())
            },
            _ => Err("syntax error: expected ParenOpen"),
        }
    }
    
    fn paren_close(&mut self) -> Result<(), ParseError> {
        match self.peek().ok_or("Error at end of input")? {
            Token::ParenClose => {
                self.lexer.next(); // consume ParenClose
                Ok(())
            },
            _ => Err("syntax error: expected ParenClose"),
        }
    }
}

impl<'a> Parser<'a> {

    fn expr(&mut self) -> Result<Box<Node>, ParseError> {
        self.literal()
        .or_else(|_| self.identifier())
        .or_else(|_| self.compound_expr())
        .and_then(|node| self.node(Kind::Expression, vec![node]))
    }
    
    fn expr_list(&mut self) -> Result<Vec<Box<Node>>, ParseError> {
        Ok(from_fn(|| self.expr().ok()).collect())
    }
    
    fn identifier(&mut self) -> Result<Box<Node>, ParseError> {        
        match self.peek().ok_or("Error at end of input")? {
            Token::Identifier(_) => self.leaf(Kind::Identifier),
            _ => Err("Unexpected token. Expected identifier"),
        }        
    }

    fn identifier_list(&mut self) -> Result<Vec<Box<Node>>, ParseError> {
        Ok(from_fn(|| self.identifier().ok()).collect())
    }
    
    fn literal(&mut self) -> Result<Box<Node>, ParseError> {        
        match self.peek().ok_or("Error at end of input")? {
            Token::Boolean(_)
            | Token::Character(_)
            | Token::String(_)
            | Token::Number(_) => self.leaf(Kind::Literal),
            _ => return Err("Unexpected token. Expected literal"),
        }
    }
    
    fn compound_expr(&mut self) -> Result<Box<Node>, ParseError> {
        self.paren_open().and_then(|_| 
            self.definition()
            .or_else(|_| self.iff())
            .or_else(|_| self.lambda())
            .or_else(|_| self.set())
            .or_else(|_| self.procedure_call())
            .or(Err("Unexpected token.")))
    }
    
    fn definition(&mut self) -> Result<Box<Node>, ParseError> {
        self.keyword("define").and_then(|_| 
            self.variable_definition().or_else(|_|
                self.function_definition())
        )
    }
        
    fn variable_definition(&mut self) -> Result<Box<Node>, ParseError> {
        self.identifier().and_then(|id| 
            self.expr().and_then(|expr| 
                self.paren_close().and_then(|_| 
                    self.node(Kind::VariableDefinition, vec![id, expr]))))
    }
        
    fn function_definition(&mut self) -> Result<Box<Node>, ParseError> {
        self.paren_open().and_then(|_| 
            self.identifier().and_then(|id| 
                self.def_formals().and_then(|formals| 
                    self.paren_close().and_then(|_| 
                        self.body().and_then(|body| 
                            self.paren_close().and_then(|_| 
                                self.node(Kind::FunctionDefinition, vec![id, formals, body])))))))
    } 

    fn def_formals(&mut self) -> Result<Box<Node>, ParseError> {
        self.identifier_list().and_then(|ids| self.node(Kind::DefFormals, ids))
    }

    fn lambda(&mut self) -> Result<Box<Node>, ParseError> {
        self.keyword("lambda").and_then(|_| 
            self.formals().and_then(|formals|
                self.body().and_then(|body| 
                    self.paren_close().and_then(|_| 
                        self.node(Kind::Lambda, vec![formals, body])))))
    }

    fn formals(&mut self) -> Result<Box<Node>, ParseError> {
        self.identifier().and_then(|id|
            self.node(Kind::Formals, vec![id]))
            .or_else(|_|
                self.paren_open().and_then(|_|
                    self.identifier_list().and_then(|ids|
                        self.paren_close().and_then(|_|
                            self.node(Kind::Formals, ids)))))
     }

    fn body(&mut self) -> Result<Box<Node>, ParseError> {
        let mut children: Vec<Box<Node>> = Vec::new();
        Ok(from_fn(|| self.expr().ok()).collect())
            .and_then(|exprs| self.node(Kind::Body, exprs))        
    }

    fn set(&mut self) -> Result<Box<Node>, ParseError> {
        self.keyword("set!").and_then(|_|
            self.identifier().and_then(|id| 
                self.expr().and_then(|expr| 
                    self.paren_close().and_then(|_| 
                        self.node(Kind::Assignment, vec![id, expr])))))        
    }
    
    fn iff(&mut self) -> Result<Box<Node>, ParseError> {
        self.keyword("if").and_then(|_| 
            self.expr().and_then(|test| 
                self.expr().and_then(|consequent| {
                    match self.peek().ok_or("Error at end of input")? {
                        Token::ParenClose => {
                            self.lexer.next(); // consume ParenClose
                            return self.node(Kind::Conditional, vec![test, consequent])
                        },
                        _ => self.expr().and_then(|alternative| 
                                self.paren_close().and_then(|_| 
                                    self.node(Kind::Conditional, vec![test, consequent, alternative]))),    
                    }
                })))}        

    fn procedure_call(&mut self) -> Result<Box<Node>, ParseError> {
        self.operator().and_then(|operator| 
            self.operands().and_then(|operands|
                self.paren_close().and_then(|_| {
                    self.node(Kind::ProcedureCall, vec![operator, operands])    
                })))
    }

    fn operator(&mut self) -> Result<Box<Node>, ParseError> {
        self.expr().and_then(|expr|
            self.node(Kind::Operator, vec![expr]))
    }

    fn operands(&mut self) -> Result<Box<Node>, ParseError> {
        self.expr_list().and_then(|exprs|
            self.node(Kind::Operands, exprs))
    }

}
