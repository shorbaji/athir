//! Node
//!

use crate::read::lexer::Token;
use std::convert::From;
use crate::read::ParseResult;

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    And,
    Begin,
    Comma,
    CommaAt,
    CondExpand,
    Define,
    DefineLibrary,
    DefineRecordType,
    DefineSyntax,
    DefineValues,
    Ellipsis,
    Else,
    Except,
    Export,
    If,
    Import,
    Include,
    IncludeCi,
    IncludeLibraryDeclarations,
    Lambda,
    LetSyntax,
    LetrecSyntax,
    Not,
    Only,
    Or,
    Prefix,
    Rename,
    Quasiquote,
    Quote,
    Set,
    SyntaxRules,
    Underscore,
    Unquote,
}

impl From<String> for Keyword {
    fn from(value: String) -> Self {
        match value.as_str() {
            "and" => Keyword::And,
            "begin" => Keyword::Begin,
            "comma" => Keyword::Comma,
            "comma-at" => Keyword::CommaAt,
            "cond-expand" => Keyword::CondExpand,
            "define" => Keyword::Define,
            "define-library" => Keyword::DefineLibrary,
            "define-record-type" => Keyword::DefineRecordType,
            "define-values" => Keyword::DefineValues,
            "define-syntax" => Keyword::DefineSyntax,
            "else" => Keyword::Else,
            "except" => Keyword::Except,
            "export" => Keyword::Export,
            "..." => Keyword::Ellipsis,
            "if" => Keyword::If,
            "import" => Keyword::Import,
            "include" => Keyword::Include,
            "include-ci" => Keyword::IncludeCi,
            "include-library-declarations" => Keyword::IncludeLibraryDeclarations,
            "lambda" => Keyword::Lambda,
            "let-syntax" => Keyword::LetSyntax,
            "letrec-syntax" => Keyword::LetrecSyntax,
            "not" => Keyword::Not,
            "only" => Keyword::Only,
            "or" => Keyword::Or,
            "prefix" => Keyword::Prefix,
            "quasiquote" => Keyword::Quasiquote,
            "quote" => Keyword::Quote,
            "rename" => Keyword::Rename,
            "set!" => Keyword::Set,
            "syntax-rules" => Keyword::SyntaxRules,
            "_" => Keyword::Underscore,
            "unquote" => Keyword::Unquote,
            _ => panic!("Cannot convert {} to Keyword", value),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Identifier {
    Keyword(Keyword),
    Variable(String),
}

impl From<&Token> for Identifier {
    fn from(value: &Token) -> Self {
        match value {
            Token::Identifier(s) => Identifier::Variable(s.clone()),
            _ => panic!("Cannot convert {:?} to NodeKind", value),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Boolean(bool),
    Character(char),
    String(String),
    Number(String),
    Bytevector(Vec<Box<Expr>>),
    Vector(Vec<Box<Expr>>),
    Quotation(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Null,
    Literal(Literal),
    Identifier(Identifier),
    Pair(Box<Expr>, Box<Expr>),
}

impl From<&Token> for Literal {
    fn from(value: &Token) -> Self {
        match value {
            Token::Boolean(b) => Literal::Boolean(*b),
            Token::Character(c) => Literal::Character(*c),
            Token::String(s) => Literal::String(s.clone()),
            Token::Number(n) => Literal::Number(n.clone()),
            _ => panic!("Cannot convert {:?} to NodeKind", value),
        }
    }
}

// public methods

impl Expr {
    /// Creates an expression from a Vec of expressions
    pub fn list(nodes: Vec<Box<Expr>>) -> ParseResult {
        let mut result = Expr::Null;
        for node in nodes.into_iter().rev() {
            result = Expr::Pair(node, Box::new(result));
        }

        Ok(Box::new(result))
    }

    /// Checks if the expression is a define expression, i.e 
    /// (define ...), (define-values ...), (define-record-type ...), (define-syntax ...)
    /// or (begin (define ...) ...))
    pub fn is_definition_expr(&self) -> bool {
        
        match self.car() {
            Some(node) => node.is_definition_keyword() || node.is_begin_definition_expr(),
            None => false,
        }
    }
}

// private methods

#[doc(hidden)]
impl Expr {
    fn car(&self) -> Option<&Box<Expr>> {
        match self {
            Expr::Pair(car, _) => Some(car),
            _ => None,
        }
    }

    fn cdr(&self) -> Option<&Box<Expr>> {
        match self {
            Expr::Pair(_, cdr) => Some(cdr),
            _ => None,
        }
    }

    fn cadr(&self) -> Option<&Box<Expr>> {
        match self.cdr() {
            Some(cdr) => cdr.car(),
            None => None,
        }
    }

    fn _is_null(&self) -> bool {
        match self {
            Expr::Null => true,
            _ => false,
        }
    }

    fn _cons(&self, expr: Expr) -> Expr {
        Expr::Pair(Box::new(self.clone()), Box::new(expr))
    }

    fn is_definition_keyword(&self) -> bool {
        matches!(self, 
            Expr::Identifier(Identifier::Keyword(Keyword::Define))
            | Expr::Identifier(Identifier::Keyword(Keyword::DefineValues))
            | Expr::Identifier(Identifier::Keyword(Keyword::DefineRecordType))
            | Expr::Identifier(Identifier::Keyword(Keyword::DefineSyntax))
        )
    }

    fn is_begin_keyword(&self) -> bool {
        matches!(self, Expr::Identifier(Identifier::Keyword(Keyword::Begin)))
    }

    fn is_begin_expr(&self) -> bool {
        match self.car() {
            Some(node) => node.is_begin_keyword(),
            None => false,
        }
    }

    fn is_begin_definition_expr(&self) -> bool {        
        self.is_begin_expr() && 

        match self.cdr() {
            Some(cdr) => match cdr.cadr() {
                Some(node) => matches!(**node, Expr::Literal(Literal::Boolean(true))),
                None => false,
            },
            None => false,
        }
    }


}