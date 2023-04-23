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
pub enum Object {
    Null,
    Boolean(bool),
    Character(char),
    String(String),
    Number(String),
    Bytevector(Vec<Box<Object>>),
    Vector(Vec<Box<Object>>),
    Quotation(Box<Object>),
    Identifier(Identifier),
    Pair(Box<Object>, Box<Object>),
}

impl From<&Token> for Object {
    fn from(value: &Token) -> Self {
        match value {
            Token::Boolean(b) => Object::Boolean(*b),
            Token::Character(c) => Object::Character(*c),
            Token::String(s) => Object::String(s.clone()),
            Token::Number(n) => Object::Number(n.clone()),
            _ => panic!("Cannot convert {:?} to NodeKind", value),
        }
    }
}

// public methods

impl Object {
    /// Creates an Objectession from a Vec of Objectessions
    pub fn list(nodes: Vec<Box<Object>>) -> ParseResult {
        let mut result = Object::Null;
        for node in nodes.into_iter().rev() {
            result = Object::Pair(node, Box::new(result));
        }

        Ok(Box::new(result))
    }

    pub fn list_not_null_terminated(nodes: Vec<Box<Object>>, node: Box<Object>) -> ParseResult {
        let mut result = *node;
        for node in nodes.into_iter().rev() {
            result = Object::Pair(node, Box::new(result));
        }

        Ok(Box::new(result))
    }

    pub fn car(&self) -> Option<&Box<Object>> {
        match self {
            Object::Pair(car, _) => Some(car),
            _ => None,
        }
    }

    pub fn cdr(&self) -> Option<&Box<Object>> {
        match self {
            Object::Pair(_, cdr) => Some(cdr),
            _ => None,
        }
    }

    pub fn cadr(&self) -> Option<&Box<Object>> {
        match self.cdr() {
            Some(cdr) => cdr.car(),
            None => None,
        }
    }

    pub fn cdadr(&self) -> Option<&Box<Object>> {
        match self.cadr() {
            Some(cadr) => cadr.cdr(),
            None => None,
        }
    }

    pub fn _is_null(&self) -> bool {
        match self {
            Object::Null => true,
            _ => false,
        }
    }

    fn _cons(&self, object: Object) -> Object {
        Object::Pair(Box::new(self.clone()), Box::new(object))
    }


}

pub type Expr = Object;

impl Expr {
    /// Checks if the Objectession is a define Objectession, i.e 
    /// (define ...), (define-values ...), (define-record-type ...), (define-syntax ...)
    /// or (begin (define ...) ...))
    pub fn is_definition_expr(&self) -> bool {
        let is_definition_keyword = match self.car() {
            Some(node) => node.is_definition_keyword(),
            None => false,
        };
        
        is_definition_keyword || self.is_begin_definition_expr()
    }

}
// private methods

#[doc(hidden)]
impl Expr {
    fn is_definition_keyword(&self) -> bool {
        matches!(self, 
            Object::Identifier(Identifier::Keyword(Keyword::Define))
            | Object::Identifier(Identifier::Keyword(Keyword::DefineValues))
            | Object::Identifier(Identifier::Keyword(Keyword::DefineRecordType))
            | Object::Identifier(Identifier::Keyword(Keyword::DefineSyntax))
        )
    }

    fn is_begin_keyword(&self) -> bool {
        matches!(self, Object::Identifier(Identifier::Keyword(Keyword::Begin)))
    }

    fn is_begin_expr(&self) -> bool {
        match self.car() {
            Some(node) => node.is_begin_keyword(),
            None => false,
        }
    }

    fn is_begin_definition_expr(&self) -> bool {    
        self.is_begin_expr() && 
        match self.cdadr() {
            Some(object) => matches!(&**object, Object::Boolean(true)),
            None => false,
        }
    }

}
