//! Node
//!

use crate::AthirResult;
use crate::error::Error;
use crate::eval::Env;
#[derive(Debug, Clone)]
pub enum Object {
    Boolean(bool),
    Bytevector(Vec<Box<Object>>),
    Character(char),
    Identifier(Identifier),
    Null,
    Number(String),
    Pair(Box<Object>, Box<Object>),
    Procedure(Procedure),
    Quotation(Box<Object>),
    String(String),
    Vector(Vec<Box<Object>>),
    Unspecified,
}

use std::rc::Rc;
use std::cell::RefCell;

#[derive(Clone)]
pub enum Procedure {
    Builtin(usize, fn(&[Box<Object>], &mut Env) -> AthirResult),
    Lambda(Rc<RefCell<Env>>, Box<Object>, Box<Object>),
}

impl std::fmt::Debug for Procedure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Procedure::Builtin(_, _) => write!(f, "Builtin"),
            Procedure::Lambda(_, _, _) => write!(f, "Lambda"),
        }
    }
}
// public methods

impl Object {
    /// Creates an Objectession from a Vec of Objectessions
    pub fn list(nodes: Vec<Box<Object>>) -> AthirResult {
        let mut result = Object::Null;
        for node in nodes.into_iter().rev() {
            result = Object::Pair(node, Box::new(result));
        }

        Ok(Box::new(result))
    }

    pub fn list_not_null_terminated(nodes: Vec<Box<Object>>, node: Box<Object>) -> AthirResult {
        let mut result = *node;
        for node in nodes.into_iter().rev() {
            result = Object::Pair(node, Box::new(result));
        }

        Ok(Box::new(result))
    }

    pub fn car(&self) -> Result<&Box<Object>, Error> {
        match self {
            Object::Pair(car, _) => Ok(car),
            _ => Err(Error::EvalError("car: not a pair".to_string())),
        }
    }

    pub fn cdr(&self) -> Result<&Box<Object>, Error> {
        match self {
            Object::Pair(_, cdr) => Ok(cdr),
            _ => Err(Error::EvalError("car: not a pair".to_string())),
        }
    }

    pub fn cadr(&self) -> Result<&Box<Object>, Error> {
        self.cdr()?.car()
    }

    pub fn cddr(&self) -> Result<&Box<Object>, Error> {
        self.cdr()?.cdr()
    }

    pub fn caddr(&self) -> Result<&Box<Object>, Error> {
        self.cddr()?.car()
    }

    pub fn cdadr(&self) -> Result<&Box<Object>, Error> {
        self.cadr()?.cdr()
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


pub type Expr = Object;

impl Expr {
    /// Checks if the Objectession is a define Objectession, i.e 
    /// (define ...), (define-values ...), (define-record-type ...), (define-syntax ...)
    /// or (begin (define ...) ...))
    pub fn is_definition_expr(&self) -> bool {
        let is_definition_keyword = match self.car() {
            Ok(node) => node.is_definition_keyword(),
            Err(_) => false,
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
            Ok(node) => node.is_begin_keyword(),
            Err(_) => false,
        }
    }

    fn is_begin_definition_expr(&self) -> bool {    
        self.is_begin_expr() && 
        match self.cdadr() {
            Ok(object) => matches!(&**object, Object::Boolean(true)),
            Err(_) => false,
        }
    }

    pub fn is_false(&self) -> bool {
        matches!(self, Object::Boolean(false) | Object::Null)
    }
    
    pub fn is_true(&self) -> bool {
        !self.is_false()
    }

}
