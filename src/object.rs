//! Node
//!

use crate::result::{AthirResult, VecResult};
use crate::error::Error;
use crate::eval::env::Env;
#[derive(Debug, Clone)]
pub enum Object {
    Boolean(bool),
    Bytevector(Vec<Box<Object>>),
    Character(char),
    _Eof,
    Identifier(Identifier),
    Null,
    Number(String),
    Pair(Box<Object>, Box<Object>),
    _Port,
    Procedure(Procedure),
    Quotation(Box<Object>),
    String(String),
    Vector(Vec<Box<Object>>),
    Unspecified,
}

use std::rc::Rc;
use std::cell::RefCell;

#[derive(Clone)]
pub struct Builtin {
    pub name: &'static str,
    pub min_args: Option<usize>,
    pub max_args: Option<usize>,
    pub func: fn(&[Box<Object>]) -> AthirResult,
}

impl Builtin {
    pub fn new(name: &'static str, min_args: Option<usize>, max_args: Option<usize>, func: fn(&[Box<Object>]) -> AthirResult) -> Builtin {
        Builtin {
            name,
            min_args,
            max_args,
            func,
        }
    }
}

impl std::fmt::Debug for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Builtin: {:?} min: {:?} max {:?}", self.name, self.min_args, self.max_args)
    }
}

#[derive(Clone, Debug)]
pub enum Procedure {
    Builtin(Builtin),
    Lambda(Rc<RefCell<Env>>, Box<Object>, Box<Object>),
}

// public methods

impl Object {
    /// Creates an Objects from a Vec of Objects
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

    pub fn as_list(&self) -> VecResult {
        let mut result = Vec::new();
        let mut current = self;
        loop {
            match current {
                Object::Pair(car, cdr) => {
                    result.push(car.clone());
                    current = &**cdr;
                }
                _ => break,
            }
        }

        Ok(result)
    }

    pub fn cons(&self, object: Object) -> AthirResult {
        Ok(Box::new(Object::Pair(Box::new(self.clone()), Box::new(object))))
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

    pub fn caar(&self) -> Result<&Box<Object>, Error> {
        self.car()?.car()
    }

    pub fn cdar(&self) -> Result<&Box<Object>, Error> {
        self.car()?.cdr()
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

    pub fn is_null(&self) -> bool {
        match self {
            Object::Null => true,
            _ => false,
        }
    }

    pub fn is_false(&self) -> bool {
        matches!(self, Object::Boolean(false) | Object::Null)
    }
    
    pub fn is_true(&self) -> bool {
        !self.is_false()
    }

    pub fn is_boolean(&self) -> bool {
        matches!(self, Object::Boolean(_))
    }

    pub fn is_bytevector(&self) -> bool {
        matches!(self, Object::Bytevector(_))
    }

    pub fn is_char(&self) -> bool {
        matches!(self, Object::Character(_))
    }

    pub fn is_eof_object(&self) -> bool {
        matches!(self, Object::_Eof)
    }

    pub fn is_number(&self) -> bool {
        matches!(self, Object::Number(_))
    }

    pub fn is_pair(&self) -> bool {
        matches!(self, Object::Pair(_, _))
    }

    pub fn is_procedure(&self) -> bool {
        matches!(self, Object::Procedure(_))
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Object::String(_))
    }

    pub fn is_symbol(&self) -> bool {
        matches!(self, Object::Identifier(_))
    }

    pub fn is_vector(&self) -> bool {
        matches!(self, Object::Vector(_))
    }

    pub fn is_port(&self) -> bool {
        matches!(self, Object::_Port)
    }

}


impl Object {
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
impl Object {
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


impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        if self.is_number() && other.is_number()  {
            let a = match self {
                Object::Number(n) => n,
                _ => panic!("Cannot compare non-number object"),
            };

            let b = match other {
                Object::Number(n) => n,
                _ => panic!("Cannot compare non-number object"),
            };

            a == b
        } else {
            false
        }
    }
}