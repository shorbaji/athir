//! Node
//!

use std::rc::Rc;
use std::cell::RefCell;

use lazy_static::__Deref;

use crate::result::{EvalResult, VecEvalResult};
use crate::error::Error;
use crate::eval::env::Env;

#[derive(Debug, Clone)]
pub enum Object {
    Boolean(bool),
    Bytevector(Vec<Rc<RefCell<Object>>>),
    Character(char),
    _Eof,
    Identifier(Identifier),
    Null,
    Number(String),
    Pair(Rc<RefCell<Object>>, Rc<RefCell<Object>>),
    _Port,
    Procedure(Procedure),
    Quotation(Rc<RefCell<Object>>),
    String(String),
    Vector(Vec<Rc<RefCell<Object>>>),
    Unspecified,
}


impl std::fmt::Debug for Procedure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Procedure::Builtin { name, min_args, max_args, .. } => {
                write!(f, "Builtin: {:?} min: {:?} max {:?}", name, min_args, max_args)
            },
            Procedure::Lambda{env, formals, body} => {
                write!(f, "Lambda: formals: {:?} body: {:?}", formals, body)
            }
        }
    }
}

#[derive(Clone)]
pub enum Procedure {
    Builtin {
        name: &'static str,
        min_args: Option<usize>,
        max_args: Option<usize>, 
        func: fn(&[Rc<RefCell<Object>>]) -> EvalResult
    },
    Lambda {
        env: Rc<RefCell<Env>>,
        formals: Rc<RefCell<Object>>,
        body: Rc<RefCell<Object>>,
    },
}

// public methods

pub fn cons(car: Rc<RefCell<Object>>, cdr: Rc<RefCell<Object>>) -> EvalResult {
    Ok(Rc::new(RefCell::new(Object::Pair(car, cdr))))
}

pub fn car(pair: Rc<RefCell<Object>>) -> EvalResult {
    match *pair.borrow() {
        Object::Pair(ref car, _) => Ok(car.clone()),
        _ => Err(Error::EvalError("car: expected pair".to_string())),
    }
}

pub fn cdr(pair: Rc<RefCell<Object>>) -> EvalResult {
    match *pair.borrow() {
        Object::Pair(_, ref cdr) => Ok(cdr.clone()),
        _ => Err(Error::EvalError("cdr: expected pair".to_string())),
    }
}

pub fn caar(pair: Rc<RefCell<Object>>) -> EvalResult {
    car(car(pair)?)
}

pub fn cadr(pair: Rc<RefCell<Object>>) -> EvalResult {
    car(cdr(pair)?)
}

pub fn cdar(pair: Rc<RefCell<Object>>) -> EvalResult {
    cdr(car(pair)?)
}

pub fn cddr(pair: Rc<RefCell<Object>>) -> EvalResult {
    cdr(cdr(pair)?)
}

pub fn caddr(pair: Rc<RefCell<Object>>) -> EvalResult {
    car(cdr(cdr(pair)?)?)
}

pub fn cadddr(pair: Rc<RefCell<Object>>) -> EvalResult {
    car(cdr(cdr(cdr(pair)?)?)?)
}

pub fn cdddr(pair: Rc<RefCell<Object>>) -> EvalResult {
    cdr(cdr(cdr(pair)?)?)
}

pub fn cddar(pair: Rc<RefCell<Object>>) -> EvalResult {
    cdr(cdr(car(pair)?)?)
}

pub fn caadr(pair: Rc<RefCell<Object>>) -> EvalResult {
    car(car(cdr(pair)?)?)
}

pub fn cdaar(pair: Rc<RefCell<Object>>) -> EvalResult {
    cdr(car(car(pair)?)?)
}

pub fn cdadr(pair: Rc<RefCell<Object>>) -> EvalResult {
    cdr(car(cdr(pair)?)?)
}


impl Object {
    /// Creates an Objects from a Vec of Objects
    pub fn list(nodes: Vec<Rc<RefCell<Object>>>) -> EvalResult {
        let mut result = Object::Null;

        for node in nodes.into_iter().rev() {
            result = Object::Pair(node, Rc::new(RefCell::new(result)));
        }

        Ok(Rc::new(RefCell::new(result)))
    }

    pub fn list_not_null_terminated(nodes: Vec<Rc<RefCell<Object>>>, node: Rc<RefCell<Object>>) -> EvalResult {
        let mut result = node;

        for node in nodes.into_iter().rev() {
            result = Rc::new(RefCell::new(Object::Pair(node, result)));
        }

        Ok(result)
    }

    pub fn as_list(&self) -> VecEvalResult {
        let mut result = Vec::new();
        let mut current = Rc::new(RefCell::new(self.clone()));
        loop {
            match current.clone().borrow().deref() {
                Object::Pair(car, cdr) => {
                    result.push(car.clone());
                    current = cdr.clone();
                }
                _ => break,
            }
        }

        Ok(result)
    }

    // pub fn cons(&self, object: T) -> EvalResult {
    //     Ok(Box::new(Object::Pair(Box::new(self.clone()), Box::new(object))))
    // }

    // pub fn car(&self) -> EvalResult {
    //     match self {
    //         Object::Pair(car, _) => Ok(car),
    //         _ => Err(Error::EvalError("car: not a pair".to_string())),
    //     }
    // }

    // pub fn cdr(&self) -> EvalResult {
    //     match self {
    //         Object::Pair(_, cdr) => Ok(cdr),
    //         _ => Err(Error::EvalError("car: not a pair".to_string())),
    //     }
    // }

    // pub fn caar(&self) -> EvalResult {
    //     self.car()?.car()
    // }

    // pub fn cdar(&self) -> EvalResult {
    //     self.car()?.cdr()
    // }

    // pub fn cadr(&self) -> EvalResult {
    //     self.cdr()?.car()
    // }

    // pub fn cddr(&self) -> EvalResult {
    //     self.cdr()?.cdr()
    // }

    // pub fn caddr(&self) -> EvalResult {
    //     self.cddr()?.car()
    // }

    // pub fn cdadr(&self) -> EvalResult {
    //     self.cadr()?.cdr()
    // }

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

    pub fn is_definition_expr(expr: Rc<RefCell<Object>>) -> bool {
        let is_definition_keyword = match car(expr.clone()) {
            Ok(node) => is_definition_keyword(node),
            Err(_) => false,
        };
        
        is_definition_keyword || is_begin_definition_expr(expr)
    }

    fn is_definition_keyword(expr: Rc<RefCell<Object>>) -> bool {
        matches!(*expr.borrow(), 
            Object::Identifier(Identifier::Keyword(Keyword::Define))
            | Object::Identifier(Identifier::Keyword(Keyword::DefineValues))
            | Object::Identifier(Identifier::Keyword(Keyword::DefineRecordType))
            | Object::Identifier(Identifier::Keyword(Keyword::DefineSyntax))
        )
    }

    fn is_begin_keyword(expr: Rc<RefCell<Object>>) -> bool {
        matches!(*expr.borrow(), Object::Identifier(Identifier::Keyword(Keyword::Begin)))
    }

    fn is_begin_expr(expr: Rc<RefCell<Object>>) -> bool {
        match car(expr) {
            Ok(node) => is_begin_keyword(node),
            Err(_) => false,
        }
    }

    fn is_begin_definition_expr(expr: Rc<RefCell<Object>>) -> bool {    
        is_begin_expr(expr.clone()) && 
        match cdadr(expr) {
            Ok(object) => matches!(*object.borrow(), Object::Boolean(true)),
            Err(_) => false,
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