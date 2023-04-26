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
            Procedure::Lambda{env: _, formals, body} => {
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
        func: fn(Rc<RefCell<Object>>) -> EvalResult
    },
    Lambda {
        env: Rc<RefCell<Env>>,
        formals: Rc<RefCell<Object>>,
        body: Rc<RefCell<Object>>,
    },
}

// public methods

pub fn cons(args: Rc<RefCell<Object>>) -> EvalResult {
    let a = car(args.clone())?;
    let b = cadr(args)?;

    Ok(Rc::new(RefCell::new(Object::Pair(a, b))))
}

pub fn car(pair: Rc<RefCell<Object>>) -> EvalResult {
    match *pair.borrow() {
        Object::Pair(ref a, _) => Ok(a.clone()),
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

pub fn cdadr(pair: Rc<RefCell<Object>>) -> EvalResult {
    cdr(car(cdr(pair)?)?)
}

pub fn is_null(object: Rc<RefCell<Object>>) -> EvalResult {
    match object.borrow().deref() {
        Object::Null => Ok(Rc::new(RefCell::new(Object::Boolean(true)))),
        _ => Ok(Rc::new(RefCell::new(Object::Boolean(false)))),
    }
}

pub fn is_false(object: Rc<RefCell<Object>>) -> EvalResult {
    let bool = matches!(object.borrow().deref(), Object::Boolean(false) | Object::Null);

    Ok(Rc::new(RefCell::new(Object::Boolean(bool))))

}

pub fn is_true(object: Rc<RefCell<Object>>) -> EvalResult {
    let bool = !matches!(object.borrow().deref(), Object::Boolean(false) | Object::Null);
    Ok(Rc::new(RefCell::new(Object::Boolean(bool))))
}

pub fn is_boolean(object: Rc<RefCell<Object>>) -> EvalResult {
    let bool = matches!(object.borrow().deref(), Object::Boolean(_));
    Ok(Rc::new(RefCell::new(Object::Boolean(bool))))
}

pub fn is_bytevector(object: Rc<RefCell<Object>>) -> EvalResult {
    let bool = matches!(object.borrow().deref(), Object::Bytevector(_));

    Ok(Rc::new(RefCell::new(Object::Boolean(bool))))
}

pub fn is_char(object: Rc<RefCell<Object>>) -> EvalResult {
    let bool = matches!(object.borrow().deref(), Object::Character(_));

    Ok(Rc::new(RefCell::new(Object::Boolean(bool))))
}

pub fn is_eof_object(object: Rc<RefCell<Object>>) -> EvalResult {
    let bool = matches!(object.borrow().deref(), Object::_Eof);

    Ok(Rc::new(RefCell::new(Object::Boolean(bool))))
}

pub fn is_number(object: Rc<RefCell<Object>>) -> EvalResult {
    let bool = matches!(object.borrow().deref(), Object::Number(_));

    Ok(Rc::new(RefCell::new(Object::Boolean(bool))))
}

pub fn is_pair(object: Rc<RefCell<Object>>) -> EvalResult {
    let bool = matches!(object.borrow().deref(), Object::Pair(_, _));

    Ok(Rc::new(RefCell::new(Object::Boolean(bool))))
}

pub fn is_procedure(object: Rc<RefCell<Object>>) -> EvalResult {
    let bool = matches!(object.borrow().deref(), Object::Procedure(_));

    Ok(Rc::new(RefCell::new(Object::Boolean(bool))))
}

pub fn is_string(object: Rc<RefCell<Object>>) -> EvalResult {
    let bool = matches!(object.borrow().deref(), Object::String(_));

    Ok(Rc::new(RefCell::new(Object::Boolean(bool))))
}

pub fn is_symbol(object: Rc<RefCell<Object>>) -> EvalResult {
    let bool = matches!(object.borrow().deref(), Object::Identifier(_));

    Ok(Rc::new(RefCell::new(Object::Boolean(bool))))
}

pub fn is_vector(object: Rc<RefCell<Object>>) -> EvalResult {
    let bool = matches!(object.borrow().deref(), Object::Vector(_));

    Ok(Rc::new(RefCell::new(Object::Boolean(bool))))
}

pub fn is_port(object: Rc<RefCell<Object>>) -> EvalResult {
    let bool = matches!(object.borrow().deref(), Object::_Port);

    Ok(Rc::new(RefCell::new(Object::Boolean(bool))))
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
        let a = match self {
            Object::Number(n) => n,
            _ => panic!("Cannot compare non-number object"),
        };

        let b = match other {
            Object::Number(n) => n,
            _ => panic!("Cannot compare non-number object"),
        };

        a == b
    }
}