pub mod boolean;
pub mod bytevector;
pub mod character;
pub mod env;
pub mod error;
pub mod number;
pub mod pair;
pub mod port;
pub mod procedure;
pub mod string;
pub mod symbol;
pub mod vector;

pub use crate::object::boolean::Boolean;
pub use crate::object::bytevector::Bytevector;
pub use crate::object::character::Character;
pub use crate::object::env::Env;
pub use crate::object::error::AthirError;
pub use crate::object::number::Number;
pub use crate::object::pair::Pair;
pub use crate::object::port::Port;
pub use crate::object::procedure::{Procedure, ProcedureKind};
pub use crate::object::string::AthirString;
pub use crate::object::symbol::Symbol;
pub use crate::object::vector::Vector;

use std::cell::{Ref, RefMut, RefCell};
use std::collections::HashMap;
use std::process::Termination;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Object {
    pub value: Rc<RefCell<Value>>,
}

impl Object {
    pub fn borrow(&self) -> Ref<Value> {
        self.value.borrow()
    }

    pub fn borrow_mut(&self) -> RefMut<Value> {
        self.value.borrow_mut()
    }

    pub fn eq(a: &Object, b: &Object) -> Result<Object, Object> {
    
        let bool = *a.borrow() ==  *b.borrow();
    
        Ok(<Object as Boolean>::new(bool))
    }

    pub fn as_variable_string(&self) -> Result<String, Object> {
        match *self.borrow() {
            Value::Symbol(ref value) => Ok(value.clone()),
            _ => Err(<Object as AthirError>::new(format!("not a variable"))),
        }
    }

    pub fn is_eof(&self) -> bool {
        match *self.borrow() {
            Value::Eof => true,
            _ => false,
        }
    }

    pub fn is_null(&self) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Null => Ok(<Object as Boolean>::new(true)),
            _ => Ok(<Object as Boolean>::new(false)),
        }
    }

    pub fn new_eof() -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Eof)),
        }
    }

    pub fn new_keyword(value: Keyword) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Keyword(value)))
        }
    }

    pub fn null() -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Null)),
        }
    }

    pub fn new_quotation(value: Object) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Quotation(value))),
        }
    }

    pub fn unspecified() -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Unspecified)),
        }
    }

    pub fn new_map() -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Map(HashMap::new())))
        }
    }

}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Eof,
    Null,

    Boolean(bool),
    Character(char),
    Number(String),
    String(String),
    Symbol(String),

    Bytevector(Object),
    Vector(Object),

    Pair(Object, Object),
    Port,
    Procedure(ProcedureKind), 

    Error(Object),
    Keyword(Keyword),
    Map(HashMap<String, Object>),
    Env(Object, Object),
    Quotation(Object),
    Unspecified,
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

impl Termination for Object {
    fn report(self) -> std::process::ExitCode {
        std::process::exit(0)
        
    }
}