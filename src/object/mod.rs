pub mod env;
pub mod port;

use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Boolean(bool),
    Bytevector(Object),
    Character(char),
    Number(String),
    String(String),
    Vector(Object),
    Eof,
    Error(Object),
    Keyword(Keyword),
    Map(HashMap<String, Object>),
    Env(Object, Object),
    Null,
    Pair(Object, Object),
    Port,
    Procedure(Procedure), 
    Quotation(Object),
    Unspecified,
    Variable(String),
}

pub type Object = Rc<RefCell<Value>>;


#[derive(Debug, Clone, PartialEq)]
pub enum Procedure {
    Nullary(fn() -> Result<Object, Object>),
    Unary(fn(Object) -> Result<Object, Object>),
    Binary(fn(Object, Object) -> Result<Object, Object>),
    Ternanry(fn(Object, Object, Object) -> Result<Object, Object>),
    Variadic(fn(Object) -> Result<Object, Object>),
    Lambda(Object, Object, Object),
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

pub type Key = String;

pub trait Port {
    fn new() -> Object;
}

impl Port for Object {
    fn new() -> Object {
        Rc::new(RefCell::new(Value::Port))
    }
}
pub trait ObjectExt {
    fn as_string(&self) -> Result<String, Object>;
    fn as_variable_string(&self) -> Result<String, Object>;
    fn is_boolean(&self) -> bool;
    fn is_bytevector(&self) -> bool;
    fn is_character(&self) -> bool;
    fn is_number(&self) -> bool;
    fn is_string(&self) -> bool;
    fn is_vector(&self) -> bool;
    fn is_eof(&self) -> bool;
    fn is_error(&self) -> bool;
    fn is_keyword(&self) -> bool;
    fn is_map(&self) -> bool;
    fn is_env(&self) -> bool;
    fn is_null(&self) -> bool;
    fn is_pair(&self) -> bool;
    fn is_port(&self) -> bool;
    fn is_procedure(&self) -> bool;
    fn is_quotation(&self) -> bool;
    fn is_unspecified(&self) -> bool;
    fn is_variable(&self) -> bool;    
    fn new_boolean(value: bool) -> Object;
    fn new_bytevector(value: Object) -> Object;
    fn new_character(value: char) -> Object;
    fn new_number(value: String) -> Object;
    fn new_string(value: String) -> Object;
    fn new_vector(value: Object) -> Object;
    fn new_eof() -> Object;
    fn new_error(value: String) -> Object;
    fn new_keyword(value: Keyword) -> Object;
    fn new_map() -> Object;
    fn new_procedure(value: Procedure) -> Object;
    fn new_quotation(value: Object) -> Object;
    fn new_variable(value: String) -> Object;
    fn null() -> Object;
    fn unspecified() -> Object;

}

impl ObjectExt for Object {

    fn as_string(&self) -> Result<String, Object> {
        match self.deref().borrow().deref() {
            Value::String(value) => Ok(value.clone()),
            _ => Err(Object::new_error(format!("not a string"))),
        }
    }

    fn as_variable_string(&self) -> Result<String, Object> {
        match self.deref().borrow().deref() {
            Value::Variable(value) => Ok(value.clone()),
            _ => Err(Object::new_error(format!("not a variable"))),
        }
    }

    fn is_boolean(&self) -> bool {
        match self.deref().borrow().deref() {
            Value::Boolean(_) => true,
            _ => false,
        }
    }

    fn is_bytevector(&self) -> bool {
        match self.deref().borrow().deref() {
            Value::Bytevector(_) => true,
            _ => false,
        }
    }

    fn is_character(&self) -> bool {
        match self.deref().borrow().deref() {
            Value::Character(_) => true,
            _ => false,
        }
    }

    fn is_number(&self) -> bool {
        match self.deref().borrow().deref() {
            Value::Number(_) => true,
            _ => false,
        }
    }

    fn is_string(&self) -> bool {
        match self.deref().borrow().deref() {
            Value::String(_) => true,
            _ => false,
        }
    }

    fn is_vector(&self) -> bool {
        match self.deref().borrow().deref() {
            Value::Vector(_) => true,
            _ => false,
        }
    }

    fn is_eof(&self) -> bool {
        match self.deref().borrow().deref() {
            Value::Eof => true,
            _ => false,
        }
    }

    fn is_error(&self) -> bool {
        match self.deref().borrow().deref() {
            Value::Error(_) => true,
            _ => false,
        }
    }

    fn is_keyword(&self) -> bool {
        match self.deref().borrow().deref() {
            Value::Keyword(_) => true,
            _ => false,
        }
    }

    fn is_map(&self) -> bool {
        match self.deref().borrow().deref() {
            Value::Map(_) => true,
            _ => false,
        }
    }

    fn is_env(&self) -> bool {
        match self.deref().borrow().deref() {
            Value::Env(_, _) => true,
            _ => false,
        }
    }

    fn is_null(&self) -> bool {
        match self.deref().borrow().deref() {
            Value::Null => true,
            _ => false,
        }
    }

    fn is_pair(&self) -> bool {
        match self.deref().borrow().deref() {
            Value::Pair(_, _) => true,
            _ => false,
        }
    }

    fn is_port(&self) -> bool {
        match self.deref().borrow().deref() {
            Value::Port => true,
            _ => false,
        }
    }

    fn is_procedure(&self) -> bool {
        match self.deref().borrow().deref() {
            Value::Procedure(_) => true,
            _ => false,
        }
    }

    fn is_quotation(&self) -> bool {
        match self.deref().borrow().deref() {
            Value::Quotation(_) => true,
            _ => false,
        }
    }

    fn is_unspecified(&self) -> bool {
        match self.deref().borrow().deref() {
            Value::Unspecified => true,
            _ => false,
        }
    }

    fn is_variable(&self) -> bool {
        match self.deref().borrow().deref() {
            Value::Variable(_) => true,
            _ => false,
        }
    }

    fn new_boolean(value: bool) -> Object {
        Rc::new(RefCell::new(Value::Boolean(value)))
    }

    fn new_bytevector(value: Object) -> Object {
        Rc::new(RefCell::new(Value::Bytevector(value)))
    }

    fn new_character(value: char) -> Object {
        Rc::new(RefCell::new(Value::Character(value)))
    }

    fn new_number(value: String) -> Object {
        Rc::new(RefCell::new(Value::Number(value)))
    }

    fn new_string(value: String) -> Object {
        Rc::new(RefCell::new(Value::String(value)))
    }

    fn new_vector(value: Object) -> Object {
        Rc::new(RefCell::new(Value::Vector(value)))
    }

    fn new_eof() -> Object {
        Rc::new(RefCell::new(Value::Eof))
    }

    fn new_error(value: String) -> Object {
        Rc::new(RefCell::new(Value::Error(Rc::new(RefCell::new(Value::String(value))))))
    }

    fn new_keyword(value: Keyword) -> Object {
        Rc::new(RefCell::new(Value::Keyword(value)))
    }

    fn null() -> Object {
        Rc::new(RefCell::new(Value::Null))
    }

    fn new_procedure(value: Procedure) -> Object {
        Rc::new(RefCell::new(Value::Procedure(value)))
    }

    fn new_quotation(value: Object) -> Object {
        Rc::new(RefCell::new(Value::Quotation(value)))
    }

    fn unspecified() -> Object {
        Rc::new(RefCell::new(Value::Unspecified))
    }

    fn new_variable(value: String) -> Object {
        Rc::new(RefCell::new(Value::Variable(value)))
    }

    fn new_map() -> Object {
        Rc::new(RefCell::new(Value::Map(HashMap::new())))
    }

}

