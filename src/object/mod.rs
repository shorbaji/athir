pub mod env;
pub mod port;

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

    pub fn cons(&self, cdr: &Object) -> Result<Object, Object> {
        Ok(<Object as Pair>::new(self.clone(), cdr.clone()))
    }

    pub fn car(&self) -> Result<Object, Object> { 
        match *self.borrow() {
            Value::Pair(ref car, _) => Ok(car.clone()),
            _ => Err(Object::new_error(format!("not a pair"))),
        }
    }

    pub fn cdr(&self) -> Result<Object, Object> { 
        match *self.borrow() {
            Value::Pair(_, ref cdr) => Ok(cdr.clone()),
            _ => Err(Object::new_error(format!("not a pair"))),
        }
    }

    pub fn caar(&self) -> Result<Object, Object> { self.car()?.car() }
    pub fn cadr(&self) -> Result<Object, Object> { self.cdr()?.car() }
    pub fn cdar(&self) -> Result<Object, Object> { self.car()?.cdr() }
    pub fn cddr(&self) -> Result<Object, Object> { self.cdr()?.cdr() }
    pub fn caddr(&self) -> Result<Object, Object> { self.cddr()?.car() }
    pub fn cdadr(&self) -> Result<Object, Object> { self.cadr()?.cdr() }
    
    pub fn len(&self) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Null => Ok(<Object as Number>::new("0".to_string())),
            _ => self.cdr()?.len()?.plus(&<Object as Number>::new("1".to_string())),
        }
    }

    pub fn eq(a: &Object, b: &Object) -> Result<Object, Object> {
    
        let bool = *a.borrow() ==  *b.borrow();
    
        Ok(<Object as Boolean>::new(bool))
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
    Variable(String),

    Bytevector(Object),
    Vector(Object),

    Pair(Object, Object),
    Port,
    Procedure(Procedure), 

    Error(Object),
    Keyword(Keyword),
    Map(HashMap<String, Object>),
    Env(Object, Object),
    Quotation(Object),
    Unspecified,
}

pub trait Boolean {
    fn new(value: bool) -> Object;
    fn is_boolean(&self) -> Result<Object, Object>;
    fn as_boolean(&self) -> Result<bool, Object>;
}

impl Boolean for Object {
    fn new(value: bool) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Boolean(value))),
        }
    }

    fn is_boolean(&self) -> Result<Object, Object> {
        Ok(<Object as Boolean>::new(self.as_boolean().is_ok()))
    }

    fn as_boolean(&self) -> Result<bool, Object> {
        match *(self.borrow()) {
            Value::Boolean(ref value) => Ok(*value),
            _ => Err(Object::new_error(format!("not a boolean"))),
        }
    }

}

pub trait Character {
    fn new(value: char) -> Object;
    fn is_character(&self) -> Result<Object, Object>;
    fn as_character(&self) -> Result<char, Object>;
}

impl Character for Object {
    fn new(value: char) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Character(value))),
        }
    }

    fn is_character(&self) -> Result<Object, Object> {
        Ok(<Object as Boolean>::new(self.as_character().is_ok()))
    }

    fn as_character(&self) -> Result<char, Object> {
        match *self.borrow() {
            Value::Character(ref value) => Ok(*value),
            _ => Err(Object::new_error(format!("not a character"))),
        }
    }
}

pub trait Number {
    fn new(value: String) -> Object;
    fn is_number(&self) -> Result<Object, Object>;
    fn as_number(&self) -> Result<String, Object>;
    fn plus(&self, other: &Object) -> Result<Object, Object>;
    fn add(args: &Object) -> Result<Object, Object>;
    fn multiply(args: &Object) -> Result<Object, Object>;
    fn subtract(args: &Object) -> Result<Object, Object>;
}

impl Number for Object {
    fn new(value: String) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Number(value))),
        }
    }

    fn is_number(&self) -> Result<Object, Object> {
        Ok(<Object as Boolean>::new(self.as_number().is_ok()))
    }

    fn as_number(&self) -> Result<String, Object> {
        match *self.borrow() {
            Value::Number(ref value) => Ok(value.clone()),
            _ => Err(Object::new_error(format!("not a number"))),
        }
    }

    fn plus(&self, other: &Object) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Number(ref num) => {
                match *other.borrow() {
                    Value::Number(ref other_num) => {
                        let result = num.parse::<i64>().unwrap() + other_num.parse::<i64>().unwrap();
                        Ok(<Object as Number>::new(result.to_string()))
                    },
                    _ => Err(Object::new_error(format!("Not a number"))),
                }
            },
            _ => Err(Object::new_error(format!("Not a number"))),
        }
    }
    
    fn add(args: &Object) -> Result<Object, Object> {
        let mut result = 0;
    
        let mut args = args.clone();
    
        while !matches!(*args.borrow(), Value::Null) {
            match *args.car()?.borrow() {
                Value::Number(ref num) => {
                    result += num.parse::<i64>().unwrap();
                },
                _ => return Err(Object::new_error("error with plus".to_string())),
            }
            args = args.cdr()?;
        }
        Ok(<Object as Number>::new(result.to_string()))
    }
    
    fn multiply(args: &Object) -> Result<Object, Object> {
        let mut result = 1;
    
        let mut args = args.clone();
    
        while !matches!(*args.borrow(), Value::Null) {
            match *args.car()?.borrow() {
                Value::Number(ref num) => {
                    result *= num.parse::<i64>().unwrap();
                },
                _ => return Err(Object::new_error("error with multiply".to_string())),
            }
            args = args.cdr()?;
        }
    
        Ok(<Object as Number>::new(result.to_string()))
    }
    
    fn subtract(args: &Object) -> Result<Object, Object> {
        let mut result;
    
        let first = args.car()?;
        match *first.borrow() {
            Value::Number(ref num) => {
                result = num.parse::<i64>().unwrap();
            },
            _ => return Err(Object::new_error("error with minus".to_string())),
        }
    
        let mut args = args.cdr()?;
    
        while !matches!(*args.borrow(), Value::Null) {
            match *args.car()?.borrow() {
                Value::Number(ref num) => {
                    result -= num.parse::<i64>().unwrap();
                },
                _ => return Err(Object::new_error("error with minus".to_string())),
            }
            args = args.cdr()?;
        }
    
        Ok(<Object as Number>::new(result.to_string()))
    }
    
}

pub trait AthirString {
    fn new(value: String) -> Object;
    fn is_string(&self) -> Result<Object, Object>;
    fn as_string(&self) -> Result<String, Object>;
}

impl AthirString for Object {
    fn new(value: String) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::String(value))),
        }
    }

    fn as_string(&self) -> Result<String, Object> {
        match *self.borrow() {
            Value::String(ref value) => Ok(value.clone()),
            _ => Err(Object::new_error(format!("not a string"))),
        }
    }
    fn is_string(&self) -> Result<Object, Object> {
        Ok(<Object as Boolean>::new(self.as_string().is_ok()))
    }
}

pub trait Variable {
    fn new(value: String) -> Object;
    fn is_variable(&self) -> Result<Object, Object>;
    fn as_variable_string(&self) -> Result<String, Object>;
}

impl Variable for Object {
    fn new(value: String) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Variable(value))),
        }
    }

    fn as_variable_string(&self) -> Result<String, Object> {
        match *self.borrow() {
            Value::Variable(ref value) => Ok(value.clone()),
            _ => Err(Object::new_error(format!("not a variable"))),
        }
    }
    fn is_variable(&self) -> Result<Object, Object> {
        Ok(<Object as Boolean>::new(self.as_variable_string().is_ok()))
    }
}

pub trait Bytevector {
    fn new(value: Object) -> Object;
    fn is_bytevector(&self) -> Result<Object, Object>;
    fn as_bytevector(&self) -> Result<Object, Object>;
}

impl Bytevector for Object {
    fn new(value: Object) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Bytevector(value))),
        }
    }

    fn as_bytevector(&self) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Bytevector(ref value) => Ok(value.clone()),
            _ => Err(Object::new_error(format!("not a bytevector"))),
        }
    }

    fn is_bytevector(&self) -> Result<Object, Object> {
        Ok(<Object as Boolean>::new(self.as_bytevector().is_ok()))
    }
}

pub trait Vector {
    fn new(value: Object) -> Object;
    fn is_vector(&self) -> Result<Object, Object>;
    fn as_vector(&self) -> Result<Object, Object>;
}

impl Vector for Object {
    fn new(value: Object) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Vector(value))),
        }
    }

    fn as_vector(&self) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Vector(ref value) => Ok(value.clone()),
            _ => Err(Object::new_error(format!("not a vector"))),
        }
    }

    fn is_vector(&self) -> Result<Object, Object> {
        Ok(<Object as Boolean>::new(self.as_vector().is_ok()))
    }
}

pub trait Pair {
    fn new(car: Object, cdr: Object) -> Object;
    fn is_pair(&self) -> Result<Object, Object>;
    fn as_pair(&self) -> Result<Object, Object>;
}

impl Pair for Object {
    fn new(car: Object, cdr: Object) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Pair(car, cdr))),
        }
    }

    fn is_pair(&self) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Pair(_, _) => Ok(<Object as Boolean>::new(true)),
            _ => Ok(<Object as Boolean>::new(false)),
        }
    }

    fn as_pair(&self) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Pair(_, _) => Ok(self.clone()),
            _ => Err(Object::new_error(format!("not a pair"))),
        }
    }
}

pub trait  AthirError {
    fn new(value: String) -> Object;
    fn is_error(&self) -> Result<Object, Object>;
    fn as_error(&self) -> Result<String, Object>;    
}

impl AthirError for Object {
    fn new(value: String) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Error(<Object as AthirString>::new(value)))),
        }
    }

    fn as_error(&self) -> Result<String, Object> {
        match *self.borrow() {
            Value::Error(ref value) => Ok(value.as_string().unwrap()),
            _ => Err(Object::new_error(format!("not an error"))),
        }
    }

    fn is_error(&self) -> Result<Object, Object> {
        Ok(<Object as Boolean>::new(self.as_error().is_ok()))
    }
}

impl Object {

    pub fn as_variable_string(&self) -> Result<String, Object> {
        match *self.borrow() {
            Value::Variable(ref value) => Ok(value.clone()),
            _ => Err(Object::new_error(format!("not a variable"))),
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

    pub fn new_error(value: String) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Error(<Object as AthirString>::new(value)))),
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

    pub fn new_procedure(value: Procedure) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Procedure(value))),
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

    pub fn new_variable(value: String) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Variable(value))),
        }
    }

    pub fn new_map() -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Map(HashMap::new())))
        }
    }

}



#[derive(Clone)]
pub enum Procedure {
    Nullary(fn() -> Result<Object, Object>),
    Unary(fn(&Object) -> Result<Object, Object>),
    Binary(fn(&Object, &Object) -> Result<Object, Object>),
    Ternanry(fn(&Object, &Object, &Object) -> Result<Object, Object>),
    Variadic(fn(&Object) -> Result<Object, Object>),
    Lambda(Object, Object, Object),
}

impl std::fmt::Debug for Procedure {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Procedure::Nullary(_) => write!(f, "Nullary"),
            Procedure::Unary(_) => write!(f, "Unary"),
            Procedure::Binary(_) => write!(f, "Binary"),
            Procedure::Ternanry(_) => write!(f, "Ternanry"),
            Procedure::Variadic(_) => write!(f, "Variadic"),
            Procedure::Lambda(_, _, _) => write!(f, "Lambda"),
        }
    }
}
impl PartialEq for Procedure {
    fn eq(&self, other: &Procedure) -> bool {
        match (self, other) {
            (Procedure::Nullary(_), Procedure::Nullary(_)) => true,
            (Procedure::Unary(_), Procedure::Unary(_)) => true,
            (Procedure::Binary(_), Procedure::Binary(_)) => true,
            (Procedure::Ternanry(_), Procedure::Ternanry(_)) => true,
            (Procedure::Variadic(_), Procedure::Variadic(_)) => true,
            (Procedure::Lambda(_, _, _), Procedure::Lambda(_, _, _)) => true,
            _ => false,
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

pub type Key = String;

pub trait Port {
    fn new() -> Object;
}

impl Port for Object {
    fn new() -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Port))
        }
    }
}

impl Termination for Object {
    fn report(self) -> std::process::ExitCode {
        std::process::exit(0)
        
    }
}