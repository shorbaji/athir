pub mod env;
pub mod port;

use std::fmt::Debug;
use std::rc::Rc;
use std::cell::RefCell;
use std::cell::Ref;
use std::cell::RefMut;
use std::collections::HashMap;
use std::ops::Deref;

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

    pub fn car(&self) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Pair(ref car, _) => Ok(car.clone()),
            _ => Err(Object::new_error(format!("Not a pair"))),
        }
    }
    
    pub fn cdr(&self) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Pair(_, ref cdr) => Ok(cdr.clone()),
            _ => Err(Object::new_error(format!("Not a pair"))),
        }
    }
    
    pub fn cons(&self, cdr: &Object) -> Result<Object, Object> {
        Ok(Object::new_pair(self.clone(), cdr.clone()))
    }
    
    pub fn caar(&self) -> Result<Object, Object> {
        self.car()?.car()
    }
    
    pub fn cadr(&self) -> Result<Object, Object> {
        self.cdr()?.car()
    }
    
    pub fn cdar(&self) -> Result<Object, Object> {
        self.car()?.cdr()
    }
    
    pub fn cddr(&self) -> Result<Object, Object> {
        self.cdr()?.cdr()
    }
    
    pub fn caddr(&self) -> Result<Object, Object> {
        self.cddr()?.car()
    }
    
    pub fn cdadr(&self) -> Result<Object, Object> {
        self.cadr()?.cdr()
    }
    
    pub fn len(&self) -> Result<usize, Object> {
        match *self.borrow() {
            Value::Null => Ok(0),
            Value::Pair(_, ref cdr) => Ok(cdr.len()? + 1),
            _ => Err(Object::new_error(format!("Not a pair"))),
        }
    }
    
    pub fn multiply(args: &Object) -> Result<Object, Object> {
        let mut result = 1;
    
        let mut args = args.clone();
    
        while !args.is_null() {
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
    
    pub fn add(args: &Object) -> Result<Object, Object> {
        let mut result = 0;
    
        let mut args = args.clone();
    
        while !args.is_null() {
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
    
    pub fn subtract(args: &Object) -> Result<Object, Object> {
        let mut result;
    
        let first = args.car()?;
        match *first.borrow() {
            Value::Number(ref num) => {
                result = num.parse::<i64>().unwrap();
            },
            _ => return Err(Object::new_error("error with minus".to_string())),
        }
    
        let mut args = args.cdr()?;
    
        while !args.is_null() {
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
}

impl Boolean for Object {
    fn new(value: bool) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Boolean(value))),
        }
    }

    fn is_boolean(&self) -> Result<Object, Object> {
        match *(self.borrow()) {
            Value::Boolean(_) => Ok(<Object as Boolean>::new(true)),
            _ => Ok(<Object as Boolean>::new(false)),
        }
    }
}

pub trait Character {
    fn new(value: char) -> Object;
    fn is_character(&self) -> Result<Object, Object>;
}

impl Character for Object {
    fn new(value: char) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Character(value))),
        }
    }

    fn is_character(&self) -> Result<Object, Object> {
        match self.borrow().deref() {
            Value::Character(_) => Ok(<Object as Boolean>::new(true)),
            _ => Ok(<Object as Boolean>::new(false)),
        }
    }
}

pub trait Number {
    fn new(value: String) -> Object;
    fn is_number(&self) -> Result<Object, Object>;
}

impl Number for Object {
    fn new(value: String) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Number(value))),
        }
    }

    fn is_number(&self) -> Result<Object, Object> {
        match self.borrow().deref() {
            Value::Number(_) => Ok(<Object as Boolean>::new(true)),
            _ => Ok(<Object as Boolean>::new(false)),
        }
    }
}

pub trait AthirString {
    fn new(value: String) -> Object;
    fn is_string(&self) -> Result<Object, Object>;
}

impl AthirString for Object {
    fn new(value: String) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::String(value))),
        }
    }

    fn is_string(&self) -> Result<Object, Object> {
        match self.borrow().deref() {
            Value::String(_) => Ok(<Object as Boolean>::new(true)),
            _ => Ok(<Object as Boolean>::new(false)),
        }
    }
}

pub trait Variable {
    fn new(value: String) -> Object;
    fn is_variable(&self) -> Result<Object, Object>;
}

impl Variable for Object {
    fn new(value: String) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Variable(value))),
        }
    }

    fn is_variable(&self) -> Result<Object, Object> {
        match self.borrow().deref() {
            Value::Variable(_) => Ok(<Object as Boolean>::new(true)),
            _ => Ok(<Object as Boolean>::new(false)),
        }
    }
}

pub trait Bytevector {
    fn new(value: Object) -> Object;
    fn is_bytevector(&self) -> Result<Object, Object>;
}

impl Bytevector for Object {
    fn new(value: Object) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Bytevector(value))),
        }
    }

    fn is_bytevector(&self) -> Result<Object, Object> {
        match self.borrow().deref() {
            Value::Bytevector(_) => Ok(<Object as Boolean>::new(true)),
            _ => Ok(<Object as Boolean>::new(false)),
        }
    }
}

pub trait Vector {
    fn new(value: Object) -> Object;
    fn is_vector(&self) -> Result<Object, Object>;
}

impl Vector for Object {
    fn new(value: Object) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Vector(value))),
        }
    }

    fn is_vector(&self) -> Result<Object, Object> {
        match self.borrow().deref() {
            Value::Vector(_) => Ok(<Object as Boolean>::new(true)),
            _ => Ok(<Object as Boolean>::new(false)),
        }
    }
}
impl Object {

    pub fn as_variable_string(&self) -> Result<String, Object> {
        match self.borrow().deref() {
            Value::Variable(value) => Ok(value.clone()),
            _ => Err(Object::new_error(format!("not a variable"))),
        }
    }

    pub fn is_bytevector(&self) -> bool {
        match self.borrow().deref() {
            Value::Bytevector(_) => true,
            _ => false,
        }
    }

    pub fn is_character(&self) -> bool {
        match self.borrow().deref() {
            Value::Character(_) => true,
            _ => false,
        }
    }

    pub fn is_number(&self) -> bool {
        match self.borrow().deref() {
            Value::Number(_) => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self.borrow().deref() {
            Value::String(_) => true,
            _ => false,
        }
    }

    pub fn is_vector(&self) -> bool {
        match self.borrow().deref() {
            Value::Vector(_) => true,
            _ => false,
        }
    }

    pub fn is_eof(&self) -> bool {
        match self.borrow().deref() {
            Value::Eof => true,
            _ => false,
        }
    }

    pub fn is_error(&self) -> bool {
        match self.borrow().deref() {
            Value::Error(_) => true,
            _ => false,
        }
    }

    pub fn is_keyword(&self) -> bool {
        match self.borrow().deref() {
            Value::Keyword(_) => true,
            _ => false,
        }
    }

    pub fn is_map(&self) -> bool {
        match self.borrow().deref() {
            Value::Map(_) => true,
            _ => false,
        }
    }

    pub fn is_env(&self) -> bool {
        match self.borrow().deref() {
            Value::Env(_, _) => true,
            _ => false,
        }
    }

    pub fn is_null(&self) -> bool {
        match self.borrow().deref() {
            Value::Null => true,
            _ => false,
        }
    }

    pub fn is_pair(&self) -> bool {
        match self.borrow().deref() {
            Value::Pair(_, _) => true,
            _ => false,
        }
    }

    pub fn is_port(&self) -> bool {
        match self.borrow().deref() {
            Value::Port => true,
            _ => false,
        }
    }

    pub fn is_procedure(&self) -> bool {
        match self.borrow().deref() {
            Value::Procedure(_) => true,
            _ => false,
        }
    }

    pub fn is_quotation(&self) -> bool {
        match self.borrow().deref() {
            Value::Quotation(_) => true,
            _ => false,
        }
    }

    pub fn is_unspecified(&self) -> bool {
        match self.borrow().deref() {
            Value::Unspecified => true,
            _ => false,
        }
    }

    pub fn is_variable(&self) -> bool {
        match self.borrow().deref() {
            Value::Variable(_) => true,
            _ => false,
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

    pub fn new_pair(car: Object, cdr: Object) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Pair(car, cdr))),
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

impl Debug for Procedure {
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
