mod boolean;
mod bytevector;
mod character;
mod env;
mod error;
mod keyword;
mod lambda;
mod map;
mod number;
mod pair;
mod port;
mod procedure;
mod string;
mod symbol;
mod vector;



pub use crate::object::keyword::Keyword;
pub use crate::object::procedure::BuiltIn;
pub use crate::object::port::Port;
pub use crate::object::error::Error;

use std::cell::{Ref, RefMut, RefCell};
use std::collections::HashMap;
use std::process::Termination;
use std::rc::Rc;

#[derive(Clone, PartialEq)]
pub struct Object {
    value: Rc<RefCell<Value>>,
}

#[derive(Clone, PartialEq)]
pub enum Value {
    Boolean(bool),
    Bytevector(Object),
    Character(char),
    Eof,
    Env(Object, Object),
    Error(Error),
    Keyword(Keyword),
    Map(HashMap<String, Object>),
    Null,
    Number(String),
    Pair(Object, Object),
    Port(Port),
    Builtin(BuiltIn), 
    Closure(Object, Object, Object),
    Quotation(Object),
    String(String),
    Symbol(String),
    Unspecified,
    Vector(Object),
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::Boolean(b) => write!(f, "#<boolean: {:?}>", b),
            Value::Bytevector(bv) => write!(f, "#<bytevector: {:?}>", bv),
            Value::Character(c) => write!(f, "<#char: {}>", c),
            Value::Eof => write!(f, "#<eof>"),
            Value::Env(_, _) => write!(f, "#<env>"),
            Value::Error(e) => write!(f, "#<error: {:?}>", e),
            Value::Keyword(k) => write!(f, "#<keyword: {:?}>", k),
            Value::Map(m) => write!(f, "#<map: {:?}>", m),
            Value::Null => write!(f, "#<null>"),
            Value::Number(n) => write!(f, "#<number: {:?}>", n),
            Value::Pair(car, cdr) => write!(f, "#<pair: {:?}, {:?}>", car, cdr),
            Value::Port(p) => write!(f, "#<port: {:?}>", p),
            Value::Builtin(b) => write!(f, "#<proc builtin: {:?}>", b),
            Value::Closure(formals, body, _) => write!(f, "#<closure: {:?} -> {:?}>", formals, body),
            Value::Quotation(q) => write!(f, "#<quote: {:?}>", q),
            Value::String(s) => write!(f, "#<string: {}>", s),
            Value::Symbol(s) => write!(f, "#<symbol: {}>", s),
            Value::Unspecified => write!(f, "#<unspecified>"),
            Value::Vector(v) => write!(f, "#<vector: {:?}>", v),
        }
    }
}

impl std::fmt::Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.value.borrow().fmt(f)
    }
}

impl Object {
    pub fn new(value: Value) -> Object {
        Object {
            value: Rc::new(RefCell::new(value)),
        }
    }

    pub fn borrow(&self) -> Ref<Value> {
        self.value.borrow()
    }

    pub fn borrow_mut(&self) -> RefMut<Value> {
        self.value.borrow_mut()
    }

    pub fn eq(a: &Object, b: &Object) -> Result<Object, Object> {
    
        let bool = *a.borrow() ==  *b.borrow();
    
        Ok(Object::from(bool))
    }

    pub fn is_eof(&self) -> bool {
        match *self.borrow() {
            Value::Eof => true,
            _ => false,
        }
    }

    pub fn is_null(&self) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Null => Ok(Object::from(true)),
            _ => Ok(Object::from(false)),
        }
    }

    pub fn is_unspecified(&self) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Unspecified => Ok(Object::from(true)),
            _ => Ok(Object::from(false)),
        }
    }

    pub fn unspecified() -> Object {
        Object::new(Value::Unspecified)
    }
    
    pub fn new_eof() -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Eof)),
        }
    }

}

impl Termination for Object {
    fn report(self) -> std::process::ExitCode {
        std::process::exit(0)
    }
}
