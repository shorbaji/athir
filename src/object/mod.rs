mod boolean;
mod bytevector;
mod character;
mod error;
mod keyword;
mod lambda;
mod map;
mod number;
mod pair;
mod string;
mod symbol;
mod vector;

pub mod env;
pub mod port;
pub mod procedure;


pub use crate::object::keyword::Keyword;
pub use crate::object::procedure::BuiltIn;

use std::cell::{Ref, RefMut, RefCell};
use std::collections::HashMap;
use std::process::Termination;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Object {
    value: Rc<RefCell<Value>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Boolean(bool),
    Bytevector(Object),
    Character(char),
    Eof,
    Env(Object, Object),
    Error(Object),
    Keyword(Keyword),
    Map(HashMap<String, Object>),
    Null,
    Number(String),
    Pair(Object, Object),
    Port,
    Builtin(BuiltIn), 
    Lambda(Object, Object, Object),
    Quotation(Object),
    String(String),
    Symbol(String),
    Unspecified,
    Vector(Object),
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
