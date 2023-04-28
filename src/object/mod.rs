mod boolean;
mod bytevector;
mod character;
mod error;
mod keyword;
mod map;
mod number;
mod pair;
mod string;
mod symbol;
mod vector;

pub mod env;
pub mod port;
pub mod procedure;


pub use crate::object::boolean::Boolean;
pub use crate::object::bytevector::Bytevector;
pub use crate::object::character::Character;
pub use crate::object::env::Env;
pub use crate::object::error::AthirError;
pub use crate::object::keyword::Keyword;
pub use crate::object::map::Map;
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
    Procedure(ProcedureKind), 
    Quotation(Object),
    String(String),
    Symbol(String),
    Unspecified,
    Vector(Object),
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

    pub fn new_null() -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Null)),
        }
    }

    pub fn new_quotation(value: Object) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Quotation(value))),
        }
    }

    pub fn new_unspecified() -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Unspecified)),
        }
    }

}

impl Termination for Object {
    fn report(self) -> std::process::ExitCode {
        std::process::exit(0)
    }
}
