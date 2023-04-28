use crate::object::{Object, Value, Boolean, AthirError};
use std::rc::Rc;
use std::cell::RefCell;

pub trait Symbol {
    fn new(value: String) -> Object;
    fn is_symbol(&self) -> Result<Object, Object>;
    fn as_symbol(&self) -> Result<Object, Object>;
    fn to_key(&self) -> Result<String, Object>;
}

impl Symbol for Object {
    fn new(value: String) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Symbol(value))),
        }
    }
    
    fn is_symbol(&self) -> Result<Object, Object> {
        Ok(<Object as Boolean>::new(self.as_symbol().is_ok()))
    }

    fn as_symbol(&self) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Symbol(_) => Ok(self.clone()),
            _ => Err(<Object as AthirError>::new(format!("not a symbol"))),
        }
    }

    fn to_key(&self) -> Result<String, Object> {
        match *self.borrow() {
            Value::Symbol(ref value) => Ok(value.clone()),
            _ => Err(<Object as AthirError>::new(format!("not a variable"))),
        }
    }

}

