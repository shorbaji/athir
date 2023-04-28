use crate::object::{Object, Value, Boolean, AthirError};
use std::rc::Rc;
use std::cell::RefCell;

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
            _ => Err(<Object as AthirError>::new(format!("not a string"))),
        }
    }
    fn is_string(&self) -> Result<Object, Object> {
        Ok(<Object as Boolean>::new(self.as_string().is_ok()))
    }
}

