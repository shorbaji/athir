use crate::object::{Object, Value, Boolean, AthirString};
use std::rc::Rc;
use std::cell::RefCell;

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
            _ => Err(<Object as AthirError>::new(format!("not an error"))),
        }
    }

    fn is_error(&self) -> Result<Object, Object> {
        Ok(<Object as Boolean>::new(self.as_error().is_ok()))
    }
}
