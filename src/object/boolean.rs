use crate::object::{Object, Value, AthirError};
use std::rc::Rc;
use std::cell::RefCell;

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
            _ => Err(<Object as AthirError>::new(format!("not a boolean"))),
        }
    }

}

