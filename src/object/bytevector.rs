use crate::object::{Object, Value, Boolean, AthirError};
use std::rc::Rc;
use std::cell::RefCell;

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
            _ => Err(<Object as AthirError>::new(format!("not a bytevector"))),
        }
    }

    fn is_bytevector(&self) -> Result<Object, Object> {
        Ok(<Object as Boolean>::new(self.as_bytevector().is_ok()))
    }
}

