use crate::object::{Object, Value, Boolean, AthirError};
use std::rc::Rc;
use std::cell::RefCell;

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
            _ => Err(<Object as AthirError>::new(format!("not a vector"))),
        }
    }

    fn is_vector(&self) -> Result<Object, Object> {
        Ok(<Object as Boolean>::new(self.as_vector().is_ok()))
    }
}
