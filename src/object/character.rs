use crate::object::{Object, Value, Boolean, AthirError};
use std::rc::Rc;
use std::cell::RefCell;

pub trait Character {
    fn new(value: char) -> Object;
    fn is_character(&self) -> Result<Object, Object>;
    fn as_character(&self) -> Result<char, Object>;
}

impl Character for Object {
    fn new(value: char) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Character(value))),
        }
    }

    fn is_character(&self) -> Result<Object, Object> {
        Ok(<Object as Boolean>::new(self.as_character().is_ok()))
    }

    fn as_character(&self) -> Result<char, Object> {
        match *self.borrow() {
            Value::Character(ref value) => Ok(*value),
            _ => Err(<Object as AthirError>::new(format!("not a character"))),
        }
    }
}

