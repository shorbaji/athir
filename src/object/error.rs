use crate::object::{Object, Value};

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    Syntax{depth: usize, message: String},
    Runtime{message: String},
}

impl Object {
    pub fn new_error(e: Error) -> Object {
        Object::new(Value::Error(e))
    }

    pub fn is_error(&self) -> Result<Object, Object> {
        Ok(Object::from(matches!(*self.borrow(), Value::Error(_))))
    }

    pub fn runtime_error(message: &str) -> Result<Object, Object> {
        Err(Object::new_error(Error::Runtime{message: message.to_string()}))
    }

    pub fn syntax_error(depth: usize, message: &str) -> Result<Object, Object> {
        Err(Object::new_error(Error::Syntax{depth, message: message.to_string()}))
    }
}
