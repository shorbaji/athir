use crate::object::{Object, Value};

impl From <char> for Object {
    fn from(value: char) -> Object {
        Object::new(Value::Character(value))
    }
}

impl Into<Result<char, Object>> for Object {
    fn into(self) -> Result<char, Object> {
        match *self.borrow() {
            Value::Character(ref value) => Ok(*value),
            _ => Err(Object::runtime_error("not a character")?),
        }
    }
}

impl Object {
    pub fn is_character(&self) -> Result<Object, Object> {
        Ok(Object::from(matches!(*self.borrow(), Value::Character(_))))
    }

}

