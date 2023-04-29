use crate::object::{Object, Value, };

impl From <String> for Object {
    fn from(value: String) -> Object {
        Object::new(Value::String(value))
    }
}

impl Object {
    pub fn as_string(&self) -> Result<String, Object> {
        match *self.borrow() {
            Value::String(ref value) => Ok(value.clone()),
            _ => Err(Object::runtime_error("not a string")?),
        }
    }

    pub fn is_string(&self) -> Result<Object, Object> {
        Ok(Object::from(matches!(*self.borrow(), Value::String(_))))
    }
}

