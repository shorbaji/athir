use crate::object::{Object, Value};

impl From::<bool> for Object {
    fn from(value: bool) -> Object {
        Object::new(Value::Boolean(value))
    }
}

impl Into::<Result<bool, Object>> for &Object {
    fn into(self) -> Result<bool, Object> {
        match *(self.borrow()) {
            Value::Boolean(value) => Ok(value),
            _ => Err(Object::new_error(format!("not a boolean"))),
        }
    }
}

impl Object {
    pub fn is_boolean(&self) -> Result<Object, Object> {
        Ok(Object::from(matches!(*self.borrow(), Value::Boolean(_))))
    }
}

