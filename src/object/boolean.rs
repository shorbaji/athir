use crate::object::{Object, Value};

impl Into::<Result<bool, Object>> for &Object {
    fn into(self) -> Result<bool, Object> {
        match *(self.borrow()) {
            Value::Boolean(value) => Ok(value),
            _ => Err(Object::runtime_error("not a boolean")?),
        }
    }
}


impl From::<Object> for bool {
    fn from(value: Object) -> bool {
        match *(value.borrow()) {
            Value::Boolean(value) => value,
            _ => true,
        }
    }
}

impl From::<bool> for Object {
    fn from(value: bool) -> Object {
        Object::new(Value::Boolean(value))
    }
}


impl Object {
    pub fn is_boolean(&self) -> Result<Object, Object> {
        Ok(Object::from(matches!(*self.borrow(), Value::Boolean(_))))
    }
}

