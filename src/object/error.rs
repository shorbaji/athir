use crate::object::{Object, Value};

impl Object {
    pub fn new_error(value: String) -> Object {
        Object::new(Value::Error(Object::from(value)))
    }

    pub fn is_error(&self) -> Result<Object, Object> {
        Ok(Object::from(matches!(*self.borrow(), Value::Error(_))))
    }
}
