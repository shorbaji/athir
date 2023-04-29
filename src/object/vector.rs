use crate::object::{Object, Value};

impl Object {
    pub fn new_vector(value: Object) -> Object {
        Object ::new(Value::Vector(value))
    }

    pub fn is_vector(&self) -> Result<Object, Object> {
        Ok(Object::from(matches!(*self.borrow(), Value::Vector(_))))
    }
}
