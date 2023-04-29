use crate::object::{Object, Value, };

impl Object {
    pub fn new_symbol(value: String) -> Object {
        Object ::new(Value::Symbol(value))
    }
    
    pub fn is_symbol(&self) -> Result<Object, Object> {
        Ok(Object::from(matches!(*self.borrow(), Value::Symbol(_))))
    }

    pub fn to_key(&self) -> Result<String, Object> {
        match *self.borrow() {
            Value::Symbol(ref value) => Ok(value.clone()),
            _ => Err(Object::new_error(format!("not a variable"))),
        }
    }

}

