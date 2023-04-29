use crate::object::{Object, Value, };
use std::collections::HashMap;

impl From<HashMap<String, Object>> for Object {
    fn from(value: HashMap<String, Object>) -> Object {
        Object::new(Value::Map(value))
    }
}

impl Object {
    pub fn is_map(&self) -> Result<Object, Object> {
        Ok(Object::from(matches!(*self.borrow(), Value::Map(_))))
    }

    pub fn map_lookup(&self, symbol: &Object) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Map(ref hm) => {
                let key = symbol.to_key()?;
                match hm.get(&key) {
                    Some(val) => Ok(val.clone()),
                    None => Err(Object::runtime_error("unbound variable")?),
                }
            },
            _ => Err(Object::runtime_error("not a map")?),
        }
    }

    pub fn map_insert(&self, key: &Object, val: &Object) -> Result<Object, Object> {
        match *self.borrow_mut() {
            Value::Map(ref mut hm) => {
                let key = key.to_key()?;
                hm.insert(key, val.clone());
                Ok(val.clone())
            },
            _ => Err(Object::runtime_error("not a map")?),
        }
    }
}