use std::rc::Rc;
use std::cell::RefCell;
use crate::object::{Object, Value, Boolean, AthirError, Symbol};
use std::collections::HashMap;

pub trait Map {
    fn new() -> Object;
    fn is_map(&self) -> Result<Object, Object>;
    fn as_map(&self) -> Result<Object, Object>;
    fn lookup(&self, key: &Object) -> Result<Object, Object>;
    fn insert(&self, key: &Object, val: &Object) -> Result<Object, Object>;
}

impl Map for Object {
    fn new() -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Map(HashMap::new()))),
        }
    }

    fn is_map(&self) -> Result<Object, Object> {
        Ok(<Object as Boolean>::new(self.as_map().is_ok()))
    }

    fn as_map(&self) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Map(_) => Ok(self.clone()),
            _ => Err(<Object as AthirError>::new(format!("not a map"))),
        }
    }

    fn lookup(&self, symbol: &Object) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Map(ref hm) => {
                let key = symbol.to_key()?;
                match hm.get(&key) {
                    Some(val) => Ok(val.clone()),
                    None => Err(<Object as AthirError>::new(format!("unbound variable"))),
                }
            },
            _ => Err(<Object as AthirError>::new(format!("not a map"))),
        }
    }

    fn insert(&self, key: &Object, val: &Object) -> Result<Object, Object> {
        match *self.borrow_mut() {
            Value::Map(ref mut hm) => {
                let key = key.to_key()?;
                hm.insert(key, val.clone());
                Ok(val.clone())
            },
            _ => Err(<Object as AthirError>::new(format!("not a map"))),
        }
    }
}