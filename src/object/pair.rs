use std::cell::RefCell;
use std::rc::Rc;

use crate::object::{Object, Value, Boolean, Number, AthirError};

pub trait Pair {
    fn new(car: Object, cdr: Object) -> Object;
    fn is_pair(&self) -> Result<Object, Object>;
    fn as_pair(&self) -> Result<Object, Object>;
    fn cons(&self, cdr: &Object) -> Result<Object, Object>;
    fn car(&self) -> Result<Object, Object>;
    fn cdr(&self) -> Result<Object, Object>;
    fn caar(&self) -> Result<Object, Object>;
    fn cadr(&self) -> Result<Object, Object>;
    fn cdar(&self) -> Result<Object, Object>;
    fn cddr(&self) -> Result<Object, Object>;
    fn caddr(&self) -> Result<Object, Object>;
    fn cdadr(&self) -> Result<Object, Object>;
    fn len(&self) -> Result<Object, Object>;
}

impl Pair for Object {
    fn new(car: Object, cdr: Object) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Pair(car, cdr))),
        }
    }

    fn is_pair(&self) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Pair(_, _) => Ok(<Object as Boolean>::new(true)),
            _ => Ok(<Object as Boolean>::new(false)),
        }
    }

    fn as_pair(&self) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Pair(_, _) => Ok(self.clone()),
            _ => Err(<Object as AthirError>::new(format!("not a pair"))),
        }
    }

    fn cons(&self, cdr: &Object) -> Result<Object, Object> {
        Ok(<Object as Pair>::new(self.clone(), cdr.clone()))
    }

    fn car(&self) -> Result<Object, Object> { 
        match *self.borrow() {
            Value::Pair(ref car, _) => Ok(car.clone()),
            _ => Err(<Object as AthirError>::new(format!("not a pair"))),
        }
    }

    fn cdr(&self) -> Result<Object, Object> { 
        match *self.borrow() {
            Value::Pair(_, ref cdr) => Ok(cdr.clone()),
            _ => Err(<Object as AthirError>::new(format!("not a pair"))),
        }
    }

    fn caar(&self) -> Result<Object, Object> { self.car()?.car() }
    fn cadr(&self) -> Result<Object, Object> { self.cdr()?.car() }
    fn cdar(&self) -> Result<Object, Object> { self.car()?.cdr() }
    fn cddr(&self) -> Result<Object, Object> { self.cdr()?.cdr() }
    fn caddr(&self) -> Result<Object, Object> { self.cddr()?.car() }
    fn cdadr(&self) -> Result<Object, Object> { self.cadr()?.cdr() }
    
    fn len(&self) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Null => Ok(<Object as Number>::new("0".to_string())),
            _ => self.cdr()?.len()?.plus(&<Object as Number>::new("1".to_string())),
        }
    }

}
