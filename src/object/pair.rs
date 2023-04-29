
use crate::object::{Object, Value,};

impl Object {
    fn new_pair(car: Object, cdr: Object) -> Object {
        Object::new(Value::Pair(car, cdr))
    }

    pub fn is_pair(&self) -> Result<Object, Object> {
        Ok(Object::from(matches!(*self.borrow(), Value::Pair(_, _))))
    }

    pub fn is_atom(&self) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Pair(_, _) => Ok(Object::from(false)),
            _ => Ok(Object::from(true)),
        }
    }
    
    pub fn cons(&self, cdr: &Object) -> Result<Object, Object> {
        Ok(Object::new_pair(self.clone(), cdr.clone()))
    }

    pub fn car(&self) -> Result<Object, Object> { 
        match *self.borrow() {
            Value::Pair(ref car, _) => Ok(car.clone()),
            _ => Err(Object::new_error(format!("not a pair"))),
        }
    }

    pub fn cdr(&self) -> Result<Object, Object> { 
        match *self.borrow() {
            Value::Pair(_, ref cdr) => Ok(cdr.clone()),
            _ => Err(Object::new_error(format!("not a pair"))),
        }
    }

    pub fn caar(&self) -> Result<Object, Object> { self.car()?.car() }
    pub fn cadr(&self) -> Result<Object, Object> { self.cdr()?.car() }
    pub fn cdar(&self) -> Result<Object, Object> { self.car()?.cdr() }
    pub fn cddr(&self) -> Result<Object, Object> { self.cdr()?.cdr() }
    pub fn caddr(&self) -> Result<Object, Object> { self.cddr()?.car() }
    pub fn cdadr(&self) -> Result<Object, Object> { self.cadr()?.cdr() }
    
    pub fn len(&self) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Null => Ok(Object::new_number("0".to_string())),
            _ => self.cdr()?.len()?.plus(&Object::new_number("1".to_string())),
        }
    }

}
