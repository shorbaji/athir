use std::cell::RefCell;
use std::rc::Rc;
use crate::object::{Object, Value, Boolean, Pair, AthirError};

pub trait Number {
    fn new(value: String) -> Object;
    fn is_number(&self) -> Result<Object, Object>;
    fn as_number(&self) -> Result<String, Object>;
    fn plus(&self, other: &Object) -> Result<Object, Object>;
    fn add(args: &Object) -> Result<Object, Object>;
    fn multiply(args: &Object) -> Result<Object, Object>;
    fn subtract(args: &Object) -> Result<Object, Object>;
}

impl Number for Object {
    fn new(value: String) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Number(value))),
        }
    }

    fn is_number(&self) -> Result<Object, Object> {
        Ok(<Object as Boolean>::new(self.as_number().is_ok()))
    }

    fn as_number(&self) -> Result<String, Object> {
        match *self.borrow() {
            Value::Number(ref value) => Ok(value.clone()),
            _ => Err(<Object as AthirError>::new(format!("not a number"))),
        }
    }

    fn plus(&self, other: &Object) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Number(ref num) => {
                match *other.borrow() {
                    Value::Number(ref other_num) => {
                        let result = num.parse::<i64>().unwrap() + other_num.parse::<i64>().unwrap();
                        Ok(<Object as Number>::new(result.to_string()))
                    },
                    _ => Err(<Object as AthirError>::new(format!("Not a number"))),
                }
            },
            _ => Err(<Object as AthirError>::new(format!("Not a number"))),
        }
    }
    
    fn add(args: &Object) -> Result<Object, Object> {
        let mut result = 0;
    
        let mut args = args.clone();
    
        while !matches!(*args.borrow(), Value::Null) {
            match *args.car()?.borrow() {
                Value::Number(ref num) => {
                    result += num.parse::<i64>().unwrap();
                },
                _ => return Err(<Object as AthirError>::new("error with plus".to_string())),
            }
            args = args.cdr()?;
        }
        Ok(<Object as Number>::new(result.to_string()))
    }
    
    fn multiply(args: &Object) -> Result<Object, Object> {
        let mut result = 1;
    
        let mut args = args.clone();
    
        while !matches!(*args.borrow(), Value::Null) {
            match *args.car()?.borrow() {
                Value::Number(ref num) => {
                    result *= num.parse::<i64>().unwrap();
                },
                _ => return Err(<Object as AthirError>::new("error with multiply".to_string())),
            }
            args = args.cdr()?;
        }
    
        Ok(<Object as Number>::new(result.to_string()))
    }
    
    fn subtract(args: &Object) -> Result<Object, Object> {
        let mut result;
    
        let first = args.car()?;
        match *first.borrow() {
            Value::Number(ref num) => {
                result = num.parse::<i64>().unwrap();
            },
            _ => return Err(<Object as AthirError>::new("error with minus".to_string())),
        }
    
        let mut args = args.cdr()?;
    
        while !matches!(*args.borrow(), Value::Null) {
            match *args.car()?.borrow() {
                Value::Number(ref num) => {
                    result -= num.parse::<i64>().unwrap();
                },
                _ => return Err(<Object as AthirError>::new("error with minus".to_string())),
            }
            args = args.cdr()?;
        }
    
        Ok(<Object as Number>::new(result.to_string()))
    }
    
}

