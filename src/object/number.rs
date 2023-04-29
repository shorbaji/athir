use crate::object::{Object, Value, };

impl Object {
    pub fn new_number(value: String) -> Object {
        Object::new(Value::Number(value))
    }

    pub fn is_number(&self) -> Result<Object, Object> {
        Ok(Object::from(matches!(*self.borrow(), Value::Number(_))))
    }

    pub fn plus(&self, other: &Object) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Number(ref num) => {
                match *other.borrow() {
                    Value::Number(ref other_num) => {
                        let result = num.parse::<i64>().unwrap() + other_num.parse::<i64>().unwrap();
                        Ok(Object::new_number(result.to_string()))
                    },
                    _ => Err(Object::new_error(format!("Not a number"))),
                }
            },
            _ => Err(Object::new_error(format!("Not a number"))),
        }
    }
    
    pub fn add(args: &Object) -> Result<Object, Object> {
        let mut result = 0;
    
        let mut args = args.clone();
    
        while !matches!(*args.borrow(), Value::Null) {
            match *args.car()?.borrow() {
                Value::Number(ref num) => {
                    result += num.parse::<i64>().unwrap();
                },
                _ => return Err(Object::new_error("error with plus".to_string())),
            }
            args = args.cdr()?;
        }
        Ok(Object::new_number(result.to_string()))
    }
    
    pub fn multiply(args: &Object) -> Result<Object, Object> {
        let mut result = 1;
    
        let mut args = args.clone();
    
        while !matches!(*args.borrow(), Value::Null) {
            match *args.car()?.borrow() {
                Value::Number(ref num) => {
                    result *= num.parse::<i64>().unwrap();
                },
                _ => return Err(Object::new_error("error with multiply".to_string())),
            }
            args = args.cdr()?;
        }
    
        Ok(Object::new_number(result.to_string()))
    }
    
    pub fn subtract(args: &Object) -> Result<Object, Object> {
        let mut result;
    
        let first = args.car()?;
        match *first.borrow() {
            Value::Number(ref num) => {
                result = num.parse::<i64>().unwrap();
            },
            _ => return Err(Object::new_error("error with minus".to_string())),
        }
    
        let mut args = args.cdr()?;
    
        while !matches!(*args.borrow(), Value::Null) {
            match *args.car()?.borrow() {
                Value::Number(ref num) => {
                    result -= num.parse::<i64>().unwrap();
                },
                _ => return Err(Object::new_error("error with minus".to_string())),
            }
            args = args.cdr()?;
        }
    
        Ok(Object::new_number(result.to_string()))
    }
    
}

