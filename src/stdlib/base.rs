use crate::object::{Object, Value, ObjectExt};

use std::rc::Rc;
use std::cell::RefCell;
use std::ops::Deref;

pub fn car(expr: Object) -> Result<Object, Object> {
    match expr.deref().borrow().deref() {
        Value::Pair(car, _) => Ok(car.clone()),
        _ => Err(Object::new_error(format!("Not a pair"))),
    }
}

pub fn cdr(expr: Object) -> Result<Object, Object> {
    match expr.deref().borrow().deref() {
        Value::Pair(_, cdr) => Ok(cdr.clone()),
        _ => Err(Object::new_error(format!("Not a pair"))),
    }
}

pub fn cons(expr: Object, cdr: Object) -> Result<Object, Object> {
    Ok(Rc::new(RefCell::new(Value::Pair(expr.clone(), cdr.clone()))))
}

pub fn caar(expr: Object) -> Result<Object, Object> {
    car(car(expr)?)
}

pub fn cadr(expr: Object) -> Result<Object, Object> {
    car(cdr(expr)?)
}

pub fn cdar(expr: Object) -> Result<Object, Object> {
    cdr(car(expr)?)
}

pub fn cddr(expr: Object) -> Result<Object, Object> {
    cdr(cdr(expr)?)
}

pub fn caddr(expr: Object) -> Result<Object, Object> {
    car(cddr(expr)?)
}

pub fn cdadr(expr: Object) -> Result<Object, Object> {
    cdr(cadr(expr)?)
}

pub fn len(expr: Object) -> Result<usize, Object> {
    match expr.borrow().deref() {
        Value::Null => Ok(0),
        Value::Pair(_, cdr) => len(cdr.clone()).map(|len| len + 1),
        _ => Err(Object::new_error(format!("Not a pair"))),
    }
}

pub fn multiply(args: Object) -> Result<Object, Object> {
    let mut result = 1;

    let mut args = args;

    while !args.is_null() {
        match car(args.clone())?.deref().borrow().deref() {
            Value::Number(num) => {
                result *= num.parse::<i64>().unwrap();
            },
            _ => return Err(Object::new_error("error with multiply".to_string())),
        }
        args = cdr(args)?;
    }

    Ok(Object::new_number(result.to_string()))
}

pub fn add(args: Object) -> Result<Object, Object> {
    let mut result = 0;

    let mut args = args;

    while !args.is_null() {
        match car(args.clone())?.deref().borrow().deref() {
            Value::Number(num) => {
                result += num.parse::<i64>().unwrap();
            },
            _ => return Err(Object::new_error("error with plus".to_string())),
        }
        args = cdr(args)?;
    }
    Ok(Object::new_number(result.to_string()))
}

pub fn subtract(args: Object) -> Result<Object, Object> {
    let mut result;

    let first = car(args.clone())?;
    match first.deref().borrow().deref() {
        Value::Number(num) => {
            result = num.parse::<i64>().unwrap();
        },
        _ => return Err(Object::new_error("error with minus".to_string())),
    }

    let mut args = cdr(args.clone())?;

    while !args.is_null() {
        match car(args.clone())?.deref().borrow().deref() {
            Value::Number(num) => {
                result -= num.parse::<i64>().unwrap();
            },
            _ => return Err(Object::new_error("error with minus".to_string())),
        }
        args = cdr(args)?;
    }

    Ok(Object::new_number(result.to_string()))
}

pub fn eq(a: Object, b: Object) -> Result<Object, Object> {

    let bool = a.deref().borrow().deref().clone() ==  b.deref().borrow().deref().clone();

    Ok(Object::new_boolean(bool))
}