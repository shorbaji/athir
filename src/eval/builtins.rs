use crate::object::*;
use crate::result::EvalResult;
use std::rc::Rc;
use std::cell::RefCell;
use crate::error::Error;
use std::ops::Deref;

pub fn builtins() -> Vec<(&'static str, Option<usize>, Option<usize>, fn(Rc<RefCell<Object>>) -> EvalResult)> {
    vec![
        ("+", Some(2), None, plus),
        ("-", Some(2), None, minus),
        ("*", Some(2), None, multiply),
        ("=", Some(2), None, equal),
        ("car", Some(1), Some(1), car),
        ("cdr", Some(1), Some(1), cdr),
        ("cadr", Some(1), Some(1), cadr),
        ("caar", Some(1), Some(1), caar),
        ("cdar", Some(1), Some(1), cdar),
        ("cddr", Some(1), Some(1), cddr),
        ("cons", Some(2), Some(2), cons),
        ("boolean?", Some(1), Some(1), is_boolean),
        ("bytevector?", Some(1), Some(1), is_bytevector),
        ("char?", Some(1), Some(1), is_char),
        ("eof-object?", Some(1), Some(1), is_eof_object),
        ("null?", Some(1), Some(1), is_null),
        ("number?", Some(1), Some(1), is_number),
        ("pair?", Some(1), Some(1), is_pair),
        ("port?", Some(1), Some(1), is_port),
        ("procedure?", Some(1), Some(1), is_procedure),
        ("string?", Some(1), Some(1), is_string),
        ("symbol?", Some(1), Some(1), is_symbol),
        ("vector?", Some(1), Some(1), is_vector),
    ]
}


fn multiply(args: Rc<RefCell<Object>>) -> EvalResult {
    let mut result = 1;

    let args = args.deref().borrow().as_list()?;
    for arg in args {
        match arg.deref().borrow().deref() {
            Object::Number(num) => {
                result *= num.parse::<i64>().unwrap();
            },
            _ => return Err(Error::EvalError("error with multiply".to_string())),
        }
    }

    Ok(Rc::new(RefCell::new(Object::Number(result.to_string()))))
}

fn plus(args: Rc<RefCell<Object>>) -> EvalResult {
    let mut result = 0;

    let args = args.deref().borrow().as_list()?;
    for arg in args {
        match arg.deref().borrow().deref() {
            Object::Number(num) => {
                result += num.parse::<i64>().unwrap();
            },
            _ => return Err(Error::EvalError("error with plus".to_string())),
        }
    }

    Ok(Rc::new(RefCell::new(Object::Number(result.to_string()))))
}

fn minus(args: Rc<RefCell<Object>>) -> EvalResult {
    let mut result;

    let args = args.deref().borrow().as_list()?;
    match args[0].deref().borrow().deref() {
        Object::Number(num) => {
            result = num.parse::<i64>().unwrap();
        },
        _ => return Err(Error::EvalError("error with minus".to_string())),
    }

    for arg in &args[1..] {
        match arg.deref().borrow().deref() {
            Object::Number(num) => {
                result -= num.parse::<i64>().unwrap();
            },
            _ => return Err(Error::EvalError("error with minus".to_string())),
        }
    }

    Ok(Rc::new(RefCell::new(Object::Number(result.to_string()))))
}

fn equal(args: Rc<RefCell<Object>>) -> EvalResult {
    let args = args.deref().borrow().as_list()?;

    let a = &*args[0];
    let b = &*args[1];

    Ok(Rc::new(RefCell::new(Object::Boolean(a == b))))
}