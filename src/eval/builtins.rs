use crate::object::*;
use crate::result::EvalResult;
use std::rc::Rc;
use std::cell::RefCell;

pub fn builtins() -> Vec<(&'static str, Option<usize>, Option<usize>, fn(Rc<RefCell<Object>>) -> EvalResult)> {
    vec![
        // ("+", Some(2), None, plus),
        // ("-", Some(2), None, minus),
        // ("*", Some(2), None, multiply),
        // ("=", Some(2), None, equal),
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


// fn multiply(args: &[Box<Object>]) -> EvalResult {
//     let mut result = 1;
//     for arg in args {
//         match &**arg {
//             Object::Number(num) => {
//                 result *= num.parse::<i64>().unwrap();
//             },
//             _ => return Err(Error::EvalError("error with multiply".to_string())),
//         }
//     }
//     Ok(Box::new(Object::Number(result.to_string())))
// }

// fn plus(args: &[Box<Object>]) -> EvalResult {
//     let mut result = 0;
//     for arg in args {
//         match &**arg {
//             Object::Number(num) => {
//                 result += num.parse::<i64>().unwrap();
//             },
//             _ => return Err(Error::EvalError("error with plus".to_string())),
//         }
//     }

//     Ok(Box::new(Object::Number(result.to_string())))
// }

// fn minus(args: &[Box<Object>]) -> EvalResult {
//     let mut result;

//     match &*args[0] {
//         Object::Number(num) => {
//             result = num.parse::<i64>().unwrap();
//         },
//         _ => return Err(Error::EvalError("error with minus".to_string())),
//     }

//     for arg in &args[1..] {
//         match &**arg {
//             Object::Number(num) => {
//                 result -= num.parse::<i64>().unwrap();
//             },
//             _ => return Err(Error::EvalError("error with minus".to_string())),
//         }
//     }

//     Ok(Box::new(Object::Number(result.to_string())))
// }

// fn equal(args: &[Box<Object>]) -> EvalResult {
//     let a = &*args[0];
//     let b = &*args[1];

//     Ok(Box::new(Object::Boolean(a == b)))
// }