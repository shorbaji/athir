use crate::object::Object;
use crate::result::AthirResult;
use crate::error::Error;
use crate::object::Builtin;

pub fn builtins() -> Vec<Builtin> {
    vec![
        Builtin::new("+", Some(2), None, plus),
        Builtin::new("-", Some(2), None, minus),
    ]
}

fn plus(args: &[Box<Object>]) -> AthirResult {
    let mut result = 0;
    for arg in args {
        match &**arg {
            Object::Number(num) => {
                result += num.parse::<i64>().unwrap();
            },
            _ => return Err(Error::EvalError("error with plus".to_string())),
        }
    }

    Ok(Box::new(Object::Number(result.to_string())))
}

fn minus(args: &[Box<Object>]) -> AthirResult {
    let mut result;

    match &*args[0] {
        Object::Number(num) => {
            result = num.parse::<i64>().unwrap();
        },
        _ => return Err(Error::EvalError("error with minus".to_string())),
    }

    for arg in &args[1..] {
        match &**arg {
            Object::Number(num) => {
                result -= num.parse::<i64>().unwrap();
            },
            _ => return Err(Error::EvalError("error with minus".to_string())),
        }
    }

    Ok(Box::new(Object::Number(result.to_string())))
}
