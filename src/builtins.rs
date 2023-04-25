use crate::object::Object;
use crate::result::AthirResult;
use crate::error::Error;
use crate::object::Builtin;

pub fn builtins() -> Vec<Builtin> {
    vec![
        Builtin::new("+", Some(2), None, plus),
        Builtin::new("-", Some(2), None, minus),
        Builtin::new("*", Some(2), None, multiply),
        Builtin::new("car", Some(1), Some(1), car),
        Builtin::new("cdr", Some(1), Some(1), cdr),
        Builtin::new("cadr", Some(1), Some(1), cadr),
        Builtin::new("caar", Some(1), Some(1), caar),
        Builtin::new("cdar", Some(1), Some(1), cdar),
        Builtin::new("cddr", Some(1), Some(1), cddr),
        Builtin::new("cons", Some(2), Some(1), cons),
        Builtin::new("boolean?", Some(1), Some(1), is_boolean),
        Builtin::new("bytevector?", Some(1), Some(1), is_bytevector),
        Builtin::new("char?", Some(1), Some(1), is_char),
        Builtin::new("eof-object?", Some(1), Some(1), is_eof_object),
        Builtin::new("null?", Some(1), Some(1), is_null),
        Builtin::new("number?", Some(1), Some(1), is_number),
        Builtin::new("pair?", Some(1), Some(1), is_pair),
        Builtin::new("port?", Some(1), Some(1), is_port),
        Builtin::new("procedure?", Some(1), Some(1), is_procedure),
        Builtin::new("string?", Some(1), Some(1), is_string),
        Builtin::new("symbol?", Some(1), Some(1), is_symbol),
        Builtin::new("vector?", Some(1), Some(1), is_vector),
    ]
}

fn is_boolean(args: &[Box<Object>]) -> AthirResult {
    Ok(Box::new(Object::Boolean(args[0].is_boolean())))
}

fn is_bytevector(args: &[Box<Object>]) -> AthirResult {
    Ok(Box::new(Object::Boolean(args[0].is_bytevector())))
}

fn is_char(args: &[Box<Object>]) -> AthirResult {
    Ok(Box::new(Object::Boolean(args[0].is_char())))
}

fn is_eof_object(args: &[Box<Object>]) -> AthirResult {
    Ok(Box::new(Object::Boolean(args[0].is_eof_object())))
}

fn is_null(args: &[Box<Object>]) -> AthirResult {
    Ok(Box::new(Object::Boolean(args[0].is_null())))
}

fn is_number(args: &[Box<Object>]) -> AthirResult {
    Ok(Box::new(Object::Boolean(args[0].is_number())))
}

fn is_pair(args: &[Box<Object>]) -> AthirResult {
    Ok(Box::new(Object::Boolean(args[0].is_pair())))
}

fn is_port(args: &[Box<Object>]) -> AthirResult {
    Ok(Box::new(Object::Boolean(args[0].is_port())))
}

fn is_procedure(args: &[Box<Object>]) -> AthirResult {
    Ok(Box::new(Object::Boolean(args[0].is_procedure())))
}

fn is_string(args: &[Box<Object>]) -> AthirResult {
    Ok(Box::new(Object::Boolean(args[0].is_string())))
}

fn is_symbol(args: &[Box<Object>]) -> AthirResult {
    Ok(Box::new(Object::Boolean(args[0].is_symbol())))
}

fn is_vector(args: &[Box<Object>]) -> AthirResult {
    Ok(Box::new(Object::Boolean(args[0].is_vector())))
}

fn car(args: &[Box<Object>]) -> AthirResult {
    Ok(Box::new(*args[0].car()?.clone()))
}

fn cdr(args: &[Box<Object>]) -> AthirResult {
    Ok(Box::new(*args[0].cdr()?.clone()))
}

fn cadr(args: &[Box<Object>]) -> AthirResult {
    Ok(Box::new(*args[0].cadr()?.clone()))
}

fn caar(args: &[Box<Object>]) -> AthirResult {
    Ok(Box::new(*args[0].car()?.car()?.clone()))
}

fn cdar(args: &[Box<Object>]) -> AthirResult {
    Ok(Box::new(*args[0].car()?.cdr()?.clone()))
}

fn cons(args: &[Box<Object>]) -> AthirResult {
    Ok(args[0].cons(*args[1].clone())?)
}

fn cddr(args: &[Box<Object>]) -> AthirResult {
    Ok(Box::new(*args[0].cdr()?.cdr()?.clone()))
}

fn multiply(args: &[Box<Object>]) -> AthirResult {
    let mut result = 1;
    for arg in args {
        match &**arg {
            Object::Number(num) => {
                result *= num.parse::<i64>().unwrap();
            },
            _ => return Err(Error::EvalError("error with multiply".to_string())),
        }
    }
    Ok(Box::new(Object::Number(result.to_string())))
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
