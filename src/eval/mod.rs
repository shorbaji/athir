#[cfg(test)]
mod tests;

use crate::object::{Value, Object, Procedure, Lambda, Keyword, Env, Pair, AthirError };

pub fn eval(expr: &Object, env: &Object) -> Result<Object, Object> {
    match *expr.borrow() {
        Value::Boolean(_)
        | Value::Bytevector(_) 
        | Value::Character(_) 
        | Value::Number(_) 
        | Value::String(_)
        | Value::Vector(_) => Ok(expr.clone()),
        | Value::Symbol(_) => env.lookup(expr),
        | Value::Pair(ref car, ref cdr) => {
            match *car.borrow() {
                Value::Keyword(Keyword::If) => iff(&cdr, env),
                Value::Keyword(Keyword::Lambda) => lambda(&cdr, env),
                Value::Keyword(Keyword::Define) => define(&cdr, env),
                Value::Keyword(Keyword::Set) => assignment(&cdr, env),
                Value::Keyword(Keyword::Quote) => quote(&cdr),
                _ => {
                    let operator = eval(&car, env)?;
                    let operands = evlis(&cdr, env)?;

                    let borrow = operator.borrow();
                    match *borrow {
                        Value::Procedure(_) => <Object as Procedure>::apply(&operator, &operands),
                        Value::Lambda(_, _, _) => <Object as Procedure>::apply(&operator, &operands),
                        _ => Err(<Object as AthirError>::new(format!("not a procedure"))),
                    }
                }
            }
        },
        | Value::Quotation(ref quoted) => Ok(quoted.clone()),
        _ => Err(<Object as AthirError>::new(format!("Malformed expression"))),
    }
}

fn evlis(args: &Object, env: &Object) -> Result<Object, Object> {

    match *args.borrow() {
        Value::Null => Ok(args.clone()),
        Value::Pair(ref car, ref cdr) => eval(car, env)?.cons(&evlis(cdr, env)?),
        _ => Err(<Object as AthirError>::new("Malformed args".to_string())),
    }
}

fn iff(expr: &Object, env: &Object) -> Result<Object, Object> {
    let test = expr.car()?;
    let borrow = test.borrow();
    
    match *borrow {
        Value::Boolean(_) => eval(&expr.cddr()?, env),
        _ => eval(&expr.cadr()?, env),
    }
}

fn lambda(expr: &Object, env: &Object) -> Result<Object, Object> {
    let formals = expr.car()?;
    let body = expr.cadr()?;

    Ok(<Object as Lambda>::new(formals, body, env.clone()))
}

fn define(expr: &Object, env: &Object) -> Result<Object, Object> {
    let var = expr.car()?;
    let val = eval(&expr.cadr()?, env)?;

    env.insert(&var, &val)
}

fn assignment(expr: &Object, env: &Object) -> Result<Object, Object> {
    env.lookup(&expr.car()?)?;
    define(expr, env)
}

fn quote(expr: &Object) -> Result<Object, Object> {
    Ok(expr.cadr()?)
}
