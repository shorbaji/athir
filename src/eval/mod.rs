#[cfg(test)]
mod tests;

use crate::object::{Value, Object, Keyword};

impl Object {

    pub fn eval(&self, env: &Object) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Boolean(_)
            | Value::Bytevector(_) 
            | Value::Character(_) 
            | Value::Number(_) 
            | Value::String(_)
            | Value::Vector(_) => Ok(self.clone()),
            | Value::Quotation(ref datum) => Ok(datum.clone()),
            | Value::Symbol(_) => env.lookup(self),
            | Value::Pair(ref car, ref cdr) => {
                match *car.borrow() {
                    Value::Keyword(Keyword::If) => iff(cdr, env),
                    Value::Keyword(Keyword::Lambda) => lambda(cdr, env),
                    Value::Keyword(Keyword::Define) => define(cdr, env),
                    Value::Keyword(Keyword::Set) => set(cdr, env),
                    Value::Keyword(Keyword::Quote) => quote(cdr),
                    _ => {
                        let operator = car.eval(env)?;
                        let operands = cdr.evlis(env)?;

                        operator.apply(&operands)
                    }
                }
            },
            _ => Err(Object::new_error(format!("Malformed expression"))),
        }
    }

    fn evlis(&self, env: &Object) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Null => Ok(Object::new(Value::Null)),
            Value::Pair(ref car, ref cdr) => car.eval(env)?.cons(&cdr.evlis(env)?),
            _ => Err(Object::new_error("Malformed args".to_string())),
        }
    }
    
    fn apply(&self, operands: &Object) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Builtin(_) => self.apply_as_builtin(operands),
            Value::Lambda(ref formals, ref body, ref parent) => self.apply_as_lambda(formals, body, parent, operands),
            _ => Err(Object::new_error(format!("not a procedure"))),
        }
    }
            
}

fn iff(expr: &Object, env: &Object) -> Result<Object, Object> {
    let test = expr.car()?;
    let borrow = test.borrow();
    
    match *borrow {
        Value::Boolean(_) => expr.cddr()?.eval(env),
        _ => expr.cadr()?.eval(env),
    }
}

fn lambda(expr: &Object, env: &Object) -> Result<Object, Object> {
    let formals = expr.car()?;
    let body = expr.cadr()?;

    Ok(Object::new_lambda(formals, body, env.clone()))
}

fn define(expr: &Object, env: &Object) -> Result<Object, Object> {
    let var = expr.car()?;
    let val = expr.cadr()?.eval(env)?;

    env.insert(&var, &val)
}

fn set(expr: &Object, env: &Object) -> Result<Object, Object> {
    env.lookup(&expr.car()?)?;
    define(expr, env)
}

fn quote(expr: &Object) -> Result<Object, Object> {
    Ok(expr.cadr()?)
}
