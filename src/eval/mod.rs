#[cfg(test)]
mod tests;

use crate::object::{Value::*, Object, Value};
use crate::object::Keyword::*;
impl Object {
    pub fn eval(&self, env: &Object) -> Result<Object, Object> {
        match *self.borrow() {
            _ if self.quotable()?.into() => self.quote(),
            Symbol(_) => env.lookup(self),
            Quotation(ref datum) => datum.quote(),
            Pair(ref car, ref cdr) => {
                match car {
                    _ if car.is_keyword()?.into() => match *car.borrow() {
                        Keyword(If) => cdr.iff( env),
                        Keyword(Lambda) => cdr.lambda(env),
                        Keyword(Define) => cdr.define(env),
                        Keyword(Set) => cdr.set(env),
                        Keyword(Quote) => cdr.quote(),    
                        _ => Err(Object::new_error(format!("Unknown keyword"))),
                    }
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
            Null => Ok(Object::new(Null)),
            Pair(ref car, ref cdr) => car.eval(env)?.cons(&cdr.evlis(env)?),
            _ => Err(Object::new_error("Malformed args".to_string())),
        }
    }
    
    fn apply(&self, operands: &Object) -> Result<Object, Object> {
        match *self.borrow() {
            Builtin(_) => self.apply_as_builtin(operands),
            Closure(ref formals, ref body, ref parent) => self.apply_as_lambda(formals, body, parent, operands),
            _ => Err(Object::new_error(format!("apply: not a procedure"))),
        }
    }

    fn quotable(&self) -> Result<Object, Object> {
        Ok(Object::from(self.is_boolean()?.into() 
                            || self.is_bytevector()?.into()
                            || self.is_character()?.into()
                            || self.is_number()?.into()
                            || self.is_string()?.into() 
                            || self.is_vector()?.into()))
    }

    fn iff(&self,env: &Object) -> Result<Object, Object> {
        match *self.car()?.eval(env)?.borrow() {        
            Boolean(_) => self.cddr()?.eval(env),
            _ => self.cadr()?.eval(env),
        }
    }
    
    fn lambda(&self, env: &Object) -> Result<Object, Object> {
        let formals = self.car()?;
        let body = self.cadr()?;
    
        Ok(Object::new_lambda(&formals, &body, env))
    }
    
    fn define(&self, env: &Object) -> Result<Object, Object> {
        let var = self.car()?;

        let expr = self.cadr()?;
        let rest = self.cddr()?;

        if rest.is_null()?.into() {
            env.insert(
                &var,
                &expr
            )?;
        } else {
            env.insert(
                &var, 
                &self.cdr()?.lambda(env)?
            )?;
        }
        Ok(Object::new(Value::Unspecified))
    }
    
    fn set(&self, env: &Object) -> Result<Object, Object> {
        env.lookup(&self.car()?)?;
        self.define(env)
    }
    
    fn quote(&self) -> Result<Object, Object> {
        Ok(self.clone())
    }
    
}

