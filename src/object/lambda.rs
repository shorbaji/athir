use crate::object::{Value, Object, Env, Pair, AthirError, Boolean };
use crate::eval::eval;

pub trait Lambda {
    fn new(formals: Object, body: Object, parent: Object) -> Object;
    fn as_lambda(&self) -> Result<Object, Object>;
    fn is_lambda(&self) -> Result<Object, Object>;
    fn apply(&self, formals: &Object, body: &Object, parent: &Object, args: &Object) -> Result<Object, Object>;
}

impl Lambda for Object {
    fn new(formals: Object, body: Object, parent: Object) -> Object {
        Object::new(Value::Lambda(formals, body, parent))
    }

    fn as_lambda(&self) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Lambda(_, _, _) => Ok(self.clone()),
            _ => Err(<Object as AthirError>::new(format!("not a lambda"))),
        }
    }

    fn is_lambda(&self) -> Result<Object, Object> {
        Ok(<Object as Boolean>::new(self.as_lambda().is_ok()))
    }

    fn apply(&self, formals: &Object, body: &Object, parent: &Object, args: &Object) -> Result<Object, Object> {
    
        if args.len()? == formals.len()? {
            let new_env = <Object as Env>::new_env_with_parent(parent);
    
            let mut args = args.clone();
            let mut formal:Object;
            let mut formals = formals.clone();
    
            while !matches!(*formals.borrow(), Value::Null) {
                formal = formals.car()?;
                let arg = args.car()?;
    
                new_env.insert(&formal, &arg)?;
    
                formals = formals.cdr()?;
    
                args = args.cdr()?;
    
            };
    
            let mut body = body.clone();
            let mut expr: Object;
            let mut result = Object::new(Value::Unspecified);
    
            while !matches!(*body.borrow(), Value::Null) {
                expr = formals.car()?;
                result = eval(&expr, &new_env)?;
                body = body.cdr()?;
            };
    
            Ok(result)
        } else {
            Err(<Object as AthirError>::new(format!("Wrong number of arguments")))
        }
    
    }
    
}