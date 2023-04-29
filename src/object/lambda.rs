use crate::object::{Value, Object,};

impl Object {
    pub fn new_lambda(formals: Object, body: Object, parent: Object) -> Object {
        Object::new(Value::Closure(formals, body, parent))
    }

    pub fn is_lambda(&self) -> Result<Object, Object> {
        Ok(Object::from(matches!(*self.borrow(), Value::Closure(_, _, _))))
    }

    pub fn apply_as_lambda(&self, formals: &Object, body: &Object, parent: &Object, args: &Object) -> Result<Object, Object> {
    
        if args.len()? == formals.len()? {
            let new_env = Object::new_env_with_parent(parent);
    
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
                result = expr.eval(&new_env)?;
                body = body.cdr()?;
            };
    
            Ok(result)
        } else {
            Err(Object::new_error(format!("Wrong number of arguments")))
        }
    
    }
    
}