use crate::object::{Object, Value, };

#[derive(Clone)]
pub enum BuiltIn {
    Unary(fn(&Object) -> Result<Object, Object>),
    Binary(fn(&Object, &Object) -> Result<Object, Object>),
    Variadic(fn(&Object) -> Result<Object, Object>),
}

impl From<Object> for bool {
    fn from(value: Object) -> Self {
        match *value.borrow() {
            Value::Boolean(b) => b,
            _ => true,
        }
    }
}
impl Object {
    pub fn new_procedure(value: Value) -> Object {
        Object::new(value)
    }

    pub fn is_procedure(&self) -> Result<Object, Object> {
       Ok(Object::from(self.is_built_in()?.into() || self.is_lambda()?.into()))
    }

    fn is_built_in(&self) -> Result<Object, Object> {
        Ok(Object::from(matches!(*self.borrow(), Value::Builtin(_))))
    }

    pub fn apply_as_builtin(&self, args: &Object) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Builtin(BuiltIn::Unary(f)) => f(&args.car()?),
            Value::Builtin(BuiltIn::Binary(f)) => f(&args.car()?, &args.cadr()?),
            Value::Builtin(BuiltIn::Variadic(f)) => f(args),
            _ => Err(Object::new_error(format!("not a procedure"))),
        }
    }
    
}

impl std::fmt::Debug for BuiltIn {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            BuiltIn::Unary(_) => write!(f, "Unary"),
            BuiltIn::Binary(_) => write!(f, "Binary"),
            BuiltIn::Variadic(_) => write!(f, "Variadic"),
        }
    }
}
impl PartialEq for BuiltIn {
    fn eq(&self, other: &BuiltIn) -> bool {
        match (self, other) {
            (BuiltIn::Unary(_), BuiltIn::Unary(_)) => true,
            (BuiltIn::Binary(_), BuiltIn::Binary(_)) => true,
            (BuiltIn::Variadic(_), BuiltIn::Variadic(_)) => true,
            _ => false,
        }
    }
}
