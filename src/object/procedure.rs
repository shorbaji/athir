use crate::object::{Object, Value, Boolean, AthirError, Pair};

use std::rc::Rc;
use std::cell::RefCell;

#[derive(Clone)]
pub enum ProcedureKind {
    Unary(fn(&Object) -> Result<Object, Object>),
    Binary(fn(&Object, &Object) -> Result<Object, Object>),
    Variadic(fn(&Object) -> Result<Object, Object>),
}

pub trait Procedure {
    fn new(value: Value) -> Object;
    fn new_unary(value: fn(&Object) -> Result<Object, Object>) -> Object;
    fn new_binary(value: fn(&Object, &Object) -> Result<Object, Object>) -> Object;
    fn new_variadic(value: fn(&Object) -> Result<Object, Object>) -> Object;
    fn is_procedure(&self) -> Result<Object, Object>;
    fn as_procedure(&self) -> Result<Object, Object>;
    fn apply(&self, args: &Object) -> Result<Object, Object>;
}

impl Procedure for Object {
    fn new(value: Value) -> Object {
        Object {
            value: Rc::new(RefCell::new(value)),
        }
    }

    fn new_unary(value: fn(&Object) -> Result<Object, Object>) -> Object {
        <Object as Procedure>::new(Value::Procedure(ProcedureKind::Unary(value)))
    }

    fn new_binary(value: fn(&Object, &Object) -> Result<Object, Object>) -> Object {
        <Object as Procedure>::new(Value::Procedure(ProcedureKind::Binary(value)))
    }

    fn new_variadic(value: fn(&Object) -> Result<Object, Object>) -> Object {
        <Object as Procedure>::new(Value::Procedure(ProcedureKind::Variadic(value)))
    }

    fn is_procedure(&self) -> Result<Object, Object> {
        Ok(<Object as Boolean>::new(self.as_procedure().is_ok()))
    }

    fn as_procedure(&self) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Procedure(_) => Ok(self.clone()),
            _ => Err(<Object as AthirError>::new(format!("not a procedure"))),
        }
    }

    fn apply(&self, args: &Object) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Procedure(ProcedureKind::Unary(f)) => f(&args.car()?),
            Value::Procedure(ProcedureKind::Binary(f)) => f(&args.car()?, &args.cadr()?),
            Value::Procedure(ProcedureKind::Variadic(f)) => f(args),
            _ => Err(<Object as AthirError>::new(format!("not a procedure"))),
        }
    }
    
}

impl std::fmt::Debug for ProcedureKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            ProcedureKind::Unary(_) => write!(f, "Unary"),
            ProcedureKind::Binary(_) => write!(f, "Binary"),
            ProcedureKind::Variadic(_) => write!(f, "Variadic"),
        }
    }
}
impl PartialEq for ProcedureKind {
    fn eq(&self, other: &ProcedureKind) -> bool {
        match (self, other) {
            (ProcedureKind::Unary(_), ProcedureKind::Unary(_)) => true,
            (ProcedureKind::Binary(_), ProcedureKind::Binary(_)) => true,
            (ProcedureKind::Variadic(_), ProcedureKind::Variadic(_)) => true,
            _ => false,
        }
    }
}
