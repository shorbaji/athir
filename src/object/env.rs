use std::rc::Rc;
use std::cell::RefCell;
use std::ops::{DerefMut};
use crate::object::{Object, Value, ObjectExt, Key, Procedure};
use crate::stdlib::base::*;

pub trait Env {
    fn new() -> Object;
    fn new_env_with_parent(parent: &Object) -> Object;
    fn as_env(&self) -> Result<Object, Object>;
    fn lookup(var: Key, env: &Object) -> Result<Object, Object>;
    fn insert( &self, var: Key, val: &Object) -> Result<Object, Object>;
    fn init(&self) -> Result<Object, Object>;
}

impl Env for Object {
    fn new() -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Env(Object::null(), Object::new_map()))),
        }.init().unwrap()
    }

    fn new_env_with_parent(parent: &Object) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Env(parent.clone(), Object::new_map()))),
        }
    }

    fn as_env(&self) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Env(_, ref env) => Ok(env.clone()),
            _ => Err(Object::new_error("not an environment".to_string())),
        }
    }

    fn lookup(var: Key, env: &Object) -> Result<Object, Object> {
        match *env.borrow() {
            Value::Null => Err(Object::new_error(format!("unbound variable"))),
            Value::Env(ref parent, ref hm) => {
                match hm.borrow_mut().deref_mut() {
                    Value::Map(hm) => {
                        match hm.get(&var) {
                            Some(val) => Ok(val.clone()),
                            None => Object::lookup(var, parent),
                        }
                    },
                    _ => Err(Object::new_error(format!("not a map"))),
                }
            },
            _ => Err(Object::new_error(format!("not an env"))),
        }
    }

    fn insert(&self, var: Key, val: &Object) -> Result<Object, Object> {
        match *self.borrow_mut() {
            Value::Env(_, ref hm) => {
                match hm.borrow_mut().deref_mut() {
                    Value::Map(ref mut hm) => {
                        hm.insert(var, val.clone());
                        Ok(Object::unspecified())
                    },
                    _ => Err(Object::new_error(format!("not a map"))),
                }
            },
            _ => Err(Object::new_error(format!("not an env"))),
        }
    }

    fn init(&self) -> Result<Object, Object> {

        self.insert("car".to_string(), &Object::new_procedure(Procedure::Unary(car)))?;
        self.insert("cdr".to_string(), &Object::new_procedure(Procedure::Unary(cdr)))?;
        self.insert("cons".to_string(), &Object::new_procedure(Procedure::Binary(cons)))?;
        self.insert("caar".to_string(), &Object::new_procedure(Procedure::Unary(caar)))?;
        self.insert("cdar".to_string(), &Object::new_procedure(Procedure::Unary(cdar)))?;
        self.insert("cadr".to_string(), &Object::new_procedure(Procedure::Unary(cadr)))?;
        self.insert("cddr".to_string(), &Object::new_procedure(Procedure::Unary(cddr)))?;
        self.insert("caddr".to_string(), &Object::new_procedure(Procedure::Unary(caddr)))?;
        self.insert("cdadr".to_string(), &Object::new_procedure(Procedure::Unary(cdadr)))?;
        self.insert("eq?".to_string(), &Object::new_procedure(Procedure::Binary(eq)))?;
        self.insert("+".to_string(), &Object::new_procedure(Procedure::Variadic(add)))?;
        self.insert("*".to_string(), &Object::new_procedure(Procedure::Variadic(multiply)))?;
        self.insert("-".to_string(), &Object::new_procedure(Procedure::Variadic(subtract)))?;

        Ok(self.clone())
    }
}

