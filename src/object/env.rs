use std::rc::Rc;
use std::cell::RefCell;
use std::ops::{DerefMut};
use crate::object::{Object, Value, Key, Procedure, Boolean};

pub trait Env {
    fn new() -> Object;
    fn new_env_with_parent(parent: &Object) -> Object;
    fn as_env(&self) -> Result<Object, Object>;
    fn lookup(var: Key, env: &Object) -> Result<Object, Object>;
    fn insert( &self, var: Key, val: &Object) -> Result<Object, Object>;
    fn init(&self) -> Result<Object, Object>;
    fn register(&self, symbol: &str, proc: Procedure) -> Result<Object, Object>;

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

    fn register(&self, symbol: &str, proc: Procedure) -> Result<Object, Object> {
        self.insert(symbol.to_string(), &Object::new_procedure(proc))
    }

    fn init(&self) -> Result<Object, Object> {
       let pairs = vec!(
            ("car", Procedure::Unary(Object::car)),
            ("cdr", Procedure::Unary(Object::cdr)),
            ("cons", Procedure::Binary(Object::cons)),
            ("caar", Procedure::Unary(Object::caar)),
            ("cdar", Procedure::Unary(Object::cdar)),
            ("cadr", Procedure::Unary(Object::cadr)),
            ("cddr", Procedure::Unary(Object::cddr)),
            ("caddr", Procedure::Unary(Object::caddr)),
            ("cdadr", Procedure::Unary(Object::cdadr)),
            ("eq?", Procedure::Binary(Object::eq)),
            ("+", Procedure::Variadic(Object::add)),
            ("*", Procedure::Variadic(Object::multiply)),
            ("-", Procedure::Variadic(Object::subtract)),
            ("boolean?", Procedure::Unary(Object::is_boolean)),
        );

        for (symbol, proc) in pairs.into_iter() {
            self.register(symbol, proc)?;
        }

        Ok(self.clone())
    }
}

