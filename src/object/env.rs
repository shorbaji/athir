use std::rc::Rc;
use std::cell::RefCell;
use crate::object::{Object, Value, Procedure, procedure::ProcedureKind, Boolean, Number, AthirString, Character, Pair, Vector, AthirError, Symbol, Map};

pub trait Env {
    fn new() -> Object;
    fn new_env_with_parent(parent: &Object) -> Object;
    fn as_env(&self) -> Result<Object, Object>;
    fn lookup(&self, symbol: &Object) -> Result<Object, Object>;
    fn insert( &self, symbol: &Object, val: &Object) -> Result<Object, Object>;
    fn init(&self) -> Result<Object, Object>;
    fn register(&self, symbol: &str, proc: ProcedureKind) -> Result<Object, Object>;
}

impl Env for Object {
    fn new() -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Env(Object::new_null(), <Object as Map>::new()))),
        }.init().unwrap()
    }

    fn new_env_with_parent(parent: &Object) -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Env(parent.clone(), <Object as Map>::new()))),
        }
    }

    fn as_env(&self) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Env(_, ref env) => Ok(env.clone()),
            _ => Err(<Object as AthirError>::new("not an environment".to_string())),
        }
    }

    fn lookup(&self, symbol: &Object) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Null => Err(<Object as AthirError>::new(format!("unbound variable"))),
            Value::Env(ref parent, ref hm) => {
                match <Object as Map>::lookup(hm, symbol) {
                    Ok(val) => Ok(val),
                    Err(_) => <Object as Env>::lookup(parent, symbol),
                }
            },
            _ => Err(<Object as AthirError>::new(format!("not an env"))),
        }
    }

    fn insert(&self, symbol: &Object, val: &Object) -> Result<Object, Object> {
        match *self.borrow_mut() {
            Value::Env(_, ref hm) => {
                <Object as Map>::insert(hm, symbol, val)
            },
            _ => Err(<Object as AthirError>::new(format!("not an env"))),
        }
    }

    fn register(&self, symbol: &str, kind: ProcedureKind) -> Result<Object, Object> {
        <Object as Env>::insert(self, &<Object as Symbol>::new(symbol.to_string()), &<Object as Procedure>::new(Value::Procedure(kind)))
    }

    fn init(&self) -> Result<Object, Object> {
       let pairs = vec!(
            ("car", ProcedureKind::Unary(Object::car)),
            ("cdr", ProcedureKind::Unary(Object::cdr)),
            ("cons", ProcedureKind::Binary(Object::cons)),
            ("caar", ProcedureKind::Unary(Object::caar)),
            ("cdar", ProcedureKind::Unary(Object::cdar)),
            ("cadr", ProcedureKind::Unary(Object::cadr)),
            ("cddr", ProcedureKind::Unary(Object::cddr)),
            ("caddr", ProcedureKind::Unary(Object::caddr)),
            ("cdadr", ProcedureKind::Unary(Object::cdadr)),
            ("eq?", ProcedureKind::Binary(Object::eq)),
            ("+", ProcedureKind::Variadic(Object::add)),
            ("*", ProcedureKind::Variadic(Object::multiply)),
            ("-", ProcedureKind::Variadic(Object::subtract)),
            ("boolean?", ProcedureKind::Unary(Object::is_boolean)),
            ("number?", ProcedureKind::Unary(Object::is_number)),
            ("string?", ProcedureKind::Unary(Object::is_string)),
            ("null?", ProcedureKind::Unary(Object::is_null)),
            ("procedure?", ProcedureKind::Unary(Object::is_procedure)),
            ("pair?", ProcedureKind::Unary(Object::is_pair)),
            ("char?", ProcedureKind::Unary(Object::is_character)),
            ("vector?", ProcedureKind::Unary(Object::is_vector)),
            ("len", ProcedureKind::Unary(Object::len)),
            ("read", ProcedureKind::Unary(crate::read::read))
        );

        for (symbol, kind) in pairs.into_iter() {
            self.register(symbol, kind)?;
        }

        Ok(self.clone())
    }
}

