use std::collections::HashMap;

use crate::object::{Object, Value, procedure::BuiltIn,};

impl Object {
    pub fn new_global_env() -> Object {
        Object::new(Value::Env(Object::new(Value::Null), Object::from(HashMap::new()))).init().unwrap()
    }

    pub fn new_env_with_parent(parent: &Object) -> Object {
        Object::new(Value::Env(parent.clone(), Object::from(HashMap::new())))
    }

    pub fn lookup(&self, symbol: &Object) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Null => Err(Object::new_error(format!("unbound variable"))),
            Value::Env(ref parent, ref hm) => {
                match hm.map_lookup(symbol) {
                    Ok(val) => Ok(val),
                    Err(_) => parent.lookup(symbol),
                }
            },
            _ => Err(Object::new_error(format!("not an env"))),
        }
    }

    pub fn insert(&self, symbol: &Object, val: &Object) -> Result<Object, Object> {
        match *self.borrow_mut() {
            Value::Env(_, ref hm) => {
                hm.map_insert(symbol, val)
            },
            _ => Err(Object::new_error(format!("not an env"))),
        }
    }

    fn register(&self, symbol: &str, kind: BuiltIn) -> Result<Object, Object> {
       self.insert(&Object::new_symbol(symbol.to_string()), &Object::new_procedure(Value::Builtin(kind)))
    }

    fn init(&self) -> Result<Object, Object> {
       let pairs = vec!(
            ("car", BuiltIn::Unary(Object::car)),
            ("cdr", BuiltIn::Unary(Object::cdr)),
            ("cons", BuiltIn::Binary(Object::cons)),
            ("caar", BuiltIn::Unary(Object::caar)),
            ("cdar", BuiltIn::Unary(Object::cdar)),
            ("cadr", BuiltIn::Unary(Object::cadr)),
            ("cddr", BuiltIn::Unary(Object::cddr)),
            ("caddr", BuiltIn::Unary(Object::caddr)),
            ("cdadr", BuiltIn::Unary(Object::cdadr)),
            ("eq?", BuiltIn::Binary(Object::eq)),
            ("+", BuiltIn::Variadic(Object::add)),
            ("*", BuiltIn::Variadic(Object::multiply)),
            ("-", BuiltIn::Variadic(Object::subtract)),
            ("atom?", BuiltIn::Unary(Object::is_atom)),
            ("boolean?", BuiltIn::Unary(Object::is_boolean)),
            ("bytevector?", BuiltIn::Unary(Object::is_bytevector)),
            ("char?", BuiltIn::Unary(Object::is_character)),
            ("number?", BuiltIn::Unary(Object::is_number)),
            ("map?", BuiltIn::Unary(Object::is_map)),
            ("null?", BuiltIn::Unary(Object::is_null)),
            ("pair?", BuiltIn::Unary(Object::is_pair)),
            ("procedure?", BuiltIn::Unary(Object::is_procedure)),
            ("string?", BuiltIn::Unary(Object::is_string)),
            ("symbol?", BuiltIn::Unary(Object::is_symbol)),
            ("vector?", BuiltIn::Unary(Object::is_vector)),
            ("len", BuiltIn::Unary(Object::len)),
            ("read", BuiltIn::Unary(Object::read)),
            ("display", BuiltIn::Unary(Object::display)),
            ("new-line", BuiltIn::Unary(Object::new_line)),
        );

        for (symbol, kind) in pairs.into_iter() {
            self.register(symbol, kind)?;
        }

        Ok(self.clone())
    }
}

