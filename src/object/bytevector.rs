use crate::object::{Object, Value};

impl From<Vec<u8>> for Object {
    fn from(value: Vec<u8>) -> Object {
        Object::new(Value::Bytevector(value.into()))
    }
}

// impl Into<Result<Vec<u8>, Object>> for &Object {
//     fn into(self) -> Result<Vec<u8>, Object> {
//         match *(self.borrow()) {
//             Value::Bytevector(ref value) => Ok(value.clone()),
//             _ => Err(<Object as AthirError>::new(format!("not a bytevector"))),
//         }
//     }
// }

impl Object {
    pub fn as_bytevector_from(value: Object) -> Object {
        Object::new(Value::Bytevector(value))
    }

    pub fn is_bytevector(&self) -> Result<Object, Object> {
        Ok(Object::from(matches!(*self.borrow(), Value::Bytevector(_))))
    }
}

