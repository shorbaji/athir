use crate::object::{Object, Value};

impl Object {
    pub fn print(&self) -> Result<Object, Object> {
        println!("{:?}", self);
        Ok(self.clone())
    }

    pub fn display(&self) -> Result<Object, Object> {
        print!("{:?}", self);
        Ok(Object::new(Value::Unspecified))
    }

    pub fn new_line(&self) -> Result<Object, Object> {
        println!("{:?}", self);
        Ok(Object::new(Value::Unspecified))
    }

}
