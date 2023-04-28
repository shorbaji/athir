use crate::object::Object;

pub trait Print {
    fn print(&self) -> Result<Object, Object>;
}

impl Print for Object {
    fn print(&self) -> Result<Object, Object> {
        println!("{:?}", self);
        Ok(self.clone())
    }
}
