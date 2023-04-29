use crate::object::Object;

impl Object {
    pub fn print(&self) -> Result<Object, Object> {
        println!("{:?}", self);
        Ok(self.clone())
    }
}
