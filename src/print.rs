use crate::object::Object;

pub fn print(object: Object) -> Result<Object, Object>{
    println!("{:?}", object);
    Ok(object)
}