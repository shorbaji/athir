use crate::object::Object;

pub fn repl() -> Result<Object, Object> {
    let env = Object::new_global_env();
    let port =  Object::from(std::io::stdin());

    loop { 
        port
        .read()?
        .eval(&env)?
        .print()?;
    }
}