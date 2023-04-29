use crate::object::Object;

pub fn repl() -> Result<Object, Object> {
    let env = Object::new_global_env();
    let port =  Object::from(std::io::stdin());

    println!("athir (c) 2023 Athir LLC");

    loop { 
        port
        .read()?
        .eval(&env)?
        .print()?;
    }
}