use crate::read::read;
use crate::eval::Eval;
use crate::print::Print;
use crate::object::Object;

pub fn repl() -> Result<Object, Object> {
    let env = Object::new_global_env();
    let port = Object::new_port();

    loop { 
        read(&port)?
        .eval(&env)?
        .print()?;
    }
}