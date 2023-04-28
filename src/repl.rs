use crate::read::read;
use crate::eval::Eval;
use crate::print::Print;
use crate::object::{env::Env, Port, Object};

pub fn repl() -> Result<Object, Object> {
    let env = <Object as Env>::new();
    let port = <Object as Port>::new();

    loop { 
        read(&port)?
        .eval(&env)?
        .print()?;
    }
}