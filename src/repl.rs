use crate::print::print;
use crate::read::read;
use crate::eval::eval;
use crate::object::{Port, env::Env, Object};

pub fn repl() -> Result<Object, Object> {
    let env = <Object as Env>::new();
    let port = <Object as Port>::new();
    
    loop { 
        read(port.clone())
        .and_then(|expr| eval(expr, env.clone()))
        .and_then(|val| print(val))
        .or_else(|err| print(err));
    }
}