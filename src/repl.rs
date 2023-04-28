use crate::print::print;
use crate::read::read;
use crate::eval::eval;
use crate::object::{env::Env, Port, Object};

pub fn repl() -> Result<Object, Object> {
    let env = <Object as Env>::new();
    let port = <Object as Port>::new();
    
    loop { 
        let expr = read(port.clone())?;

        eval(&expr, &env)
            .and_then(|object| print(object))
            .or_else(|object| print(object))?;
    }
}