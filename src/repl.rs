use crate::print::print;
use crate::read::read;
use crate::eval::eval;
use crate::object::{env::Env, Port, Object};

pub fn repl() -> Result<Object, Object> {
    let env = <Object as Env>::new();
    let port = <Object as Port>::new();
    
    loop { 
        let expr = read(port.clone());

        match expr {
            Ok(expr) => { print(eval(&expr, &env)?)?; },
            Err(err) => { return Err(err); }
        };
    }
}