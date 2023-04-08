//! **athir** is a Cloud Lisp engine
//! 
//! Language features are:
//! - native multi-tenancy with first-class identity (and access management)
//! - a universal, supra-global, persistent scope
//! - a universal distributed VM 
//! 

mod lexer;
mod tools;

mod repl {
    use crate::lexer::*;
    use logos::Logos;
    use std::io::Write;

    pub fn repl() {
        print!("> ");
        std::io::stdout().flush().unwrap();
            loop {
    
            for line in std::io::stdin().lines() {
                let line = line.unwrap();
                let lex = Token::lexer(&line);
                for token in lex {
                    println!("token: {:?}", token);
                }
                print!("> ");
                std::io::stdout().flush().unwrap();
            }
        }
    }    
}

use repl::repl;

fn main() -> std::io::Result<()>{
    println!("athir (c) 2023 Omar Shorbaji");
    repl();
    Ok(())
}
