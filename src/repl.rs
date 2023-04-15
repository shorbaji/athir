//! Read eval print loop
//!
//! The REPL is a Read-Eval-Print-Loop that allows the user to interactively enter Lisp expressions and see the results of their evaluation.
//! Currently, the REPL runs the lexer and prints the tokens it finds since the parser and evaluator are not yet implemented.
//! 

use std::io::Write;

use crate::read::Reader;
use crate::eval::eval;
use crate::error::{Error, ErrorKind::*};

pub fn repl() {
    print!("> ");
    std::io::stdout().flush().unwrap();
    loop {
        for line in std::io::stdin().lines() {
            let line = line.unwrap();
            Reader::new(&line).read().and_then(|tree|
                eval(&tree).and_then(|object| {
                    println!("object: {:?}", object);
                    print!("> ");
                    std::io::stdout().flush().unwrap();
                    Ok(object)
                })
            ).or(Err(Error::new(EvalError)));
        }
    }
}
