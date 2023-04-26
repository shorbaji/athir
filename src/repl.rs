//! Read eval print loop
//!
//! The REPL is a Read-Eval-Print-Loop that allows the user to interactively enter Lisp expressions and see the results of their evaluation.
//! Currently, the REPL runs the lexer and prints the tokens it finds since the parser and evaluator are not yet implemented.
//! 

use crate::read::StdinRead;
use crate::eval::Eval; 
use std::io::Write; // print

pub fn repl() {

    let mut eval: Eval = Eval::new();
    let read = StdinRead::new();
    
    print!("> ");
    std::io::stdout().flush().unwrap();

    for expr in read {
        match expr {
            Ok(expr) => println!("{:?}", eval.eval_global(expr)),
            Err(err) => println!("{}", err),
        }
        print!("> ");
        std::io::stdout().flush().unwrap();
    }
}
