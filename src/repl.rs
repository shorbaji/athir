//! Read eval print loop
//!
//! The REPL is a Read-Eval-Print-Loop that allows the user to interactively enter Lisp expressions and see the results of their evaluation.
//! Currently, the REPL runs the lexer and prints the tokens it finds since the parser and evaluator are not yet implemented.
//! 

use crate::read::Read;
use crate::eval::Eval; 
use std::io::Write; // print

pub fn repl() {

    let mut evaluator: Eval = Eval::new();
    let reader = Read::new(std::io::stdin().lines());
    
    print!("> ");
    std::io::stdout().flush().unwrap();

    for expr in reader {
        match expr {
            Ok(expr) => println!("{:?}", evaluator.eval_global(expr)),
            Err(err) => println!("{}", err),
        }
        print!("> ");
        std::io::stdout().flush().unwrap();
    }
}
