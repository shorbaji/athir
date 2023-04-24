//! Read eval print loop
//!
//! The REPL is a Read-Eval-Print-Loop that allows the user to interactively enter Lisp expressions and see the results of their evaluation.
//! Currently, the REPL runs the lexer and prints the tokens it finds since the parser and evaluator are not yet implemented.
//! 

use std::io::Write;

pub fn repl() {

    let mut reader = crate::read::Reader::new(std::io::stdin().lines());
    let mut eval = crate::eval::Eval::new(reader);

    print!("> ");
    std::io::stdout().flush().unwrap();

    for object in eval {
        match object {
            Ok(object) => println!("{:?}", object),
            Err(err) => println!("{}", err),
        }
        print!("> ");
        std::io::stdout().flush().unwrap();
    }
}
