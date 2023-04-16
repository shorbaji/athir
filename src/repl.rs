//! Read eval print loop
//!
//! The REPL is a Read-Eval-Print-Loop that allows the user to interactively enter Lisp expressions and see the results of their evaluation.
//! Currently, the REPL runs the lexer and prints the tokens it finds since the parser and evaluator are not yet implemented.
//! 

use std::io::Write;

use crate::read::parser::Parser;
use crate::read::lexer::Source;

use crate::eval::eval;

pub fn repl() {
    print!("> ");
    std::io::stdout().flush().unwrap();

    let source = Source::new(std::io::stdin().lines().map(|line| line.unwrap()));
    let parser = Parser::new(source);

    for expr in parser {
        match expr {
            Ok(expr) => println!("{:?}", eval(&expr)),
            Err(err) => println!("{}", err),
        }
        print!("> ");
        std::io::stdout().flush().unwrap();
    }
}
