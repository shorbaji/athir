//! Read eval print loop
//!
//! The REPL is a Read-Eval-Print-Loop that allows the user to interactively enter Lisp expressions and see the results of their evaluation.
//! Currently, the REPL runs the lexer and prints the tokens it finds since the parser and evaluator are not yet implemented.
//! 

use std::io::Write;

pub fn repl() {
    use crate::gc::NoGC;
    use crate::eval::Eval;
    use crate::read::Reader;


    let mut vm: Eval<NoGC> = Eval::new();
    let reader = Reader::new(std::io::stdin().lines());
    
    print!("> ");
    std::io::stdout().flush().unwrap();

    for expr in reader {
        match expr {
            Ok(expr) => println!("{:?}", vm.eval_in_global_env(&expr)),
            Err(err) => println!("{}", err),
        }
        print!("> ");
        std::io::stdout().flush().unwrap();
    }
}
