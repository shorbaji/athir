    //! Read eval print loop
    //!
    //! The REPL is a Read-Eval-Print-Loop that allows the user to interactively enter Lisp expressions and see the results of their evaluation.
    //! Currently, the REPL runs the lexer and prints the tokens it finds since the parser and evaluator are not yet implemented.
    //! 
    use std::io::Write;
    use crate::parser::Parser;
    
    pub fn repl() {
        print!("> ");
        std::io::stdout().flush().unwrap();
        loop {
            for line in std::io::stdin().lines() {
                let line = line.unwrap();

                for token in Parser::new(&line).parse() {
                    println!("token: {:?}", token);
                }
                print!("> ");
                std::io::stdout().flush().unwrap();
            }
        }
    }
