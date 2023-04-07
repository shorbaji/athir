mod lexer;
mod tools;

use lexer::*;
use logos::Logos;
use std::io::Write;

fn repl() {
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

fn main() -> std::io::Result<()>{
    println!("athir (c) 2023 Omar Shorbaji");
    print!("> ");
    std::io::stdout().flush().unwrap();
    repl();
    Ok(())
}
