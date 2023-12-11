//! Lexeme generator tool
//! 
//! This tool generates random lexemes for testing purposes
//! Usage: lexeme_gen <number of lexemes> <type>
//! e.g. lexeme_gen 100 number
//! 
//! Types:
//! - number
//! - identifier
//!
mod identifier;
mod number;
mod randomizable;

use identifier::random as random_identifier;
use number::random as random_number;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 3 {
        println!("Usage: {} <number of lexemes> <type>", args[0]);
    } else {
        let n = &args[1].parse::<usize>().unwrap_or_else(|_| {
            panic!("Invalid number of lexemes. Must be an integer (usize)");
        });

        let f = match String::from(&args[2]).as_str() {
            "number" => random_number,
            "identifier" => random_identifier,
            _ => panic!("Invalid type. Must be either number or identifier"),
        };

        for _ in 0..*n {
            println!("{}", f());
        }
    }
}
