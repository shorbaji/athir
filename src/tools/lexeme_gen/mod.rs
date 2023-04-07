
mod identifier;
mod number;
mod randomizable;

use std::env;
use identifier::random as random_identifier;
use number::random as random_number;

fn main() {
    let args: Vec<String> = env::args().collect();


    if args.len() != 3 {
        println!("Usage: {} <number of lexemes> <type>", args[0]);
        return;
    } else {
        let n = &args[1].parse::<usize>()
            .unwrap_or_else(|_| {
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
