// TODO 
// - [x] Lexer for number-specific tokens
// - [x] Parser for numbers
// - [x] Apply parser to existin lexer's to_number() function
// - [x] link value::Number to pub enum V
// - [x] Uncomment lexer test_number()
// - [ ] real @ uinteger dot ...
// - [ ] More tests for number parsing
// - [ ] Guard for out of bound integers and floats in number parser with errors
// - [ ] Uncomment eval tests
// - [ ] expose number built-ins to global env
// - [ ] add more number built-ins

mod alloc;
mod env; 
mod eval;
mod read;
mod repl;
mod stdlib;
mod value;

use repl::repl;

fn main() {
    println!("Athir Scheme {} (c) 2023 Athir LLC", env!("CARGO_PKG_VERSION"));
    repl();
}
