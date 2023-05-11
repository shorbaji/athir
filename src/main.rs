// TODO 
// - [ ] Lexer for number-specific tokens
// - [ ] Parser for numbers
// - [ ] Apply parser to existin lexer's to_number() function
// - [ ] Uncomment eval tests
// - [ ] Uncomment lexer test_number()
// - [x] link value::Number to pub enum V
// - [ ] Tests

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
