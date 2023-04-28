/// TODOs
///
/// [P0]
/// - error recovery with rdepth
/// - eval testing
/// - port & reader support for stdin
/// - port & reader support for string
/// 
/// [P1]
/// - port & reader support for file
/// - print 
/// - standard library read 
/// - standard library write
/// - standard library char
/// - main should return a result
/// - better error messages
/// - use &Object instead of Object
/// - number
/// - standard library cxr
/// - standard library base

mod eval;
mod object;
mod print;
mod read;
mod repl;
mod stdlib;

fn main() -> Result<(), ()> {
    println!("athir (c) 2023 Athir LLC");

    match repl::repl() {
        Ok(_) => Ok(()),
        Err(_) => Ok(()),
    }
}