/// TODOs
///
/// P0
/// - ctrl-d exists as eof
/// - error recovery with rdepth
/// - port & reader support for stdin
/// - port & reader support for file
/// - port & reader support for string
/// - standard library read 
/// - standard library write
/// - standard library char
/// - eval testing
/// - main should return a result
/// - better error messages
/// - printing of env 
/// - printing of lists 
/// - use &Object instead of Object
/// - number
/// - standard library eval/environment
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