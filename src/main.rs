/// TODOs
///
/// P0
/// - port & reader support for file
/// - port & reader support for string
/// - port & reader support for stdin
/// - standard library read 
/// 
/// - ctrl-d exists as eof
/// - eval testing
/// - main should return a result
/// - error recovery with rdepth
/// - better error messages
/// - printing of env 
/// - printing of lists 
/// - use &Object instead of Object
/// - number
/// - standard library write
/// - standard library eval/environment
/// - standard library cxr
/// - standard library base
/// - standard library char

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