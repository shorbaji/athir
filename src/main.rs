/// TODOs
///
/// [P0]
/// - port & reader support for stdin
/// - port & reader support for string
/// - port & reader support for file
/// - eval testing
/// - error recovery with rdepth
/// 
/// [P1]
/// - print 
/// - standard library read 
/// - standard library write
/// - standard library char
/// - main should return a result
/// - better error messages
/// - number
/// - standard library cxr
/// - standard library base

mod eval;
mod object;
mod print;
mod read;
mod repl;

fn main() -> Result<crate::object::Object, crate::object::Object> {
    println!("athir (c) 2023 Athir LLC");

    repl::repl()
}