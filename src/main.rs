/// TODOs
///
/// [P0]
/// - eval testing
/// - call/cc
/// - tail recursion
/// 
/// [P1]
/// - error recovery with rdepth
/// - better errors
/// - number
/// - standard library base
/// - standard library write
/// - standard library char
/// - standard library string
/// - standard library cxr
/// - port & reader support for file

mod object;

mod read;
mod eval;
mod print;

mod repl;

fn main() -> Result<crate::object::Object, crate::object::Object> {
    repl::repl()
}