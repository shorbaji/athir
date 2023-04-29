/// TODOs
///
/// [P0]
/// - error recovery with rdepth
/// - call/cc
/// - tail recursion
/// 
/// [P1]
/// - better errors
/// - number
/// - standard library base
/// - standard library write
/// - standard library char
/// - standard library string
/// - standard library cxr
/// - port from/to file
/// - result as an Object

mod object;

mod read;
mod eval;
mod print;

mod repl;

fn main() -> Result<crate::object::Object, crate::object::Object> {
    repl::repl()
}