/// TODOs
///
/// [P0]
/// - port & reader support for string
/// - port & reader support for file
/// - eval testing
/// - error recovery with rdepth
/// 
/// [P1]
/// - standard library write
/// - standard library char
/// - better error messages
/// - number
/// - standard library cxr
/// - standard library base

mod object;

mod read;
mod eval;
mod print;
mod repl;

fn main() -> Result<crate::object::Object, crate::object::Object> {
    repl::repl()
}