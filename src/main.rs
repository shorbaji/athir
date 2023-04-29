/// TODOs
///
/// [P0]
/// - eval testing
/// - port & reader support for file
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