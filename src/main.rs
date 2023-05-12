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
