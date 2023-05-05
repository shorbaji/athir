
mod alloc;
mod env; 
mod eval;
mod read;
mod repl;
mod stdlib;
mod value;

use repl::repl;

fn main() {
    println!("Athir Scheme v0.0.3 (c) 2023 Athir LLC");
    repl();
}
