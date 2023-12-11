//! # Athir
//! 
//! A Lisp interpreter and virtual machine runtime written in Rust.
//! 

#[doc(hidden)]
mod alloc;

#[doc(hidden)]
mod env;

#[doc(hidden)]
mod eval;

#[doc(hidden)]
mod read;
mod repl;

#[doc(hidden)]
mod stdlib;

#[doc(hidden)]
mod value;

use repl::repl;

#[doc(hidden)]
fn main() {
    println!(
        "Athir Scheme {} (c) 2023 Athir LLC",
        env!("CARGO_PKG_VERSION")
    );
    repl();
}
