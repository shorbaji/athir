//! **athir** is a Cloud Lisp engine
//!
//! Language features are:
//! - native multi-tenancy with first-class identity (and access management)
//! - a universal, supra-global, persistent scope
//! - a universal distributed VM
//!
//! Modules are:
//! - a lexical analyzer
//! - a repl
//! - tools including a lexeme generator and a regex generator

mod error;
mod eval;
mod object;
mod read;
mod repl;
mod result;

use repl::repl;

/// Main entry point
///
/// At the moment, the main function simply prints a welcome message and calls the REPL.
/// This will evolve into launching a server and listening for connections or setting up a remote REPL to a server
fn main() -> std::io::Result<()> {
    println!("athir (c) 2023 Athir LLC");
    repl();
    Ok(())
}
