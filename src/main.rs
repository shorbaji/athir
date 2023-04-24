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

#[macro_use]
extern crate lazy_static;



mod error;
mod eval;
mod object;
mod read;
mod repl;

use repl::repl;


use object::Object;
use error::Error;

#[doc(hidden)]
pub type AthirResult = Result<Box<Object>, Error>;

#[doc(hidden)]
pub type AthirVecResult = Result<Vec<Box<Object>>, Error>;

/// Main entry point
///
/// At the moment, the main function simply prints a welcome message and calls the REPL.
/// This will evolve into launching a server and listening for connections or setting up a remote REPL to a server
fn main() -> std::io::Result<()> {
    println!("athir (c) 2023 Athir LLC");
    repl();
    Ok(())
}
