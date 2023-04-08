//! **athir** is a Cloud Lisp engine
//!
//! Language features are:
//! - native multi-tenancy with first-class identity (and access management)
//! - a universal, supra-global, persistent scope
//! - a universal distributed VM
//!
//! Modules are:
//! - a lexical analyzer (./lexer/mod.rs)
//! - tools including a lexeme generator (./tools/lexeme_gen/mod.rs) and a regex generator (./tools/regex_gen.rs)
// 

mod lexer;
mod repl;
mod tools;

fn main() -> std::io::Result<()> {
    //! Main entry point
    //!
    //! At the moment, the main function simply prints a welcome message and calls the REPL.
    //! This will evolve into launching a server and listening for connections or setting up a remote REPL to a server
    //
    use repl::repl;

    println!("athir (c) 2023 Athir LLC");
    repl();
    Ok(())
}
