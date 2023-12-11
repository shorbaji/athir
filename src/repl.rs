use std::io::Write;
use std::ops::Deref;

use crate::alloc::{A, R};
use crate::env::global_env;
use crate::eval::{eval, trampoline};
use crate::stdlib::base::{car, cons, read};
use crate::value::V;

///
/// repl()
///
/// Implements the read eval print loop by starting the interpreter driver loop (trampline) with
/// the initial continuation to prompt the user for input and then read
///

pub fn repl() {
    let env = global_env();

    // Create the initial read continuation which
    let k = A::continuation(read_cont, &env, &A::continuation_null());

    // Create a pair of ports for stdin and stdout
    let ports = cons(&A::port_stdin(), &A::port_stdout());

    // start the interpreter driver loop (trampoline) with the read continuation and the pair of ports
    trampoline(&k, &ports);
}

/// The read continuation function prompts the user for input and then reads the input
/// It returns an eval continuation and the expression read

fn read_cont(e: &R, r: &R, k: &R) -> (R, R) {
    // Create an eval continuation
    let k = A::continuation_plus(eval_cont, e, r, k);

    // Prompt the user for input
    print!("> ");
    std::io::stdout().flush().unwrap();

    // e carries a pair of ports, stdin and stdout
    // stdin is the first port in the pair
    let stdin_port = car(e);

    // Read the input from stdin and return the eval continuation and the expression read
    (k, read(Some(&stdin_port)))
}

/// The eval continuation function evaluates the expression read by the read continuation
/// It returns a print continuation and the expression evaluated
fn eval_cont(e: &R, ports: &R, r: &R, k: &R) -> (R, R) {
    // check if the expression is an EOF object
    // if it is, return the current continuation with the eof object
    // otherwise, return an eval_continuation with the print continuation
    match e.deref().borrow().deref() {
        V::EofObject => (k.clone(), e.clone()),
        _ => {
            let k = A::continuation_plus(print_cont, ports, r, k);
            (A::continuation(eval, r, &k), e.clone())
        }
    }
}

/// The print continuation function prints the expression evaluated by the eval continuation
/// It returns a read continuation and a null expression therefore completing the read eval print loop
fn print_cont(e: &R, ports: &R, r: &R, k: &R) -> (R, R) {
    println!("=> {e:?}");
    (A::continuation(read_cont, r, k), ports.clone())
}
