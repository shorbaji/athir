use std::io::Write;
use std::ops::{Deref, DerefMut};

use crate::alloc::{A, R}; // A is the allocator, R is the reference type
use crate::env::global_env; // global_env() returns the global environment
use crate::eval::{eval, trampoline}; // eval() evaluates an expression, trampoline() is the interpreter driver loop
use crate::read::ExprReader; // Reader is the trait for reading expressions
use crate::stdlib::base::{car, cons}; // required base functions
use crate::value::{V};


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

    // Allocate a pair of ports for/with stdin and stdout
    let ports = cons(
        &A::port_stdin(), 
        &A::port_stdout() 
    );

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

    let expr = if let V::Port(p) = stdin_port.deref().borrow_mut().deref_mut() {
        p.read()
    } else {
        panic!("stdin is not a port");
    };

    // Read the input from stdin and return the eval continuation and the expression read
    (k, expr)
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
    println!("=> {}", e.deref().borrow().deref());
    (A::continuation(read_cont, r, k), ports.clone())
}
