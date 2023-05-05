use std::io::Write;
use std::ops::Deref;

use crate::alloc::{A, R};
use crate::env::global_env;
use crate::eval::{eval, start};
use crate::stdlib::base::{car, cons, read};
use crate::value::{V};

pub fn repl() {
    let env = global_env();

    let ports = cons(&A::port_stdin(), &A::port_stdout());

    let k = A::continuation_null();
    let k = A::continuation_plus(prompt_read_cont, &ports, &env, &k);

    start(&k, &A::null());
}

fn prompt_read_cont(e: &R, ports: &R, r: &R, k: &R) -> (R, R) {
    if let V::String(s) = e.deref().borrow().deref() {
        print!("{} ", s);
        std::io::stdout().flush().unwrap();
    }

    let k = A::continuation_plus(eval_cont, ports, r, k);


    print!("> ");
    std::io::stdout().flush().unwrap();

    let stdin_port = car(ports);


    (k, read(Some(&stdin_port)))

}
fn eval_cont(e: &R, ports: &R, r: &R, k: &R) -> (R, R) {
    match e.deref().borrow().deref() {
        V::EofObject => (k.clone(), A::null()),
        _ => {
            let k = A::continuation_plus(print_cont, ports, r, k);
            (A::continuation(eval, r, &k), e.clone())
        }
    }
}

fn print_cont(e: &R, ports: &R, r: &R, k: &R) -> (R, R) {
    println!("=> {:?}", e);
    (A::continuation_plus(prompt_read_cont, ports, r, k), A::null())
}
