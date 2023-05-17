//! # Eval
//! 
//! The eval module implements the eval function which evaluates an expression in an environment
//! This is implemented with a trampoline (driver loop) which takes a continuation and an expression
//! and calls the continuation with the expression
//! The continuation returns a new continuation and a new expression to the trampoline
//! 

#[cfg(test)]
mod tests;

use crate::alloc::{A, R};
use crate::env::lookup;
use crate::stdlib::base::{car, cdr, cons, cadr};
use crate::syntax::expand;
use crate::value::{V, procedure::Procedure};

use std::collections::HashMap;
use std::ops::{Deref, DerefMut};

/// The trampline (driver loop) takes a continuation and an expression and calls the continuation with the expressions
///
/// There are three types of continuations:
/// 1. Continuation - a continuation that only captures the environment (r) and continuation (k) in which it is created (Procedure::Continuation)
///     this is accompanied with a ternary function f(e, r, k)
/// 2. ContinuationPlus - a continuation that also captures an additional object (o) 
///    this is accompanied with a quaternary function f(e, o, r, k)
/// 3. ContinuationNull - the null continuation which terminates the driver loop
/// 

pub fn trampoline(k: &R, e: &R) -> R{
    let mut k = k.clone();
    let mut e = e.clone();

    loop {
        (k, e) = match k.deref().borrow().deref() {
            V::Procedure(Procedure::Continuation { f, r, k }) => f(&e, r, k),
            V::Procedure(Procedure::ContinuationPlus { f, o, r, k }) => f(&e, o, r, k),
            V::Procedure(Procedure::ContinuationNull) => { return e },
            _ => panic!("not a continuation {:?}", k)
        }    
    }
}

/// The eval_program function evaluates a program which is a list of expressions
pub fn _eval_program(_e: &R, program: &R, r: &R, k: &R) -> (R, R) {
    match program.deref().borrow().deref() {
        V::Null => (k.clone(), A::null()),
        V::Pair(car, cdr) => {
            let then = A::continuation_plus(_eval_program, cdr, &r, &k);
            (A::continuation(eval, r, &then), car.clone())
        },
        _ => (k.clone(), A::runtime_error(format!("expected a program as a list of exprs {:?}", program)))
    }
}

/// The eval function evaluates an expression in an environment
pub fn eval(e: &R, r: &R, k: &R) -> (R, R) {
    match e.deref().borrow().deref() {
        V::Boolean(_) 
        | V::Bytevector(_)
        | V::Char(_)
        | V::Number(_) 
        | V::String(_) 
        | V::Vector(_)
        | V::Null => (k.clone(), e.clone()),

        V::Symbol(s) => (k.clone(), lookup(&s, r)),
        V::Pair(car, cdr) => match car.deref().borrow().deref() {
                V::Symbol(s) => match s.as_str() {
                    "define" => (A::continuation(eval_define, r, k), cdr.clone()),
                    "if" => (A::continuation(eval_if, r, k), cdr.clone()),
                    "lambda" => (A::continuation(eval_lambda, r, k), cdr.clone()),
                    "quote" => (A::continuation(eval_quote, r, k), cdr.clone()),
                    "set!" => (A::continuation(eval_set, r, k), cdr.clone()),
                    "define-syntax" => (A::continuation(eval_define_syntax, r, k), cdr.clone()),
                    // TODO: implement the rest of the special forms
                    _ => if let V::Transformer(t) = lookup(s, r).deref().borrow().deref() {
                            println!("macro use: {}", e);
                            println!("expanded to: {}", expand(&t, &e).unwrap());
                            (A::continuation(eval, r, k), expand(&t, &e).unwrap())
                        } else {
                            (A::continuation_plus(eval_application, cdr, r, k), car.clone())
                        },
                },
                _ => (A::continuation_plus(eval_application, cdr, r, k), car.clone()),
        },
        _ => (k.clone(), A::runtime_error(format!("eval error: malformed expr {:?}", e)))
    }
}

/// Evaluate a define expression
fn eval_define(e: &R, r: &R, k: &R) -> (R, R) {
    let var = car(e);
    let val = cadr(e);

    let then = A::continuation_plus(eval_define_or_set_cont, &var, r, k);
    (A::continuation(eval, r, &then), val)
}

fn eval_define_syntax(e: &R, r: &R, k: &R) -> (R, R) {
    let var = car(e);
    let val = cadr(e);

    let then = A::continuation_plus(eval_define_or_set_cont, &var, r, k);
    (A::continuation(eval_syntax_rules, r, &then), val)
}

fn eval_syntax_rules(e: &R, _r: &R, k: &R) -> (R, R) {
     (k.clone(), A::transformer(e))
}

/// Evaluate a set expression
fn eval_set(e: &R, r: &R, k: &R) -> (R, R) {
    let var = car(e);
    let val = cadr(e);

    if let V::Symbol(s) = var.deref().borrow().deref() {
        // It is an error if var is not bound either in some region
        // enclosing the set! expression or else globally. 
        if let V::Error(_) = lookup(&s, r).deref().borrow().deref() {
            return (k.clone(), A::runtime_error(format!("unbound variable {:?}", s)))
        }
    } else {
        return (k.clone(), A::runtime_error(format!("not a symbol {:?}", var)))
    };

    let then = A::continuation_plus(eval_define_or_set_cont, &var, r, k);
    (A::continuation(eval, r, &then), val)        

}

fn eval_define_or_set_cont(val: &R, var: &R, r: &R, k: &R) -> (R, R) {
    // Expression val is evaluated, and the resulting
    // value is stored in the location to which var is bound.

    if let V::Symbol(s) = var.deref().borrow().deref() {
        if let V::Env{map, outer: _} = r.deref().borrow_mut().deref_mut() {
            map.insert(s.clone(), val.clone());
            (k.clone(), A::unspecified()) // The result of the define or set! expression is unspecified.
        } else {
            (k.clone(), A::runtime_error("not an environment".to_string()))
        }
    } else {
        (k.clone(), A::runtime_error(format!("not a symbol {:?}", var)))
    }
}

/// Evaluate an if expression
fn eval_if(e: &R, r: &R, k: &R) -> (R, R) {
    let test = car(e);
    let rest = cdr(e);
    let then = A::continuation_plus(eval_if_cont, &rest, r, k);

    (A::continuation(eval, r, &then), test)
}

fn eval_if_cont(test: &R, rest: &R, r: &R, k: &R) -> (R, R) {
    match test.deref().borrow().deref() {
        V::Boolean(false) => eval(&cadr(rest), r, k),
        _ => eval(&car(rest), r, k),
    }
}

/// Evaluate a lambda expression
fn eval_lambda(e: &R, r: &R, k: &R) -> (R, R) {
    let formals = car(e);
    let body = cdr(e);
    let env = r.clone();

    (k.clone(), A::closure(&formals, &body, &env))
}

/// Evaluate a quote expression
fn eval_quote(e: &R, _r: &R, k: &R) -> (R, R) {
    (k.clone(), car(e))
}

/// Evaluate an application expression
fn eval_application(operator: &R, operands: &R, r: &R, k: &R) -> (R, R) {
    let then = A::continuation_plus(eval_operator_cont, operands, r, k);

    // note: we evaluate the operator before the operands
    (A::continuation(eval, r, &then), operator.clone())
}

/// Evaluate the operands after the operator
fn eval_operator_cont(e: &R, o: &R, r: &R, k: &R) -> (R, R) {
    let then = A::continuation_plus(eval_operands_cont, e, r, k);
    (A::continuation(evlis, r, &then), o.clone())
}

/// Apply the evaluated operator to the evaluated operands
fn eval_operands_cont(operands: &R, operator: &R, r: &R, k: &R) -> (R, R) {
    apply(operator, operands, r, k)
}

/// Evaluate a list of expressions
/// pub for use by the list primitive
pub fn evlis(ls: &R, r: &R, k: &R) -> (R, R) {
    match ls.deref().borrow().deref() {
        V::Null => (k.clone(), A::null()),
        V::Pair(car, cdr) => {
            let then = A::continuation_plus(eval_car_cont, cdr, r, k);
            (A::continuation(eval, r, &then), car.clone())
        },
        _ => (k.clone(), A::runtime_error(format!("not a list {:?}", ls)))
    }
}

fn eval_car_cont(car: &R, ls: &R, r: &R, k: &R) -> (R, R) {
    let then = A::continuation_plus(eval_cdr_cont, car, r, k);
    (A::continuation(evlis, r, &then) , ls.clone())
}

fn eval_cdr_cont(cdr: &R, car: &R, _r: &R, k: &R) -> (R, R) {
    (k.clone(), cons(car, cdr))
}

#[doc(hidden)]
/// Apply a procedure to a list of arguments
/// pub for call/cc which applies a procedure to the current continuation
pub fn apply(operator: &R, operands: &R, r: &R, k: &R) -> (R, R) {
    match operator.deref().borrow().deref() {
        V::Procedure(Procedure::PrimitiveUnary(f,_ )) => (k.clone(), f(&car(operands))),
        V::Procedure(Procedure::PrimitiveBinary(f, _)) => (k.clone(), f(&car(operands), &cadr(operands))),
        V::Procedure(Procedure::PrimitiveVariadic(f, _)) => (k.clone(), f(operands)),
        V::Procedure(Procedure::Closure { formals, body, env }) => apply_closure(formals, body, env, operands, r, k),
        V::Procedure(Procedure::PrimitiveOptionalUnary(f, _)) => match operands.deref().borrow().deref() {
                V::Null => (k.clone(), f(None)),
                V::Pair(car, _) => (k.clone(), f(Some(car))),
                _ => (k.clone(), A::runtime_error(format!("not a list {:?}", operands)))
        },
        V::Procedure(Procedure::PrimitiveERK(f, _)) => f(operands, r, k),
        V::Procedure(Procedure::Continuation { f:_ , r:_, k:_ }) => (operator.clone(), car(operands)),
        V::Procedure(Procedure::ContinuationPlus { f:_, o:_, r:_, k: _ }) => (operator.clone(), car(operands)),
        _  => (k.clone(), A::runtime_error(format!("not a procedure {:?}", operator))),
    }
}

fn apply_closure(formals: &R, body: &R, env: &R, args: &R, _r: &R, k: &R) -> (R, R) {
    // create a new environment
    let mut map = HashMap::new();

    // bind the arguments to the formals
    let mut formals = formals.clone();
    let mut args = args.clone();

    loop {
        match formals.clone().deref().borrow().deref() {
            V::Null => break,
            V::Pair(fcar, fcdr) => {
                match fcar.deref().borrow().deref() {
                    V::Symbol(s) => map.insert(s.clone(), car(&args)),
                    _ => panic!("not a symbol")
                };
                formals = fcdr.clone();
                args = cdr(&args);
            },
            _ => panic!("not a list")
        }
    }

    // return (k.clone(), V::error("not implemented".to_string()));

    eval_body(body, &A::env(map, Some(env.clone())), k)  
}

fn eval_body(body: &R, env: &R, k: &R) -> (R, R) {
    let then = A::continuation_plus(eval_body_after_car, &cdr(body), env, k);
    (A::continuation(eval, env, &then), car(body))
}

fn eval_body_after_car(e: &R, o: &R, r: &R, k: &R) -> (R, R) {
    match o.deref().borrow().deref() {
        V::Null => (k.clone(), e.clone()),
        V::Pair(car, cdr) => {
            let then = A::continuation_plus(eval_body_after_car, cdr, r, k);
            (A::continuation(eval, r, &then), car.clone())
        },
        _ => (k.clone(), A::runtime_error(format!("body is not a list {:?}", o)))
    }
}
