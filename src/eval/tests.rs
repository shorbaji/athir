use super::*;
use crate::alloc::A;
use crate::env::global_env;
use crate::stdlib::base::read;
use crate::value::V;

fn test(code: &str, predicate: fn(&V) -> bool) {
    let mut e: R = A::null();
    let env = global_env();
    let port = A::port_string(code.to_string());

    loop {
        let expr = read(Some(&port));
        if let V::EofObject = expr.deref().borrow().deref() {
            break;
        }
        e = trampoline(&A::continuation(eval, &env, &A::continuation_null()), &expr);
    }

    println!("{:?}", e.deref().borrow().deref());
    assert!(predicate(e.deref().borrow().deref()));
}

#[test]
fn test_eval_boolean() {
    test("#t", |e| matches!(e, V::Boolean(b) if b == &true));
}

#[test]
fn test_eval_bytevector() {
    test("#u8(1 2 3)", |e| matches!(e, V::Bytevector(_)));
}

#[test]
fn test_eval_character() {
    test("#\\a", |e| matches!(e, V::Char(c) if c == &'a'));
}

#[test]
fn test_eval_null() {
    test("'()", |e| matches!(e, V::Null));
}

#[test]
fn test_eval_number() {
    test(
        "42",
        |e| matches!(e, V::Number(n) if n == &"42".to_string()),
    );
}

#[test]
fn test_eval_pair() {
    test("(cons 1 2)", |e| matches!(e, V::Pair(_, _)));
}

#[test]
fn test_eval_port() {
    test("(open-input-string \"hello\")", |e| matches!(e, V::Port(_)));
}

#[test]
fn test_eval_procedure() {
    test("(lambda (x) x)", |e| matches!(e, V::Procedure(_)));
}

#[test]
fn test_eval_string() {
    test(
        "\"hello\"",
        |e| matches!(e, V::String(s) if s == &"hello".to_string()),
    );
}

#[test]
fn test_eval_vector() {
    test("#(1 2 3)", |e| matches!(e, V::Vector(_)));
}

#[test]
fn test_eval_symbol_not_found() {
    test("x", |e| matches!(e, V::Error(_)));
}

#[test]
fn test_eval_symbol_found() {
    test("identity", |e| matches!(e, V::Procedure(_)));
}

#[test]
fn test_quote_short() {
    test("'x", |e| matches!(e, V::Symbol(s) if s == &"x".to_string()));
}

#[test]
fn test_quote_long() {
    test(
        "(quote x)",
        |e| matches!(e, V::Symbol(s) if s == &"x".to_string()),
    );
}

#[test]
fn test_procedure_call_built_in() {
    test(
        "(identity 1)",
        |e| matches!(e, V::Number(n) if n == &"1".to_string()),
    );
}

#[test]
fn test_procedure_call_lambda() {
    test(
        "((lambda (x) x) 1)",
        |e| matches!(e, V::Number(n) if n == &"1".to_string()),
    );
}

#[test]
fn test_procedure_call_lambda_nested() {
    test(
        "((lambda (x) ((lambda (x) x) x)) 1)",
        |e| matches!(e, V::Number(n) if n == &"1".to_string()),
    );
}

#[test]
fn test_lambda() {
    test("(lambda (x) x)", |e| matches!(e, V::Procedure(_)));
}

#[test]
fn test_if() {
    test(
        "(if #t 1 2)",
        |e| matches!(e, V::Number(n) if n == &"1".to_string()),
    );
}

#[test]
fn test_if_false() {
    test(
        "(if #f 1 2)",
        |e| matches!(e, V::Number(n) if n == &"2".to_string()),
    );
}

#[test]
fn test_if_compound_test() {
    test(
        "(if (identity #t) 1 2)",
        |e| matches!(e, V::Number(n) if n == &"1".to_string()),
    );
}

#[test]
fn test_if_test_not_boolean() {
    test(
        "(if 1 1 2)",
        |e| matches!(e, V::Number(n) if n == &"1".to_string()),
    );
}

#[test]
fn test_define() {
    test(
        "(define x 1) x",
        |e| matches!(e, V::Number(n) if n == &"1".to_string()),
    );
}

#[test]
fn test_define_lambda() {
    test(
        "(define x (lambda (x) x)) (x 1)",
        |e| matches!(e, V::Number(n) if n == &"1".to_string()),
    );
}

#[test]
fn test_set_after_define() {
    test(
        "(define x 1) (set! x 2) x",
        |e| matches!(e, V::Number(n) if n == &"2".to_string()),
    );
}

#[test]
fn test_set_without_define_error() {
    test("(set! x 2)", |e| matches!(e, V::Error(_)));
}
