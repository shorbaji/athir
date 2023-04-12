use super::*;

#[test]
fn test_parse_identifier() {
    assert!(Parser::new("hello").parse().is_ok());
}

#[test]
fn test_parse_literals() {
    assert!(Parser::new("123").parse().is_ok());
    assert!(Parser::new("\"hello\"").parse().is_ok());
    assert!(Parser::new("#t").parse().is_ok());
    assert!(Parser::new("#f").parse().is_ok());
    assert!(Parser::new("#\\a").parse().is_ok());
}

#[test]
fn test_procedure_call() {
    assert!(Parser::new("(+ 1 2)").parse().is_ok());
    assert!(Parser::new("(+ 1 (list 2 3))").parse().is_ok());
}

#[test]
fn test_lambda() {
    assert!(Parser::new("(lambda (x) x)").parse().is_ok());
    assert!(Parser::new("(lambda (x) (list x))").parse().is_ok());
    assert!(Parser::new("(lambda (x) (list (+ x 1)))").parse().is_ok());
    assert!(Parser::new("(lambda x x)").parse().is_ok());
    assert!(Parser::new("(lambda (x) (define y 1))").parse().is_ok());
}

#[test]
fn test_if() {
    assert!(Parser::new("(if #t 1 2)").parse().is_ok());
    assert!(Parser::new("(if #t 1)").parse().is_ok());
    assert!(Parser::new("(if #t 1 2 3)").parse().is_err());
    assert!(Parser::new("(").parse().is_err());
    assert!(Parser::new("(lambda (x) x").parse().is_err());
}

#[test]
fn test_assignment() {
    assert!(Parser::new("(set! x 1)").parse().is_ok());
    assert!(Parser::new("(set! x (+ 1 2))").parse().is_ok());
    assert!(Parser::new("(set! 1 1)").parse().is_err());
    assert!(Parser::new("(set! x)").parse().is_err());
    assert!(Parser::new("(set! x 1 2)").parse().is_err());
}

#[test]
fn test_variable_definition() {
    assert!(Parser::new("(define x 1)").parse().is_ok());
    assert!(Parser::new("(define x (+ 1 2))").parse().is_ok());
    assert!(Parser::new("(define x)").parse().is_err());
    assert!(Parser::new("(define f (lambda (x) x))").parse().is_ok());
}

#[test]
fn test_function_definition() {
    assert!(Parser::new("(define (f x) x)").parse().is_ok());
    assert!(Parser::new("(define (f) x)").parse().is_ok());
    assert!(Parser::new("(define (f x) (set! x 1) x)").parse().is_ok());
    assert!(Parser::new("(define (f x))").parse().is_err());
    assert!(Parser::new("(define (f x y) x)").parse().is_ok());
    // assert!(Parser::new("(define (f x . y) x)").parse().is_ok());
    // assert!(Parser::new("(define (f x . y z) x)").parse().is_err());
}

#[test]
fn test_abbreviated_quotation() {
    assert!(Parser::new("'x").parse().is_ok());
}

#[test]
fn test_quotation() {
    assert!(Parser::new("(quote x)").parse().is_ok());
}

