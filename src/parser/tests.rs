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
    assert!(Parser::new("'(1 2 3)").parse().is_ok());
    assert!(Parser::new("'(x y . z)").parse().is_ok());
}

#[test]
fn test_quotation() {
    assert!(Parser::new("(quote x)").parse().is_ok());
}

#[test]
fn test_template() {

    for s in [
        "foo",
        "( foo bar )",
        "( foo . bar )",
        "( foo bar . baz )",
        "#()",
        "#( foo )",
        "1.2e3",
        "#\\a",
        "\"hello\"",
        "( foo (bar (baz)))", 
        "( foo ... (bar ... (baz ...)))",
    ] {
        let mut parser = Parser::new(s);
        assert!(parser.template().is_ok())
    }

    for s in [
        "...",
        "( . foo )",
        "#( . foo )",
        "#( foo . bar )"
    ] {
        let mut parser = Parser::new(s);
        assert!(parser.template().is_err())
    }

}

#[test]
fn test_pattern() {

    for s in [
        "foo",
        "_",
        "()",
        "( foo bar )",
        "( foo . bar )",
        "( foo bar ... baz )",
        "( foo bar ... baz . lorem)",

    ] {
        let mut parser = Parser::new(s);
        assert!(parser.pattern().is_ok())
    };

    for s in [
        "...",
        // "( . foo )",
        // "#( . foo )",
        // "#( foo . bar )"
    ] {
        let mut parser = Parser::new(s);
        assert!(parser.pattern().is_err())
    }
}

#[test]
fn test_syntax_rule() {
    for s in [
        "(foo bar)",
        "((a b c .d ) foo)",
    ] {
        let mut parser = Parser::new(s);
        assert!(parser.syntax_rule().is_ok())
    }
}

#[test]
fn test_transformer_spec() {
    for s in [
        "(syntax-rules (foo bar) (a b))",
    ] {
        let mut parser = Parser::new(s);
        assert!(parser.transformer_spec().is_ok())
    }
}

#[test]
fn test_macro_block() {
    for s in [
        "let-syntax ((foo (syntax-rules (foo bar) (a b)))) a b c d)",
        "let-syntax ((f (syntax-rules () ((_ x) x))) (g (syntax-rules () ((_ x) (f x))))) (list (f 1) (g 1)))"
    ] {
        let mut parser = Parser::new(s);
        assert!(parser.macro_block().is_ok())
    }
}

#[test]
fn test_macro_block_suffix() {
    for s in [
        "((foo (syntax-rules (foo bar) (a b)))) a b c d)"
    ] {
        let mut parser = Parser::new(s);
        assert!(parser.macro_block_suffix().is_ok())
    }
}

#[test]
fn test_syntax_spec() {
    for s in [
        "(foo (syntax-rules (foo bar) (a b)))"
    ] {
        let mut parser = Parser::new(s);
        assert!(parser.syntax_spec().is_ok())
    }
}

#[test]
fn test_syntax_specs() {
    for s in [
        "(foo (syntax-rules (foo bar) (a b)))"
    ] {
        let mut parser = Parser::new(s);
        assert!(parser.syntax_specs().is_ok())
    }
}

#[test]
fn test_vector() {
    for s in [
        "#()",
        "#(1 2 3)",
    ] {
        assert!(Parser::new(s).parse().is_ok());
    }
}

#[test]
fn test_byte_vector() {
    for s in [
        "#u8()",
        "#u8(1 2 3)",
    ] {
        assert!(Parser::new(s).parse().is_ok());
    }
}
