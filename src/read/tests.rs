use super::*;
use std::iter::Peekable;
use crate::read::lexer::{Token, Lexer};
use crate::read::Reader;

struct TestReader {
    string: String,
    used: bool,
    tokens: Peekable<std::vec::IntoIter<Token>>,
}

impl TestReader {
    fn new(string: String) -> Self {
        Self {
            string,
            tokens: vec!().into_iter().peekable(),
            used: false,
        }
    }
}

impl Lexer for TestReader {
    fn read_line(&mut self) -> Option<String> {
        if self.used {
            None
        } else {
            self.used = true;
            Some(self.string.clone())
        }
    }

    fn get_tokens(&mut self) -> &mut Peekable<std::vec::IntoIter<Token>> {
        &mut self.tokens
    }

    fn set_tokens(&mut self, tokens: Peekable<std::vec::IntoIter<Token>>) {
        self.tokens = tokens;
    }
}

impl Reader for TestReader {
    fn get_next_token(&mut self) -> Option<Token> {
        Lexer::get_next_token(self)
    }

    fn peek_next_token(&mut self) -> Option<Token> {
        match Lexer::peek_next_token(self) {
            Some(t) => Some(t),
            None => None,
        }
    }
}

fn test_parse(goods: &[&str], bads: &[&str]) {

    let parser = goods.iter().map(|s| {
        let mut reader = TestReader::new(s.to_string());
        reader.read()
    });

    for result in parser {
        println!("{:?}", result);
        assert!(!matches!(result.deref().borrow().deref(), V::Error(_)));
    }

    let parser = bads.iter().map(|s| {
        let mut reader = TestReader::new(s.to_string());
        reader.read()
    });

    for result in parser {
        println!("{:?}", result);
        assert!(matches!(result.deref().borrow().deref(), V::Error(_)));
    }
}

#[test]
fn test_parse_literals() {
    let goods = ["123", "\"hello\"", "#t", "#f", "#\\a"];
    let bads = [];
    test_parse(&goods, &bads);
}

#[test]
fn test_parse_identifier() {
    let goods = ["hello"];
    let bads = [","];

    test_parse(&goods, &bads);
}

#[test]
fn test_vector() {
    let goods = [
        "#()",
        "#(1 2 3)",
        "#(0 10 5)",
    ];
    let bads = [];
    test_parse(&goods, &bads);
}

#[test]
fn test_bytevector() {
    let goods = [
        "#u8()",
        "#u8(1 2 3)",
    ];

    let bads = [
    ];

    test_parse(&goods, &bads);
}


#[test]
fn test_procedure_call() {
    let goods = ["(+ 1 2)", "(+ 1 (list 2 3))"];
    let bads = [];
    test_parse(&goods, &bads);
}

#[test]
fn test_lambda() {
    let goods = [
        "(lambda (x) x)",
        "(lambda (x) (list x))",
        "(lambda (x) (list (+ x 1)))",
        "(lambda x x)",
        "(lambda (x) (define y 1))"
    ];
    let bads = [
        "(lambda (x) (define x 1) 2 (define y 3))"
    ];
    test_parse(&goods, &bads);
}

#[test]
fn test_if() {
    let goods = [
        "(if #t 1 2)",
        "(if #t 1)"
    ];
    let bads = [
        "(if #t 1 2 3)",
    ];
    test_parse(&goods, &bads);
}

#[test]
fn test_assignment() {
    let goods = [
        "(set! x 1)",
        "(set! x (+ 1 2))"
    ];
    let bads = [
        "(set! 1 2)",
        "(set! x)",
        "(set! x 1 2)"
        ];
    test_parse(&goods, &bads);
}

#[test]
fn test_variable_definition() {
    let goods = ["(define x 1)", "(define x (+ 1 2))", "(define f (lambda (x) x))"];
    let bads = ["(define x)"];
    test_parse(&goods, &bads);
}

#[test]
fn test_function_definition() {
    let goods = ["(define (f x) x)", "(define (f) x)", "(define (f x) (set! x 1) x)", "(define (f x y) x)", "(define (f x . y) x)"];
    let bads = [
        "(define (f x))",
        "(define (f x . y z) x)"
        ];
    test_parse(&goods, &bads);
}

#[test]
fn test_abbreviated_quotation() {
    let goods = ["'x", "'(1 2 3)", "'(x y . z)"];
    let bads = [];
    test_parse(&goods, &bads);
}

#[test]
fn test_quotation() {
    let goods = ["(quote x)", "(quote (1 2 3))", "(quote (x y . z))"];
    let bads = [];
    test_parse(&goods, &bads);
}


#[test]
fn test_macro_block() {
    let goods = [
        "(let-syntax ((foo (syntax-rules (foo bar) (a b)))) a b c d)",
        "(let-syntax ((f (syntax-rules () ((_ x) x))) (g (syntax-rules () ((_ x) (f x))))) (list (f 1) (g 1)))"
    ];
    let bads = [];
    test_parse(&goods, &bads);
}

#[test]
fn test_define_values() {
    let goods = [
        "(define-values (x y) (+ 1 2) 1 2 3)",
    ];
    let bads = [];
    test_parse(&goods, &bads);
}

#[test]
fn test_define_record_type() {
    let goods = [
        "(define-record-type <animal>
            (animal name age species owner-name)
            animal?
            (name animal-name set-animal-name!)
            (age animal-age set-animal-age!)
            (species animal-species)
            (owner-name animal-owner set-animal-owner!))",
    ];
    let bads = [];
    test_parse(&goods, &bads);
}

#[test]
fn test_include() {
    let goods = [
        "(include \"foo.scm\")",
    ];
    let bads = ["(include)", "(include 1)"];
    test_parse(&goods, &bads);
}

#[test]
fn test_include_ci() {
    let goods = [
        "(include-ci \"foo.scm\")",
    ];
    let bads = ["(include-ci)", "(include-ci 1)"];
    test_parse(&goods, &bads);

}

#[test]
fn test_quasiquotation() {
    let goods = [
        "`1",
        "`(1 2 3)",
        "`,1",
        "`(1 ,2 3)",
        "`(,(+ 1 2) 2)",
        "(quasiquote 1)",
        "(quasiquote (1 2 3))",
        "(quasiquote ,1)",
        "(quasiquote (1 ,2 3))",
        "(quasiquote (,(+ 1 2) 2))",
        "`,`,1",
        "``,,1",
        "`(unquote 1)",
        "(quasiquote (unquote (+ 1 2)))",
    ];

    let bads = [
        "`,,",
    ];

    test_parse(&goods, &bads);
}

#[test]
fn test_define_library() {
    let goods = [
        "(define-library (foo))",
        "(define-library (0123))",
        "(define-library (foo 1))",
        "(define-library (foo) (export bar))",
        "(define-library (foo) (include \"bar.scm\"))",
        "(define-library (foo) (include \"bar.scm\") (export bar))",
        "(define-library (foo) (export bar) (include \"bar.scm\"))",
        "(define-library (foo) (import (bar 1)))",
        "(define-library (foo) (import (only (bar n) x y z)))",
        "(define-library (foo) (import (except (bar n) x y z)))",
        "(define-library (foo) (import (prefix (bar n) x)))",
        "(define-library (foo)
            (import (rename (bar n) 
                            (x y)
                            (z a))))",
        "(define-library (foo)
            (begin (define x 1) (+ x 1)))",
        "(define-library (foo)
            (include-library-declarations \"bar.scm\" \"baz.scm\"))",
        "(define-library (foo)
            (cond-expand (bar (export baz))))",
        "(define-library (foo)
            (cond-expand (bar (export baz))
                    (else (import (qux 1)))))",
    ];

    let bads = [
        "(define-library foo)",
        "(define-library 1)",
        "(define-library (1.23))",
        "(define-library (foo) (include bar))",
        "(define-library (foo) (import (prefix (bar n) x y)))",
    ];

    test_parse(&goods, &bads);
}

#[test]
fn test_begin() {
    let goods = [
        "(begin 1 2 3)",
        "(begin 1)",
        "(begin (define x 1) (+ x 1))",
        "(begin (define x 1))",
    ];

    let bads = [
    ];

    test_parse(&goods, &bads);
}

#[test]
fn test_begin_def() {
    let goods = [
        "(begin (define x 1))",
        "(begin)",
    ];

    let bads = [
    ];

    test_parse(&goods, &bads);
}
