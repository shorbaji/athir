use super::*;

fn test_parse(goods: &[&str], bads: &[&str], kind: NodeKind) {

    goods
    .iter()
    .map(|s| s.to_string())
    .for_each(|s| {
        match Parser::new([s.clone()].into_iter()).read() {
            Ok(node) => {
                println!("{} => {:?}", s, node);
                assert_eq!(node.kind(), &kind);
            },
            Err(err) => panic!("Error parsing {}: {}", s, err),
        }
    });

    bads
    .iter()
    .map(|s| s.to_string())
    .for_each(|s| assert!(Parser::new([s].into_iter()).read().is_err()));
}

#[test]
fn test_parse_identifier() {
    let goods = ["hello"];
    let bads = [","];

    test_parse(&goods, &bads, NodeKind::Identifier);
}

#[test]
fn test_parse_literals() {
    let goods = ["123", "\"hello\"", "#t", "#f", "#\\a"];
    let bads = ["#\\ab", "#\\"];
    test_parse(&goods, &bads, NodeKind::Literal);
}

#[test]
fn test_procedure_call() {
    let goods = ["(+ 1 2)", "(+ 1 (list 2 3))"];
    let bads = [];
    test_parse(&goods, &bads, NodeKind::ProcedureCall);
}

#[test]
fn test_lambda() {
    let goods = ["(lambda (x) x)", "(lambda (x) (list x))", "(lambda (x) (list (+ x 1)))", "(lambda x x)", "(lambda (x) (define y 1))"];
    let bads = ["(lambda (x) (define x 1) 2 (define y 3))"];
    test_parse(&goods, &bads, NodeKind::Lambda);
}

#[test]
fn test_if() {
    let goods = ["(if #t 1 2)", "(if #t 1)"];
    let bads = ["(if #t 1 2 3)", "(", "(lambda (x) x"];
    test_parse(&goods, &bads, NodeKind::Conditional);
}

#[test]
fn test_assignment() {
    let goods = ["(set! x 1)", "(set! x (+ 1 2))"];
    let bads = ["(set! 1 1)", "(set! x)", "(set! x 1 2)"];
    test_parse(&goods, &bads, NodeKind::Assignment);
}

#[test]
fn test_variable_definition() {
    let goods = ["(define x 1)", "(define x (+ 1 2))", "(define f (lambda (x) x))"];
    let bads = ["(define x)"];
    test_parse(&goods, &bads, NodeKind::VariableDefinition);
}

#[test]
fn test_function_definition() {
    let goods = ["(define (f x) x)", "(define (f) x)", "(define (f x) (set! x 1) x)", "(define (f x y) x)", "(define (f x . y) x)"];
    let bads = ["(define (f x))", "(define (f x . y z) x)"];
    test_parse(&goods, &bads, NodeKind::FunctionDefinition);
}

#[test]
fn test_abbreviated_quotation() {
    let goods = ["'x", "'(1 2 3)", "'(x y . z)"];
    let bads = [];
    test_parse(&goods, &bads, NodeKind::Quotation);
}

#[test]
fn test_quotation() {
    let goods = ["(quote x)", "(quote (1 2 3))", "(quote (x y . z))"];
    let bads = [];
    test_parse(&goods, &bads, NodeKind::Quotation);
}


#[test]
fn test_macro_block() {
    let goods = [
        "(let-syntax ((foo (syntax-rules (foo bar) (a b)))) a b c d)",
        "(let-syntax ((f (syntax-rules () ((_ x) x))) (g (syntax-rules () ((_ x) (f x))))) (list (f 1) (g 1)))"
    ];
    let bads = [];
    test_parse(&goods, &bads, NodeKind::MacroBlock);
}

#[test]
fn test_vector() {
    let goods = [
        "#()",
        "#(1 2 3)",
        "#(0 10 5),"
    ];
    let bads = [];
    test_parse(&goods, &bads, NodeKind::Vector);
}

#[test]
fn test_byte_vector() {
    let goods = [
        "#u8()",
        "#u8(1 2 3)",
    ];
    let bads = [];
    test_parse(&goods, &bads, NodeKind::ByteVector);
}

#[test]
fn test_define_values() {
    let goods = [
        "(define-values (x y) (+ 1 2) 1 2 3)",
    ];
    let bads = [];
    test_parse(&goods, &bads, NodeKind::ValuesDefinition);
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
    test_parse(&goods, &bads, NodeKind::RecordTypeDefinition);
}

#[test]
fn test_include() {
    let goods = [
        "(include \"foo.scm\")",
    ];
    let bads = ["(include)", "(include 1)"];
    test_parse(&goods, &bads, NodeKind::Includer);
}

#[test]
fn test_include_ci() {
    let goods = [
        "(include-ci \"foo.scm\")",
    ];
    let bads = ["(include-ci)", "(include-ci 1)"];
    test_parse(&goods, &bads, NodeKind::IncluderCI);

}

#[test]
fn test_quasiquotation() {
    let goods = [
        "`1",
        "`(1 2 3)",
    ];

    let bads = [];

    test_parse(&goods, &bads, NodeKind::Quasiquote);
}
