use crate::object::{Object, Value};


fn result_from_str(s: &str) -> Object {
    let port = Object::new_port_from_string(&Object::from(s.to_string())).unwrap();
    let expr = port.read().unwrap();
    let env = Object::new_global_env();
    
    expr.eval(&env).unwrap()

}

fn test_x_yields_x(s: &str, p: fn(&Object) -> Result<Object, Object>) {

    let result = result_from_str(s);

    assert!(<Object as Into<bool>>::into(p(&result).unwrap()));    
}

#[test]
fn test_boolean_yields_boolean() {
    test_x_yields_x("#t", |x| x.is_boolean());
}

#[test]
fn test_bytevector_yields_bytevector() {
    test_x_yields_x("#u8(1 2 3)", |x| x.is_bytevector());
}

#[test]
fn test_char_yields_char() {
    test_x_yields_x("#\\a", |x| x.is_character());
}

#[test]
fn test_number_yields_number() {
    test_x_yields_x("1", |x| x.is_number());
}

#[test]
fn test_pair_yields_pair() {
    test_x_yields_x("'(1 . 2)", |x| x.is_pair());
}

#[test]
fn test_procedure_yields_procedure() {
    test_x_yields_x("(lambda (x) x)", |x| x.is_procedure());
}

#[test]
fn test_string_yields_string() {
    test_x_yields_x("\"'a\"", |x| x.is_string());
}

#[test]
fn test_symbol_yields_symbol() {
    test_x_yields_x("'foo", |x| x.is_symbol());
}

#[test]
fn test_vector_yields_vector() {
    test_x_yields_x("#(1 2 3)", |x| x.is_vector());
}

#[test]
fn test_define_yields_unspecified() {
    test_x_yields_x("(define x 1)", |x| x.is_unspecified());
}

#[test]
fn test_define_function_yields_unspecified() {
    test_x_yields_x("(define (f x) x)", |x| x.is_unspecified());
}

#[test]
fn test_add_two() {
    let result = result_from_str("(+ 1 2)");
    assert_eq!(result, Object::new_number("3".to_string()));
}

#[test]
fn test_lambda_invocation() {
    let result = result_from_str("((lambda (x) x) 1)");
    assert_eq!(result, Object::new_number("1".to_string()));
}


#[test]
fn test_quote_list() {
    let result = result_from_str("(quote (a b c))");

    assert_eq!(result.len().unwrap(), Object::new_number("3".to_string()));
    assert_eq!(result.car().unwrap(), Object::new_symbol("a".to_string()));
    assert_eq!(result.cadr().unwrap(), Object::new_symbol("b".to_string()));
    assert_eq!(result.caddr().unwrap(), Object::new_symbol("c".to_string()));

}

#[test]
fn test_boolean() {
    let result = result_from_str("#t");
    assert_eq!(result, Object::from(true));
}

#[test]
fn test_bytevector() {
    match *result_from_str("#u8(1 2 3)").borrow() {
        Value::Bytevector(ref v) => {
            assert_eq!(v.len().unwrap(), Object::new_number("3".to_string()));
            assert_eq!(v.car().unwrap(), Object::new_number("1".to_string()));
            assert_eq!(v.cadr().unwrap(), Object::new_number("2".to_string()));
            assert_eq!(v.caddr().unwrap(), Object::new_number("3".to_string()));
        },
        _ => panic!("expected bytevector"),
    }
}

#[test]
fn test_character() {
    let result = result_from_str("#\\a");
    assert_eq!(result, Object::from('a'));
}

#[test]
fn test_number() {
    let result = result_from_str("1");
    assert_eq!(result, Object::new_number("1".to_string()));
}

#[test]
fn test_string() {
    let result = result_from_str("\"foo\"");
    assert_eq!(result, Object::from("foo".to_string()));
}

#[test]
fn test_vector() {
    match *result_from_str("#(1 2 3)").borrow() {
        Value::Vector(ref v) => {
            assert_eq!(v.len().unwrap(), Object::new_number("3".to_string()));
            assert_eq!(v.car().unwrap(), Object::new_number("1".to_string()));
            assert_eq!(v.cadr().unwrap(), Object::new_number("2".to_string()));
            assert_eq!(v.caddr().unwrap(), Object::new_number("3".to_string()));
        },
        _ => panic!("expected vector"),
    }
}

#[test]
fn test_quote_short_atom() {
    let result = result_from_str("'a");
    assert_eq!(result, Object::new_symbol("a".to_string()));
}

#[test]
fn test_quote_atom() {
    let result = result_from_str("(quote a)");
    assert_eq!(result, Object::new_symbol("a".to_string()));
}

#[test]
fn test_quote_short_list() {
    let result = result_from_str("'(a b c)");

    assert_eq!(result.len().unwrap(), Object::new_number("3".to_string()));
    assert_eq!(result.car().unwrap(), Object::new_symbol("a".to_string()));
    assert_eq!(result.cadr().unwrap(), Object::new_symbol("b".to_string()));
    assert_eq!(result.caddr().unwrap(), Object::new_symbol("c".to_string()));

}

#[test]
fn test_lookup() {
    let result = result_from_str("+");
    assert_eq!(result.is_procedure().unwrap(), Object::from(true));
}

#[test]
fn test_if_consequent() {
    let result = result_from_str("(if #t 1 2)");
    assert_eq!(result, Object::new_number("1".to_string()));
}

#[test]
fn test_if_alternative() {
    let result = result_from_str("(if #f 1 2)");
    assert_eq!(result, Object::new_number("2".to_string()));
}

#[test]
fn test_lambda() {
    let result = result_from_str("(lambda (x) x)");
    assert_eq!(result.is_procedure().unwrap(), Object::from(true));
}

#[test]
fn test_define() {
    let result = result_from_str("(define x 1)");
    assert_eq!(result.is_unspecified().unwrap(), Object::from(true));
}

#[test]
fn test_define_function() {
    let result = result_from_str("(define (f x) x)");
    assert_eq!(result.is_unspecified().unwrap(), Object::from(true));
}

#[test]
fn test_procedure_call() {
    let result = result_from_str("((lambda (x) x) 1)");
    assert_eq!(result, Object::new_number("1".to_string()));
}

#[test]
fn test_builtin_procedure_call() {
    let result = result_from_str("(+ 1 2)");
    assert_eq!(result, Object::new_number("3".to_string()));
}

#[test]
fn test_apply() {
    let result = result_from_str("(apply + '(1 2 3))");
    assert_eq!(result, Object::new_number("6".to_string()));
}

// #[test]
// fn test_set() {
//     let env = Object::new_global_env();

//     let s = "(define x 1) (set! x 2) x";

//     let port = Object::new_port_from_string(&Object::from(s.to_string())).unwrap();

//     let expr = port.read().unwrap();  
//     let result = expr.eval(&env).unwrap();
//     assert_eq!(result.is_unspecified().unwrap(), Object::from(true));

//     let expr = port.read().unwrap();  
//     let result = expr.eval(&env).unwrap();
//     assert_eq!(result.is_unspecified().unwrap(), Object::from(true));

//     let expr = port.read().unwrap();  
//     let result = expr.eval(&env).unwrap();
//     assert_eq!(result, Object::new_number("2".to_string()));

// }