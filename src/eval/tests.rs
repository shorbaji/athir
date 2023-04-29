use crate::object::Object;


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
