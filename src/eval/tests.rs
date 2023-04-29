use crate::object::Object;

#[test]
fn test_boolean() {
    let port = Object::new_port_from_string(&Object::from("1".to_string())).unwrap();

    let expr = port.read().unwrap();

    let env = Object::new_global_env();
    println!("{:?}", expr.eval(&env));

    assert!(false);
}