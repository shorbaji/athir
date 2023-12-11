//! Base library
//! 
//! R7RS section 6.1. Standard procedures - base library
//! 
use crate::value::V;
use crate::alloc::{A, R};
use std::ops::{Deref, DerefMut};
use crate::eval::apply;

//
// Equivalence predicates
//

//
// Numbers
//

pub fn is_number(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Number(_) => A::boolean(true),
        _ => A::boolean(false)
    }
}

// pub fn plus(e1: &R, e2: &R) -> R {
//     match (e1.deref().borrow().deref(), e2.deref().borrow().deref()) {
//         (V::Number(n1), V::Number(n2)) => V::number(n1 + n2),
//         _ => panic!("not a number")
//     }
// }

// 
// Booleans
//

pub fn is_boolean(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Boolean(_) => A::boolean(true),
        _ => A::boolean(false)
    }
}

//
// Pairs and lists
//

pub fn cons(e1: &R, e2: &R) -> R { 
    A::pair(e1, e2)
}

pub fn caar(e: &R) -> R { car(&car(e)) } 
pub fn cadr(e: &R) -> R { car(&cdr(e)) } 

pub fn car(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Pair(car, _) => car.clone(),
        _ => A::runtime_error(format!("not a pair {e:?}"))
    }
}

pub fn cdar(e: &R) -> R { cdr(&car(e)) } 
pub fn cddr(e: &R) -> R { cdr(&cdr(e)) } 

pub fn cdr(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Pair(_, cdr) => cdr.clone(),
        _ => A::runtime_error(format!("not a pair {e:?}"))
    }
}

pub fn len(e: &R) -> R {
    fn len_acc(e: &R, acc: i32) -> i32 {
        match e.deref().borrow().deref() {
            V::Null => acc,
            V::Pair(_, cdr) => len_acc(cdr, acc + 1),
            _ => panic!("not a list")
        }
    }

    A::number(format!("{}", len_acc(e, 0)))
}

pub fn is_null(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Null => A::boolean(true),
        _ => A::boolean(false)
    }
}

pub fn is_pair(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Pair(_, _) => A::boolean(true),
        _ => A::boolean(false)
    }
}

//
// Symbols
//

pub fn is_symbol(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Symbol(_) => A::boolean(true),
        _ => A::boolean(false)
    }
}

//
// Characters
//

pub fn is_char(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Char(_) => A::boolean(true),
        _ => A::boolean(false)
    }
}

//
// Strings
//

pub fn is_string(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::String(_) => A::boolean(true),
        _ => A::boolean(false)
    }
}

//
// Vectors
//

pub fn is_vector(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Vector(_) => A::boolean(true),
        _ => A::boolean(false)
    }
}

//
// Bytevectors
//

pub fn is_bytevector(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Bytevector(_) => A::boolean(true),
        _ => A::boolean(false)
    }
}


//
// Control features
//

pub fn is_procedure(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Procedure(_) => A::boolean(true),
        _ => A::boolean(false)
    }
}

pub fn call_cc(e: &R, r: &R, k: &R) -> (R, R) {
    apply(&car(e), &cons(k, &A::null()), r, k)
}

//
// Exceptions
//

// pub fn is_error(e: &R) -> R {
//     match e.deref().borrow().deref() {
//         V::Error(_) => A::boolean(true),
//         _ => A::boolean(false)
//     }
// }

//
// Environments and evaluation
//

// pub fn is_env(e: &R) -> R {
//     match e.deref().borrow().deref() {
//         V::Env{map: _, outer: _} => A::boolean(true),
//         _ => A::boolean(false)
//     }
// }


// Input and Output
//

pub fn display(e: &R) -> R {
    print!("{e:?}");
    A::unspecified()
}

pub fn new_line(_: &R) -> R { println!(); A::unspecified() }


pub fn is_eof_object(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::EofObject => A::boolean(true),
        _ => A::boolean(false)
    }
}

pub fn is_port(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Port(_) => A::boolean(true),
        _ => A::boolean(false)
    }
}


pub fn read(e: Option<&R>) -> R {
    use crate::read::Reader;

    let e = match e {
        Some(e) => e.clone(),
        None => A::port_stdin(),
    };

    let x = match e.deref().borrow_mut().deref_mut() {
        V::Port(port) => port.read(),
        _ => A::runtime_error(format!("not a port {e:?}")),
    }; x
}

pub fn peek_char(e: Option<&R>) -> R {
    let e = match e {
        Some(e) => e.clone(),
        None => A::port_stdin(),
    };

    let x = match e.deref().borrow_mut().deref_mut() {
        V::Port(port) => match port.peek_char() {
            Some(c) => A::char(c),
            None => A::eof_object(),
        },
        _ => A::runtime_error(format!("not a port {e:?}")),
    }; x
    
}

pub fn read_char(e: Option<&R>) -> R {
    let e = match e {
        Some(e) => e.clone(),
        None => A::port_stdin(),
    };

    let x = match e.deref().borrow_mut().deref_mut() {
        V::Port(port) => match port.read_char() {
            Some(c) => A::char(c),
            None => A::eof_object(),
        },
        _ => A::runtime_error(format!("not a port {e:?}")),
    }; x
}

pub fn read_line(e: Option<&R>) -> R {
    let e = match e {
        Some(e) => e.clone(),
        None => A::port_stdin(),
    };

    let x = match e.deref().borrow_mut().deref_mut() {
        V::Port(port) => match port.read_line() {
            Some(s) => A::string(s),
            None => A::eof_object(),
        },
        _ => A::runtime_error(format!("not a port {e:?}")),
    }; x
}

pub fn open_input_string(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::String(s) => A::port_string(s.clone()),
        _ => A::runtime_error(format!("not a string {e:?}")),
    }
}

//
// Miscellaneous (extras)
//

pub fn identity(e: &R) -> R { e.clone() }

// pub fn is_unspecified(e: &R) -> R {
//     match e.deref().borrow().deref() {
//         V::Unspecified => A::boolean(true),
//         _ => A::boolean(false)
//     }
// }
