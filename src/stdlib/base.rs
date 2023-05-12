//! Base library
//! 
//! R7RS section 6.1. Standard procedures - base library
//! 
use crate::value::V;
use crate::alloc::{A, R};
use std::ops::{Deref, DerefMut};
use crate::eval::apply;
use crate::value::number::Number;

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

pub fn is_complex(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Number(n) => A::boolean(n.is_complex()),
        _ => A::boolean(false)
    }
}

pub fn is_real(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Number(n) => A::boolean(n.is_real()),
        _ => A::boolean(false)
    }
}

pub fn is_rational(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Number(n) => A::boolean(n.is_rational()),
        _ => A::boolean(false)
    }
}

pub fn is_integer(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Number(n) => A::boolean(n.is_integer()),
        _ => A::boolean(false)
    }
}

pub fn is_exact(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Number(n) => A::boolean(n.is_exact()),
        _ => A::boolean(false)
    }
}

pub fn is_inexact(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Number(n) => A::boolean(n.is_inexact()),
        _ => A::boolean(false)
    }
}

pub fn is_exact_integer(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Number(n) => A::boolean(n.is_exact_integer()),
        _ => A::boolean(false)
    }
}

pub fn is_finite(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Number(n) => A::boolean(n.is_finite()),
        _ => A::boolean(false)
    }
}

pub fn is_infinite(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Number(n) => A::boolean(n.is_infinite()),
        _ => A::boolean(false)
    }
}

pub fn is_nan(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Number(n) => A::boolean(n.is_nan()),
        _ => A::boolean(false)
    }
}

pub fn is_zero(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Number(n) => A::boolean(n.is_zero()),
        _ => A::boolean(false)
    }
}

pub fn is_positive(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Number(n) => A::boolean(n.is_positive()),
        _ => A::boolean(false)
    }
}

pub fn is_negative(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Number(n) => A::boolean(n.is_negative()),
        _ => A::boolean(false)
    }
}

pub fn is_odd(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Number(n) => A::boolean(n.is_odd()),
        _ => A::boolean(false)
    }
}

pub fn is_even(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Number(n) => A::boolean(n.is_even()),
        _ => A::boolean(false)
    }
}

pub fn abs(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Number(n) => A::number(n.clone().abs()),
        _ => panic!("not a number")
    }
}

pub fn square(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Number(n) => A::number(n.clone().square()),
        _ => panic!("not a number")
    }
}

pub fn sqrt(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Number(n) => A::number(n.clone().sqrt()),
        _ => panic!("not a number")
    }
}

pub fn add(args: &R) -> R {
    match args.deref().borrow().deref() {
        V::Null => A::number(Number::from(0)),
        V::Pair(car, cdr) => {
            match car.deref().borrow().deref() {
                V::Number(n) => {
                    match add(cdr).deref().borrow().deref() {
                        V::Number(m) => A::number(n.clone() + m.clone()),
                        _ => A::runtime_error("invalid number in arguments".to_string())
                    }
                },
                _ => A::runtime_error("invalid number in arguments".to_string())
            }
        },
        _ => A::runtime_error("not a valid list".to_string())
    }
}

pub fn sub(args: &R) -> R {
    match args.deref().borrow().deref() {
        V::Null => A::number(Number::from(0)),
        V::Pair(car, cdr) => match car.deref().borrow().deref() {
                V::Number(n) => {
                    match add(cdr).deref().borrow().deref() {
                        V::Number(m) => A::number(n.clone() - m.clone()),
                        _ => A::runtime_error("invalid number in arguments".to_string())
                    }
                },
                _ => A::runtime_error("invalid number in arguments".to_string())
            },
        _ => A::runtime_error("not a valid list".to_string())
    }
}

pub fn mul(args: &R) -> R {
    match args.deref().borrow().deref() {
        V::Null => A::number(Number::from(1)),
        V::Pair(car, cdr) => {
            match car.deref().borrow().deref() {
                V::Number(n) => {
                    match mul(cdr).deref().borrow().deref() {
                        V::Number(m) => A::number(n.clone() * m.clone()),
                        _ => A::runtime_error("invalid number in arguments".to_string())
                    }
                },
                _ => A::runtime_error("invalid number in arguments".to_string())
            }
        },
        _ => A::runtime_error("not a valid list".to_string())
    }
}

pub fn div(args: &R) -> R {
    match args.deref().borrow().deref() {
        V::Null => A::number(Number::from(1)),
        V::Pair(car, cdr) => {
            match car.deref().borrow().deref() {
                V::Number(n) => {
                    match mul(cdr).deref().borrow().deref() {
                        V::Number(m) => A::number(n.clone() / m.clone()),
                        _ => A::runtime_error("invalid number in arguments".to_string())
                    }
                },
                _ => A::runtime_error("invalid number in arguments".to_string())
            }
        },
        _ => A::runtime_error("not a valid list".to_string())
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
        _ => A::runtime_error(format!("not a pair {:?}", e))
    }
}

pub fn cdar(e: &R) -> R { cdr(&car(e)) } 
pub fn cddr(e: &R) -> R { cdr(&cdr(e)) } 

pub fn cdr(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::Pair(_, cdr) => cdr.clone(),
        _ => A::runtime_error(format!("not a pair {:?}", e))
    }
}

pub fn len(e: &R) -> R {
    fn len_acc(e: &R, acc: u32) -> u32 {
        match e.deref().borrow().deref() {
            V::Null => acc,
            V::Pair(_, cdr) => len_acc(cdr, acc + 1),
            _ => panic!("not a list")
        }
    }

    A::number(Number::from(len_acc(e, 0)))
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
    print!("{:?}", e);
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
        _ => A::runtime_error(format!("not a port")),
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
        _ => A::runtime_error(format!("not a port")),
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
        _ => A::runtime_error(format!("not a port")),
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
        _ => A::runtime_error(format!("not a port")),
    }; x
}

pub fn open_input_string(e: &R) -> R {
    match e.deref().borrow().deref() {
        V::String(s) => A::port_string(s.clone()),
        _ => A::runtime_error(format!("not a string")),
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
