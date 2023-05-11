//! # Allocator
//! 
//! Allocates new values and returns references to them.
//! Currently uses Rust's Rc<RefCell<...>> for reference counting and interior mutability.
//! 
use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;
use std::ops::Deref;

use crate::value::{V, port::Port, Keyword, Procedure, Error, number::Number};

#[derive(Clone, Debug)]
pub struct R {
    inner: Rc<RefCell<V>>
}

impl R {
    fn new(v: V) -> R {
        R { inner: Rc::new(RefCell::new(v)) }
    }
    
    pub fn deref(&self) -> &RefCell<V> {
        self.inner.deref()
    }
}

pub struct A;

impl A {
    pub fn boolean(b: bool) -> R {
        R::new(V::Boolean(b))
    }
    
    pub fn bytevector(v: &R) -> R {
        R::new(V::Bytevector(v.clone()))
    }

    pub fn char(c: char) -> R {
        R::new(V::Char(c))
    }

    pub fn eof_object() -> R {
        R::new(V::EofObject)
    }

    pub fn null() -> R {
        R::new(V::Null)
    }
    
    pub fn number(n: Number) -> R {
        R::new(V::Number(n))
    }

    pub fn pair(car: &R, cdr: &R) -> R {
        R::new(V::Pair(car.clone(), cdr.clone()))
    }

    pub fn port_string(s: String) -> R {
        R::new(V::Port(Port::from(s)))
    }

    pub fn port_stdin() -> R {
        R::new(V::Port(Port::stdin()))
    }

    pub fn port_stdout() -> R {
        R::new(V::Port(Port::stdout()))
    }
    
    pub fn string(s: String) -> R {
        R::new(V::String(s))
    }
    
    pub fn symbol(s: &str) -> R {
        R::new(V::Symbol(s.to_string()))
    }
    
    pub fn vector(v: &R) -> R {
        R::new(V::Vector(v.clone()))
    }

    pub fn env(map: HashMap<String, R>, outer: Option<R>) -> R {
        R::new(V::Env{ map, outer })
    }
    
    pub fn runtime_error(message: String) -> R {
        R::new(V::Error(Error::Runtime { message }))
    }
    
    pub fn syntax_error(depth: usize, message: &str) -> R {
        R::new(V::Error(Error::Syntax { depth, message: message.to_string()}))
    }

    pub fn keyword(keyword: Keyword) -> R {
        R::new(V::Keyword(keyword))
    }

    pub fn quotation(e: &R) -> R {
        R::new(V::Quotation(e.clone()))
    }

    pub fn continuation(f: fn(&R, &R, &R) -> (R, R), r: &R, k: &R) -> R {
        R::new(V::Procedure(Procedure::Continuation{ f, r: r.clone(), k: k.clone() }))
    }

    pub fn continuation_plus(f: fn(&R, &R, &R, &R) -> (R, R), o: &R, r: &R, k: &R) -> R {
        R::new(V::Procedure(Procedure::ContinuationPlus{ f, o: o.clone(), r: r.clone(), k: k.clone() }))
    }

    pub fn continuation_null() -> R {
        R::new(V::Procedure(Procedure::ContinuationNull))
    }

    pub fn unary(f: fn(&R)->R, s: String) -> R {
        R::new(V::Procedure(Procedure::PrimitiveUnary(f, s)))
    }

    pub fn binary(f: fn(&R, &R)->R, s: String) -> R {
        R::new(V::Procedure(Procedure::PrimitiveBinary(f, s)))
    }

    pub fn closure(formals: &R, body: &R, env: &R) -> R {
        R::new(V::Procedure(Procedure::Closure{ formals: formals.clone(), body: body.clone(), env: env.clone() }))
    }
    
    pub fn primitive_erk(f: fn(&R, &R, &R) -> (R, R), s: String) -> R {
        R::new(V::Procedure(Procedure::PrimitiveERK(f, s)))
    }

    pub fn optional_unary(f: fn(Option<&R>)->R, s: String) -> R {
        R::new(V::Procedure(Procedure::PrimitiveOptionalUnary(f, s)))
    }

    pub fn unspecified() -> R {
        R::new(V::Unspecified)
    }

}
