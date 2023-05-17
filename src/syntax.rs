use crate::alloc::{A, R};
use crate::value::V;
use crate::stdlib::base::{car, cdr, cons};

use std::ops::Deref;

pub fn pattern_match(pattern: &R, expr: &R, ellipsis: String) -> Result<R, R> {
    match (pattern.deref().borrow().deref(), expr.deref().borrow().deref()) {
        (V::Null, V::Null) => Ok(A::null()),
        (V::Boolean(a), V::Boolean(b)) if a == b => Ok(A::null()),
        (V::Number(a), V::Number(b)) if a == b => Ok(A::null()),
        (V::Char(a), V::Char(b)) if a == b => Ok(A::null()),
        (V::String(a), V::String(b)) if a == b => Ok(A::null()),
        // (V::Vector(a), V::Vector(b)) => a == b,
        // (V::Bytevector(a), V::Bytevector(b)) => a == b,
        (V::Symbol(_), V::Null) => Err(cons(pattern, expr)),
        (V::Symbol(_), _) => Ok(cons(pattern, expr)),
        (V::Pair(a, b), V::Pair(c, d)) if matches!(car(&b).deref().borrow().deref(), V::Symbol(s) if s.clone() == ellipsis.clone()) => {
            let mut found = vec!(c.clone());
            let mut rest = d.clone();
            let tail = cdr(&b);
            let mut ok = false;

            loop {
                if pattern_match(&tail, &rest, ellipsis.clone()).is_ok() { ok = true; break; } 

                if matches!(rest.deref().borrow().deref(), V::Null){ break; }

                found.push(car(&rest));
                rest = cdr(&rest);
            }

            if ok {
                let found = found.into_iter().rev().fold(A::null(), |acc, x| cons(&x, &acc));

                let mut result = cons(
                                    &cons(&a, 
                                        &cons(
                                            &car(&b),
                                            &A::null())),
                                    &found);

                if !matches!(tail.deref().borrow().deref(), V::Null) {
                    result = cons(&result, &cons(&tail, &rest));
                }

                Ok(result)
            } else {
                Err(cons(pattern, expr))
            }
        },
        (V::Pair(a, b), V::Pair(c, d)) => match (pattern_match(a, c, ellipsis.clone()), pattern_match(b, d, ellipsis)) {
            (Ok(a), Ok(b)) => Ok(cons(&a, &b)),
            _ => Err(cons(pattern, expr)),
        },
        _ => Err(cons(pattern, expr)),
    }
}