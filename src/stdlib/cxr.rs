//! # cxr
//!
//! cxr functions are used to access the elements of a list.
//! R7RS section 6.1 Standard procedures - cxr library
//!
use crate::alloc::R;
use crate::stdlib::base::{caar, cadr, car, cdar, cddr, cdr};

pub fn caaaar(e: &R) -> R {
    car(&caaar(e))
}
pub fn caaadr(e: &R) -> R {
    car(&caadr(e))
}
pub fn caaar(e: &R) -> R {
    car(&caar(e))
}
pub fn caadar(e: &R) -> R {
    car(&cadar(e))
}
pub fn caaddr(e: &R) -> R {
    car(&caddr(e))
}
pub fn caadr(e: &R) -> R {
    car(&cadr(e))
}
pub fn cadaar(e: &R) -> R {
    car(&cdaar(e))
}
pub fn cadadr(e: &R) -> R {
    car(&cdadr(e))
}
pub fn cadar(e: &R) -> R {
    car(&cdar(e))
}
pub fn caddar(e: &R) -> R {
    car(&cddar(e))
}
pub fn cadddr(e: &R) -> R {
    car(&cdddr(e))
}
pub fn caddr(e: &R) -> R {
    car(&cddr(e))
}
pub fn cdaaar(e: &R) -> R {
    cdr(&caaar(e))
}
pub fn cdaadr(e: &R) -> R {
    cdr(&caadr(e))
}
pub fn cdaar(e: &R) -> R {
    cdr(&caar(e))
}
pub fn cdadar(e: &R) -> R {
    cdr(&cadar(e))
}
pub fn cdaddr(e: &R) -> R {
    cdr(&caddr(e))
}
pub fn cdadr(e: &R) -> R {
    cdr(&cadr(e))
}
pub fn cddaar(e: &R) -> R {
    cdr(&cdaar(e))
}
pub fn cddadr(e: &R) -> R {
    cdr(&cdadr(e))
}
pub fn cddar(e: &R) -> R {
    cdr(&cdar(e))
}
pub fn cdddar(e: &R) -> R {
    cdr(&cddar(e))
}
pub fn cddddr(e: &R) -> R {
    cdr(&cdddr(e))
}
pub fn cdddr(e: &R) -> R {
    cdr(&cddr(e))
}
