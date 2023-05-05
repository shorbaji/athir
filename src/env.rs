use crate::alloc::{R, A};
use crate::eval::{call_cc, evlis};
use crate::stdlib::base::*;
use crate::stdlib::cxr::*;

use std::collections::HashMap;

pub fn global_env() -> R {
    let mut map = HashMap::new();

    map.insert("cons".to_string(), A::binary(cons, "cons".to_string()));
    map.insert("car".to_string(), A::unary(car, "car".to_string()));
    map.insert("cdr".to_string(), A::unary(cdr, "cdr".to_string()));
    map.insert("caar".to_string(), A::unary(caar, "caar".to_string()));
    map.insert("cadr".to_string(), A::unary(cadr, "cadr".to_string()));
    map.insert("cdar".to_string(), A::unary(cdar, "cdar".to_string()));

    map.insert("cddr".to_string(), A::unary(cddr, "cddr".to_string()));
    map.insert("caaar".to_string(), A::unary(caaar, "caaar".to_string()));
    map.insert("caadr".to_string(), A::unary(caadr, "caadr".to_string()));
    map.insert("cadar".to_string(), A::unary(cadar, "cadar".to_string()));
    map.insert("caddr".to_string(), A::unary(caddr, "caddr".to_string()));
    map.insert("cdaar".to_string(), A::unary(cdaar, "cdaar".to_string()));
    map.insert("cdadr".to_string(), A::unary(cdadr, "cdadr".to_string()));
    map.insert("cddar".to_string(), A::unary(cddar, "cddar".to_string()));
    map.insert("cdddr".to_string(), A::unary(cdddr, "cdddr".to_string()));

    map.insert("caaaar".to_string(), A::unary(caaaar, "caaaar".to_string()));
    map.insert("caaadr".to_string(), A::unary(caaadr, "caaadr".to_string()));
    map.insert("caadar".to_string(), A::unary(caadar, "caadar".to_string()));
    map.insert("caaddr".to_string(), A::unary(caaddr, "caaddr".to_string()));
    map.insert("cadaar".to_string(), A::unary(cadaar, "cadaar".to_string()));
    map.insert("cadadr".to_string(), A::unary(cadadr, "cadadr".to_string()));
    map.insert("caddar".to_string(), A::unary(caddar, "caddar".to_string()));
    map.insert("cadddr".to_string(), A::unary(cadddr, "cadddr".to_string()));
    map.insert("cdaaar".to_string(), A::unary(cdaaar, "cdaaar".to_string()));
    map.insert("cdaadr".to_string(), A::unary(cdaadr, "cdaadr".to_string()));
    map.insert("cdadar".to_string(), A::unary(cdadar, "cdadar".to_string()));
    map.insert("cdaddr".to_string(), A::unary(cdaddr, "cdaddr".to_string()));
    map.insert("cddaar".to_string(), A::unary(cddaar, "cddaar".to_string()));
    map.insert("cddadr".to_string(), A::unary(cddadr, "cddadr".to_string()));
    map.insert("cdddar".to_string(), A::unary(cdddar, "cdddar".to_string()));
    map.insert("cddddr".to_string(), A::unary(cddddr, "cddddr".to_string()));

    map.insert("length".to_string(), A::unary(len, "len".to_string()));

    map.insert("boolean?".to_string(), A::unary(is_boolean, "boolean?".to_string()));
    map.insert("bytevector?".to_string(), A::unary(is_bytevector, "bytevector?".to_string()));
    map.insert("char?".to_string(), A::unary(is_char, "char?".to_string()));
    map.insert("eof-object?".to_string(), A::unary(is_eof_object, "eof-object?".to_string()));
    map.insert("null?".to_string(), A::unary(is_null, "null?".to_string()));
    map.insert("number?".to_string(), A::unary(is_number, "number?".to_string()));
    map.insert("pair?".to_string(), A::unary(is_pair, "pair?".to_string()));
    map.insert("port?".to_string(), A::unary(is_port, "port?".to_string()));
    map.insert("procedure?".to_string(), A::unary(is_procedure, "procedure?".to_string()));
    map.insert("string?".to_string(), A::unary(is_string, "string?".to_string()));
    map.insert("symbol?".to_string(), A::unary(is_symbol, "symbol?".to_string()));
    map.insert("vector?".to_string(), A::unary(is_vector, "vector?".to_string()));
    
    map.insert("display".to_string(), A::unary(display, "display".to_string()));
    map.insert("new-line".to_string(), A::unary(new_line, "new_line".to_string()));
    map.insert("identity".to_string(), A::unary(identity, "identity".to_string()));

    map.insert("call/cc".to_string(), A::primitive_erk(call_cc, "call/cc".to_string()));
    map.insert("list".to_string(), A::primitive_erk(evlis, "list".to_string()));


    map.insert("read-line".to_string(), A::optional_unary(read_line, "read_line".to_string()));
    map.insert("read".to_string(), A::optional_unary(read, "read".to_string()));
    map.insert("read-char".to_string(), A::optional_unary(read_char, "read_char".to_string()));
    map.insert("peek-char".to_string(), A::optional_unary(peek_char, "peek_char".to_string()));
    map.insert("open-input-string".to_string(), A::unary(open_input_string, "open_input_string".to_string()));

    // map.insert("+".to_string(), A::binary(plus, "+".to_string()));


    A::env(map, None)
}
