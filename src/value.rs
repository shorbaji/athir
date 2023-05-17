pub mod port;
pub mod number;
pub mod procedure;
pub mod transformer;
mod keyword;

use std::{collections::HashMap, ops::Deref};

use crate::alloc::{A, R};
use crate::stdlib::base::{car, cdr};

use number::Number;
use port::Port;
use procedure::Procedure;
use transformer::Transformer;

pub enum V {
    Boolean(bool),
    Bytevector(Vec<u8>),
    Char(char),
    EofObject,
    Null,
    Number(Number),
    Pair(R, R),
    Port(Port),
    Procedure(Procedure),
    String(String),
    Symbol(String),
    Vector(Vec<R>),

    Env{ map: HashMap<String, R>, outer: Option<R> },
    Error(Error),
    
    Transformer(R),
    Unspecified,
}

impl std::fmt::Debug for V {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            V::Boolean(b) => write!(f, "Boolean({})", b),
            V::Bytevector(_) => write!(f, "Bytevector"),
            V::Char(c) => write!(f, "Char({})", c),
            V::EofObject => write!(f, "EofObject"),
            V::Null => write!(f, "Null"),
            V::Number(n) => write!(f, "Number({:?})", n),
            V::Pair(car, cdr) => write!(f, "Pair({:?}, {:?})", car, cdr),
            V::Port(p) => write!(f, "Port {:?}", p),
            V::Procedure(p) => write!(f, "Procedure {:?}", p),
            V::String(s) => write!(f, "String({})", s),
            V::Symbol(s) => write!(f, "Symbol({})", s),
            V::Vector(_) => write!(f, "Vector"),

            V::Env{ map:_, outer:_ } => write!(f, "Env"),
            V::Error(s) => write!(f, "Error({:?})", s),

            V::Transformer(t) => write!(f, "TransformerSpec({:?})", t),
            V::Unspecified => write!(f, "Unspecified"),
        }
    }
}

impl std::fmt::Display for V {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            V::Boolean(b) => write!(f, "{}", match b { true => "#t", false => "#f" }),
            V::Bytevector(_) => write!(f, "#<bytevector>"),
            V::Char(c) => write!(f, "#\\{}", c),
            V::EofObject => write!(f, "#<eof-object>"),
            V::Null => write!(f, "()"),
            V::Number(n) => write!(f, "{}", n),
            V::Pair(ncar, ncdr) => {
                write!(f, "(");

                let mut ls = A::pair(ncar, ncdr);

                loop {
                    write!(f, "{}", car(&ls));
                    let x = match cdr(&ls).deref().borrow().deref() {
                        V::Null => break,
                        V::Pair(_, _) => {
                            ls = cdr(&ls);
                            write!(f, " ");
                            continue;
                        },
                        _ => {
                            write!(f, " . {}", cdr(&ls));
                            break;
                        }
                    };
                }                

                write!(f, ")")
            },
            V::Port(p) => write!(f, "#<port {:?}>", p),
            V::Procedure(p) => write!(f, "#<procedure {:?}>", p),
            V::String(s) => write!(f, "\"{}\"", s),
            V::Symbol(s) => write!(f, "{}", s),
            V::Vector(_) => write!(f, "#<vector>"),

            V::Env{ map:_, outer:_ } => write!(f, "#<env>"),
            V::Error(s) => write!(f, "#<error {:?}>", s),

            V::Transformer(_) => write!(f, "#<macro>"),
            V::Unspecified => write!(f, "#<unspecified>"),
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    Syntax{depth: usize, message: String},
    Runtime{message: String},
}
