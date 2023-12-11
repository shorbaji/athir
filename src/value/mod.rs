pub mod port;

use crate::alloc::R;
use port::Port;
use std::collections::HashMap;

pub enum V {
    Boolean(bool),
    Bytevector(R),
    Char(char),
    EofObject,
    Null,
    Number(String),
    Pair(R, R),
    Port(Port),
    Procedure(Procedure),
    String(String),
    Symbol(String),
    Vector(R),

    Env {
        map: HashMap<String, R>,
        outer: Option<R>,
    },
    Error(Error),
    Keyword(Keyword),
    Quotation(R),
    Unspecified,
}

impl std::fmt::Debug for V {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            V::Boolean(b) => write!(f, "Boolean({b}"),
            V::Bytevector(_) => write!(f, "Bytevector"),
            V::Char(c) => write!(f, "Char({c})"),
            V::EofObject => write!(f, "EofObject"),
            V::Null => write!(f, "Null"),
            V::Number(n) => write!(f, "Number({n})"),
            V::Pair(car, cdr) => write!(f, "Pair({car:?}, {cdr:?})"),
            V::Port(p) => write!(f, "Port {p:?}"),
            V::Procedure(p) => write!(f, "Procedure {p:?}"),
            V::String(s) => write!(f, "String({s})"),
            V::Symbol(s) => write!(f, "Symbol({s})"),
            V::Vector(_) => write!(f, "Vector"),

            V::Env { map: _, outer: _ } => write!(f, "Env"),
            V::Error(s) => write!(f, "Error({s:?})"),
            V::Keyword(k) => write!(f, "Keyword({k:?})"),
            V::Quotation(q) => write!(f, "Quotation({q:?})"),
            V::Unspecified => write!(f, "Unspecified"),
        }
    }
}

pub enum Procedure {
    Closure {
        formals: R,
        body: R,
        env: R,
    },

    Continuation {
        f: fn(&R, &R, &R) -> (R, R), // continuation function (ternary)
        r: R,                        // captured environment
        k: R,                        // captured continuation
    },

    ContinuationPlus {
        f: fn(&R, &R, &R, &R) -> (R, R), // continuation function (quadrenary)
        o: R,                            // captured variable
        r: R,                            // captured environment
        k: R,                            // captured continuation
    },

    ContinuationNull,
    PrimitiveUnary(fn(&R) -> R, String),
    PrimitiveBinary(fn(&R, &R) -> R, String),
    PrimitiveERK(fn(&R, &R, &R) -> (R, R), String),
    PrimitiveOptionalUnary(fn(Option<&R>) -> R, String),
}

impl std::fmt::Debug for Procedure {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Procedure::Closure { formals, body, env } => {
                write!(f, "Closure({formals:?}, {body:?}, {env:?})")
            }
            Procedure::Continuation { f: _, r: _, k: _ } => write!(f, "Continuation"),
            Procedure::ContinuationPlus {
                f: _,
                o: _,
                r: _,
                k: _,
            } => write!(f, "ContinuationPlus"),
            Procedure::ContinuationNull => write!(f, "ContinuationNull"),
            Procedure::PrimitiveUnary(_, s) => write!(f, "PrimitiveUnary({s})"),
            Procedure::PrimitiveBinary(_, s) => write!(f, "PrimitiveBinary({s})"),
            Procedure::PrimitiveERK(_, s) => write!(f, "Primitive({s})"),
            Procedure::PrimitiveOptionalUnary(_, s) => write!(f, "PrimitiveOptionalUnary({s})"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    Syntax { depth: usize, message: String },
    Runtime { message: String },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    And,
    Begin,
    Comma,
    CommaAt,
    CondExpand,
    Define,
    DefineLibrary,
    DefineRecordType,
    DefineSyntax,
    DefineValues,
    Ellipsis,
    Else,
    Except,
    Export,
    If,
    Import,
    Include,
    IncludeCi,
    IncludeLibraryDeclarations,
    Lambda,
    LetSyntax,
    LetrecSyntax,
    Not,
    Only,
    Or,
    Prefix,
    Rename,
    Quasiquote,
    Quote,
    Set,
    SyntaxRules,
    Underscore,
    Unquote,
}

impl From<String> for Keyword {
    fn from(value: String) -> Self {
        match value.as_str() {
            "and" => Keyword::And,
            "begin" => Keyword::Begin,
            "comma" => Keyword::Comma,
            "comma-at" => Keyword::CommaAt,
            "cond-expand" => Keyword::CondExpand,
            "define" => Keyword::Define,
            "define-library" => Keyword::DefineLibrary,
            "define-record-type" => Keyword::DefineRecordType,
            "define-values" => Keyword::DefineValues,
            "define-syntax" => Keyword::DefineSyntax,
            "else" => Keyword::Else,
            "except" => Keyword::Except,
            "export" => Keyword::Export,
            "..." => Keyword::Ellipsis,
            "if" => Keyword::If,
            "import" => Keyword::Import,
            "include" => Keyword::Include,
            "include-ci" => Keyword::IncludeCi,
            "include-library-declarations" => Keyword::IncludeLibraryDeclarations,
            "lambda" => Keyword::Lambda,
            "let-syntax" => Keyword::LetSyntax,
            "letrec-syntax" => Keyword::LetrecSyntax,
            "not" => Keyword::Not,
            "only" => Keyword::Only,
            "or" => Keyword::Or,
            "prefix" => Keyword::Prefix,
            "quasiquote" => Keyword::Quasiquote,
            "quote" => Keyword::Quote,
            "rename" => Keyword::Rename,
            "set!" => Keyword::Set,
            "syntax-rules" => Keyword::SyntaxRules,
            "_" => Keyword::Underscore,
            "unquote" => Keyword::Unquote,
            _ => panic!("Cannot convert {value} to Keyword"),
        }
    }
}
