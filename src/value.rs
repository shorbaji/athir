pub mod port;
pub mod number;
pub mod procedure;

use std::collections::HashMap;

use crate::alloc::R;
use number::Number;
use port::Port;
use procedure::Procedure;

pub enum V {
    Boolean(bool),
    Bytevector(R),
    Char(char),
    EofObject,
    Null,
    Number(Number),
    Pair(R, R),
    Port(Port),
    Procedure(Procedure),
    String(String),
    Symbol(String),
    Vector(R),

    Env{ map: HashMap<String, R>, outer: Option<R> },
    Error(Error),
    Keyword(Keyword),
    Quotation(R),
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
            V::Keyword(k) => write!(f, "Keyword({:?})", k),
            V::Quotation(q) => write!(f, "Quotation({:?})", q),
            V::Unspecified => write!(f, "Unspecified"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    Syntax{depth: usize, message: String},
    Runtime{message: String},
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
            _ => panic!("Cannot convert {} to Keyword", value),
        }
    }
}

