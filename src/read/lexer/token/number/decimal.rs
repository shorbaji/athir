use logos::{Lexer, Logos};
use crate::value::number::{Number, real::{Real, RealValue}};

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum DecimalNumberToken {
    #[token("@")]
    At,

    #[token("/")]
    Backslash,

    #[regex(r"(([0-9]+(((e|E)((\+|-)?)([0-9]+))?))|(\.[0-9]+(((e|E)((\+|-)?)([0-9]+))?))|([0-9]+\.[0-9]*(((e|E)((\+|-)?)([0-9]+))?)))", priority = 2)]
    Decimal,

    #[token(".")]
    Dot,

    #[regex(r"(#e|#E)")]
    Exact,

    #[token("i")]
    I,

    #[regex(r"(#i|#I)")]
    Inexact,

    #[regex(r"(\+inf\.0|\+INF\.0)")]
    InfPlus,

    #[regex(r"((\-inf\.0)|(\-INF\.0))")]
    InfMinus,

    #[token("-")]
    Minus,

    #[regex(r"(\+nan\.0|\+NAN\.0)")]
    NanPlus,

    #[regex(r"(-nan\.0|-NAN\.0)")]
    NanMinus,

    #[token("+")]
    Plus,

    #[token("#b")]
    Radix2,

    #[token("#o")]
    Radix8,

    #[token("#d")]
    Radix10,

    #[token("#x")]
    Radix16,

    #[regex(r"(((e|E)((\+|-)?)([0-9]+))?)", priority = 2)]
    Suffix,

    #[regex(r"[0-1]+", priority = 8)]
    Uinteger2,

    #[regex(r"[0-7]+", priority = 6)]
    Uinteger8,

    #[regex(r"[0-9]+", priority = 4)]
    Uinteger10,

    #[regex(r"[0-9a-fA-F]+")]
    Uinteger16,

}

