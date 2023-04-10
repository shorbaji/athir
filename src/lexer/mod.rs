//! Lexical analyzer
//! 
//! This module implements the lexical analyzer for Athir
//! 
//! It currently uses the [logos](https://crates.io/crates/logos) crate.
//! It is based on section 7.1.1 the [R7RS](https://small.r7rs.org/attachment/r7rs.pdf) specification
//! 
//! Example usage:
//! 
//! let lex = Lexeme::delimited_lexer(&line);
//! for token in lex {
//!     println!("token: {:?}", token);
//! }
//! 
//! Known issues:
//! - Though curly and square braces are reserved for future use we do not ascribe any meaning to them
//! - Only simple comments support (no nested comments or comments with datum)
//! - No values attached to lexemes

#![warn(missing_docs)]

#[cfg(test)]
mod tests;
use logos::{Lexer, Logos};
use std::{iter::Peekable};

mod values {
    //! This module contains a set of functions to extract values from lexemes
    //! These are not currently used
    use logos::Lexer;
    use crate::lexer::Lexeme;

    pub fn lexeme_to_bool(lex: &mut Lexer<Lexeme>) -> Option<bool> {
        match lex.slice().to_lowercase().as_str() {
            "#t" | "#true" => Some(true),
            "#f" | "#false" => Some(false),
            _ => None,
        }
    }
    
    pub fn lexeme_to_char(lex: &mut Lexer<Lexeme>) -> Option<char> {
        let s = lex.slice();
    
        if s.len() == 3 {
            return Some(s[2..].chars().next().unwrap());
        } else {
            match &s[0..3] {
                "#\\x" => {
                    let hex = &s[3..];
                    let hex = u32::from_str_radix(hex, 16).unwrap();
                    let c = std::char::from_u32(hex).unwrap();
                    Some(c)
                }
                _ => match &s[2..] {
                    "alarm" => Some('\u{0007}'),
                    "backspace" => Some('\u{0008}'),
                    "delete" => Some('\u{007F}'),
                    "escape" => Some('\u{001B}'),
                    "newline" => Some('\u{000A}'),
                    "null" => Some('\u{0000}'),
                    "return" => Some('\u{000D}'),
                    "space" => Some('\u{0020}'),
                    "tab" => Some('\u{0009}'),
                    _ => None,
                },
            }
        }
    }
    
    fn lexeme_to_identifier(lex: &mut Lexer<Lexeme>) -> Option<String> {
        Some(lex.slice().to_string())
    }
    
    // enum NumericValue {
    //     Integer(i64),
    //     Float(f64),
    // }
    
    // struct Number {
    //     exactness: Option<char>,
    //     numerator: NumericValue,
    //     denominator: Option<NumericValue>,
    // }
    
    type Number = String;
    
    fn lexeme_to_number(lex: &mut Lexer<Lexeme>) -> Option<Number> {
        Some(lex.slice().to_string())
    }
    
    fn lexeme_to_string(lex: &mut Lexer<Lexeme>) -> Option<String> {
        let s = lex.slice();
    
        let s: String = String::from(&s[1..s.len() - 1]);
        Some(s)
    }    
}
/// Tokens base on R7RS
// from R7RS <token> -> <identifier>| <boolean> | <number>
///     | <character> | <string> 
///     | ( | ) | #( | #u8( | ' | ` | , | ,@ | .
/// 

#[derive(Logos, Debug, PartialEq)]
pub enum Lexeme {
    #[regex(r"(#(([tT][rR][uU][eE])|([fF][aA][lL][sS][eE])|([tT]|[fF])))")]
    Boolean,

    #[regex(r"((#\\x([0-9a-fA-F]+))|(#\\(alarm|backspace|delete|escape|newline|null|return|space|tab))|(#\\.))")]
    Character,

    #[regex(r",")]
    Comma,

    #[regex(r",@")]
    CommaAt,

    #[regex(r"\.")]
    Dot,

    #[error]
    Error,

    #[regex(r"(;[^(\r\n|\r|\n)]*)", logos::skip)]
    Comment,

    #[regex(r"((#!fold-case)|(#!no-fold-case))")]
    Directive,

    #[regex(r"(([a-zA-Z]|[!\$%&\*/:<=>\?\^_~])(([a-zA-Z]|[!\$%&\*/:<=>\?\^_~])|[0-9]|((\+|-)|\.|@))*)")]
    #[regex(r"(((\+|-)|((\+|-)(([a-zA-Z]|[!\$%&\*/:<=>\?\^_~])|(\+|-)|@)(([a-zA-Z]|[!\$%&\*/:<=>\?\^_~])|[0-9]|((\+|-)|\.|@))*)|((\+|-)\.((([a-zA-Z]|[!\$%&\*/:<=>\?\^_~])|(\+|-)|@)|\.)(([a-zA-Z]|[!\$%&\*/:<=>\?\^_~])|[0-9]|((\+|-)|\.|@))*)|(\.((([a-zA-Z]|[!\$%&\*/:<=>\?\^_~])|(\+|-)|@)|\.)(([a-zA-Z]|[!\$%&\*/:<=>\?\^_~])|[0-9]|((\+|-)|\.|@))*)))")]
    Identifier,

    #[regex(r"(\|([^\|\\]|(\\x([0-9a-fA-F]+);)|(\\[aA]|\\[bB]|\\[tT]|\\[nN]|\\[rR])|(\\\|))*\|)")]
    VerticalLineIdentifier,

    // prioritize Number of Identifier since +i and -i are valid identifiers and numbers according to the r7rs spec
    #[regex(r"(((((#b)((#[eEiI])?))|(((#[eEiI])?)(#b)))((((((\+|-)?)(((((0|1))+)/(((0|1))+))|(((0|1))+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))(\+|-)(((((0|1))+)/(((0|1))+))|(((0|1))+))(i|I))|(((((\+|-)?)(((((0|1))+)/(((0|1))+))|(((0|1))+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)(i|I))|(((((\+|-)?)(((((0|1))+)/(((0|1))+))|(((0|1))+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))(\+|-)(i|I))|(((((\+|-)?)(((((0|1))+)/(((0|1))+))|(((0|1))+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))@((((\+|-)?)(((((0|1))+)/(((0|1))+))|(((0|1))+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)))|((\+|-)(((((0|1))+)/(((0|1))+))|(((0|1))+))(i|I))|((\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)(i|I))|(((((\+|-)?)(((((0|1))+)/(((0|1))+))|(((0|1))+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)))|((\+|-)(i|I))))|((((#o)((#[eEiI])?))|(((#[eEiI])?)(#o)))((((((\+|-)?)(((([0-7])+)/(([0-7])+))|(([0-7])+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))(\+|-)(((([0-7])+)/(([0-7])+))|(([0-7])+))(i|I))|(((((\+|-)?)(((([0-7])+)/(([0-7])+))|(([0-7])+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)(i|I))|(((((\+|-)?)(((([0-7])+)/(([0-7])+))|(([0-7])+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))(\+|-)(i|I))|(((((\+|-)?)(((([0-7])+)/(([0-7])+))|(([0-7])+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))@((((\+|-)?)(((([0-7])+)/(([0-7])+))|(([0-7])+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)))|((\+|-)(((([0-7])+)/(([0-7])+))|(([0-7])+))(i|I))|((\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)(i|I))|(((((\+|-)?)(((([0-7])+)/(([0-7])+))|(([0-7])+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)))|((\+|-)(i|I))))|(((((#d)?)((#[eEiI])?))|(((#[eEiI])?)((#d)?)))((((((\+|-)?)(((((([0-9]))+)/((([0-9]))+))|((([0-9]))+))|((((([0-9]))+)(((e|E)((\+|-)?)(([0-9])+))?))|(\.([0-9])+(((e|E)((\+|-)?)(([0-9])+))?))|(([0-9])+\.([0-9])*(((e|E)((\+|-)?)(([0-9])+))?)))))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))(\+|-)(((((([0-9]))+)/((([0-9]))+))|((([0-9]))+))|((((([0-9]))+)(((e|E)((\+|-)?)(([0-9])+))?))|(\.([0-9])+(((e|E)((\+|-)?)(([0-9])+))?))|(([0-9])+\.([0-9])*(((e|E)((\+|-)?)(([0-9])+))?))))(i|I))|(((((\+|-)?)(((((([0-9]))+)/((([0-9]))+))|((([0-9]))+))|((((([0-9]))+)(((e|E)((\+|-)?)(([0-9])+))?))|(\.([0-9])+(((e|E)((\+|-)?)(([0-9])+))?))|(([0-9])+\.([0-9])*(((e|E)((\+|-)?)(([0-9])+))?)))))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)(i|I))|(((((\+|-)?)(((((([0-9]))+)/((([0-9]))+))|((([0-9]))+))|((((([0-9]))+)(((e|E)((\+|-)?)(([0-9])+))?))|(\.([0-9])+(((e|E)((\+|-)?)(([0-9])+))?))|(([0-9])+\.([0-9])*(((e|E)((\+|-)?)(([0-9])+))?)))))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))(\+|-)(i|I))|(((((\+|-)?)(((((([0-9]))+)/((([0-9]))+))|((([0-9]))+))|((((([0-9]))+)(((e|E)((\+|-)?)(([0-9])+))?))|(\.([0-9])+(((e|E)((\+|-)?)(([0-9])+))?))|(([0-9])+\.([0-9])*(((e|E)((\+|-)?)(([0-9])+))?)))))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))@((((\+|-)?)(((((([0-9]))+)/((([0-9]))+))|((([0-9]))+))|((((([0-9]))+)(((e|E)((\+|-)?)(([0-9])+))?))|(\.([0-9])+(((e|E)((\+|-)?)(([0-9])+))?))|(([0-9])+\.([0-9])*(((e|E)((\+|-)?)(([0-9])+))?)))))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)))|((\+|-)(((((([0-9]))+)/((([0-9]))+))|((([0-9]))+))|((((([0-9]))+)(((e|E)((\+|-)?)(([0-9])+))?))|(\.([0-9])+(((e|E)((\+|-)?)(([0-9])+))?))|(([0-9])+\.([0-9])*(((e|E)((\+|-)?)(([0-9])+))?))))(i|I))|((\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)(i|I))|(((((\+|-)?)(((((([0-9]))+)/((([0-9]))+))|((([0-9]))+))|((((([0-9]))+)(((e|E)((\+|-)?)(([0-9])+))?))|(\.([0-9])+(((e|E)((\+|-)?)(([0-9])+))?))|(([0-9])+\.([0-9])*(((e|E)((\+|-)?)(([0-9])+))?)))))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)))|((\+|-)(i|I))))|((((#x)((#[eEiI])?))|(((#[eEiI])?)(#x)))((((((\+|-)?)((((([0-9a-fA-F]))+)/((([0-9a-fA-F]))+))|((([0-9a-fA-F]))+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))(\+|-)((((([0-9a-fA-F]))+)/((([0-9a-fA-F]))+))|((([0-9a-fA-F]))+))(i|I))|(((((\+|-)?)((((([0-9a-fA-F]))+)/((([0-9a-fA-F]))+))|((([0-9a-fA-F]))+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)(i|I))|(((((\+|-)?)((((([0-9a-fA-F]))+)/((([0-9a-fA-F]))+))|((([0-9a-fA-F]))+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))(\+|-)(i|I))|(((((\+|-)?)((((([0-9a-fA-F]))+)/((([0-9a-fA-F]))+))|((([0-9a-fA-F]))+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))@((((\+|-)?)((((([0-9a-fA-F]))+)/((([0-9a-fA-F]))+))|((([0-9a-fA-F]))+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)))|((\+|-)((((([0-9a-fA-F]))+)/((([0-9a-fA-F]))+))|((([0-9a-fA-F]))+))(i|I))|((\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)(i|I))|(((((\+|-)?)((((([0-9a-fA-F]))+)/((([0-9a-fA-F]))+))|((([0-9a-fA-F]))+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)))|((\+|-)(i|I)))))",
        priority=3)] 
    Number,

    #[regex(r"\(")]
    ParenOpen,

    #[regex(r"\)")]
    ParenClose,

    #[regex(r"`")]
    Quasiquote,

    #[regex(r"'")]
    Quote,

    #[regex(r"#\(")]
    SharpOpen,

    #[regex(r"#u8\(")]
    SharpU8Open,

    #[regex(r#""([^"\\]|(\\[aA]|\\[bB]|\\[tT]|\\[nN]|\\[rR])|\\"|\\|\\( |\t)*(\r\n|\r|\n)( |\t)*|(\\x([0-9a-fA-F]+);))*""#)]
    String,

    #[regex(r"(( |\t)|(\r\n|\r|\n))")]
    Whitespace,
}

impl Lexeme {
    pub fn delimited_lexer(input: &str) -> DelimitedLexer {
    //! This function is used to create a lexer that the following tokens are 
    //! terminated by a delimiter:
    //! boolean, character, directive, dot, identifier (without vertical lines), number 
    //! 
    //! This is required since Rust regex implementation does not support lookaheads.
        DelimitedLexer::new(Lexeme::lexer(input))
    }
}

/// This struct encapsulates a peekable Logos Lexer
pub struct DelimitedLexer<'a> {
    inner: Peekable<Lexer<'a, Lexeme>>,
}

impl<'a> DelimitedLexer<'a> {
    fn new(lexer: Lexer<'a, Lexeme>) -> Self {
        Self {
            inner: lexer.peekable(),
        }
    }
}

impl Iterator for DelimitedLexer<'_> {
    type Item = Lexeme;

    fn next(&mut self) -> Option<Self::Item> {
        //! This function returns the next token in the lexer.
        //! 
        //! Boolean, character, directive, dot, identifier (without vertical lines), number must
        //! be terminated by a delimiter.
        //! 
        use Lexeme::*;

        match self.inner.peek()? {
            // If the next lexeme is a boolean, character, directive, dot, identifier (without vertical lines), number
            // then we need to check if the lexeme after it starts with a delimiter.
            Boolean | Character | Dot | Directive | Identifier | Number => {
                let lexeme = self.inner.next()?;
                let next = self.inner.peek();

                match next {
                    None => Some(lexeme),
                    Some(next) => match next {
                        Error| Whitespace | ParenClose | ParenOpen | String | Comment | VerticalLineIdentifier=> {
                            Some(lexeme)
                        },
                        _ => {
                            self.inner.next();
                            Some(Error)
                        }
                    }
                }
            },
            VerticalLineIdentifier => {
                self.inner.next();
                Some(Identifier)
            },
            _ => self.inner.next(),    
        }
    }
}

