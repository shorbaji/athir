/// Tokens base on R7RS
/// Uses [Logos](https://docs.rs/logos/0.11.0/logos/) to generate a lexer
///
use logos::{Lexer, Logos};

use crate::read::lexer::number::Number;

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    #[regex(r"(#(([tT][rR][uU][eE])|([fF][aA][lL][sS][eE])|([tT]|[fF])))", to_bool)]
    Boolean(bool),

    #[regex(r"((#\\x([0-9a-fA-F]+))|(#\\(alarm|backspace|delete|escape|newline|null|return|space|tab))|(#\\.))",
            to_char)]
    Character(char),

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

    #[regex(
        r"(([a-zA-Z]|[!\$%&\*/:<=>\?\^_~])(([a-zA-Z]|[!\$%&\*/:<=>\?\^_~])|[0-9]|((\+|-)|\.|@))*)",
        to_identifier
    )]
    #[regex(r"(((\+|-)|((\+|-)(([a-zA-Z]|[!\$%&\*/:<=>\?\^_~])|(\+|-)|@)(([a-zA-Z]|[!\$%&\*/:<=>\?\^_~])|[0-9]|((\+|-)|\.|@))*)|((\+|-)\.((([a-zA-Z]|[!\$%&\*/:<=>\?\^_~])|(\+|-)|@)|\.)(([a-zA-Z]|[!\$%&\*/:<=>\?\^_~])|[0-9]|((\+|-)|\.|@))*)|(\.((([a-zA-Z]|[!\$%&\*/:<=>\?\^_~])|(\+|-)|@)|\.)(([a-zA-Z]|[!\$%&\*/:<=>\?\^_~])|[0-9]|((\+|-)|\.|@))*)))",
            to_identifier)]
    Identifier(String),

    #[regex(
        r"(\|([^\|\\]|(\\x([0-9a-fA-F]+);)|(\\[aA]|\\[bB]|\\[tT]|\\[nN]|\\[rR])|(\\\|))*\|)",
        to_identifier
    )]
    VerticalLineIdentifier(String),

    // prioritize Number over Identifier since +i and -i are valid identifiers and numbers according to the r7rs spec
    #[regex(r"(((((#b)((#[eEiI])?))|(((#[eEiI])?)(#b)))((((((\+|-)?)(((((0|1))+)/(((0|1))+))|(((0|1))+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))(\+|-)(((((0|1))+)/(((0|1))+))|(((0|1))+))(i|I))|(((((\+|-)?)(((((0|1))+)/(((0|1))+))|(((0|1))+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)(i|I))|(((((\+|-)?)(((((0|1))+)/(((0|1))+))|(((0|1))+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))(\+|-)(i|I))|(((((\+|-)?)(((((0|1))+)/(((0|1))+))|(((0|1))+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))@((((\+|-)?)(((((0|1))+)/(((0|1))+))|(((0|1))+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)))|((\+|-)(((((0|1))+)/(((0|1))+))|(((0|1))+))(i|I))|((\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)(i|I))|(((((\+|-)?)(((((0|1))+)/(((0|1))+))|(((0|1))+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)))|((\+|-)(i|I))))|((((#o)((#[eEiI])?))|(((#[eEiI])?)(#o)))((((((\+|-)?)(((([0-7])+)/(([0-7])+))|(([0-7])+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))(\+|-)(((([0-7])+)/(([0-7])+))|(([0-7])+))(i|I))|(((((\+|-)?)(((([0-7])+)/(([0-7])+))|(([0-7])+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)(i|I))|(((((\+|-)?)(((([0-7])+)/(([0-7])+))|(([0-7])+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))(\+|-)(i|I))|(((((\+|-)?)(((([0-7])+)/(([0-7])+))|(([0-7])+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))@((((\+|-)?)(((([0-7])+)/(([0-7])+))|(([0-7])+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)))|((\+|-)(((([0-7])+)/(([0-7])+))|(([0-7])+))(i|I))|((\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)(i|I))|(((((\+|-)?)(((([0-7])+)/(([0-7])+))|(([0-7])+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)))|((\+|-)(i|I))))|(((((#d)?)((#[eEiI])?))|(((#[eEiI])?)((#d)?)))((((((\+|-)?)(((((([0-9]))+)/((([0-9]))+))|((([0-9]))+))|((((([0-9]))+)(((e|E)((\+|-)?)(([0-9])+))?))|(\.([0-9])+(((e|E)((\+|-)?)(([0-9])+))?))|(([0-9])+\.([0-9])*(((e|E)((\+|-)?)(([0-9])+))?)))))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))(\+|-)(((((([0-9]))+)/((([0-9]))+))|((([0-9]))+))|((((([0-9]))+)(((e|E)((\+|-)?)(([0-9])+))?))|(\.([0-9])+(((e|E)((\+|-)?)(([0-9])+))?))|(([0-9])+\.([0-9])*(((e|E)((\+|-)?)(([0-9])+))?))))(i|I))|(((((\+|-)?)(((((([0-9]))+)/((([0-9]))+))|((([0-9]))+))|((((([0-9]))+)(((e|E)((\+|-)?)(([0-9])+))?))|(\.([0-9])+(((e|E)((\+|-)?)(([0-9])+))?))|(([0-9])+\.([0-9])*(((e|E)((\+|-)?)(([0-9])+))?)))))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)(i|I))|(((((\+|-)?)(((((([0-9]))+)/((([0-9]))+))|((([0-9]))+))|((((([0-9]))+)(((e|E)((\+|-)?)(([0-9])+))?))|(\.([0-9])+(((e|E)((\+|-)?)(([0-9])+))?))|(([0-9])+\.([0-9])*(((e|E)((\+|-)?)(([0-9])+))?)))))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))(\+|-)(i|I))|(((((\+|-)?)(((((([0-9]))+)/((([0-9]))+))|((([0-9]))+))|((((([0-9]))+)(((e|E)((\+|-)?)(([0-9])+))?))|(\.([0-9])+(((e|E)((\+|-)?)(([0-9])+))?))|(([0-9])+\.([0-9])*(((e|E)((\+|-)?)(([0-9])+))?)))))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))@((((\+|-)?)(((((([0-9]))+)/((([0-9]))+))|((([0-9]))+))|((((([0-9]))+)(((e|E)((\+|-)?)(([0-9])+))?))|(\.([0-9])+(((e|E)((\+|-)?)(([0-9])+))?))|(([0-9])+\.([0-9])*(((e|E)((\+|-)?)(([0-9])+))?)))))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)))|((\+|-)(((((([0-9]))+)/((([0-9]))+))|((([0-9]))+))|((((([0-9]))+)(((e|E)((\+|-)?)(([0-9])+))?))|(\.([0-9])+(((e|E)((\+|-)?)(([0-9])+))?))|(([0-9])+\.([0-9])*(((e|E)((\+|-)?)(([0-9])+))?))))(i|I))|((\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)(i|I))|(((((\+|-)?)(((((([0-9]))+)/((([0-9]))+))|((([0-9]))+))|((((([0-9]))+)(((e|E)((\+|-)?)(([0-9])+))?))|(\.([0-9])+(((e|E)((\+|-)?)(([0-9])+))?))|(([0-9])+\.([0-9])*(((e|E)((\+|-)?)(([0-9])+))?)))))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)))|((\+|-)(i|I))))|((((#x)((#[eEiI])?))|(((#[eEiI])?)(#x)))((((((\+|-)?)((((([0-9a-fA-F]))+)/((([0-9a-fA-F]))+))|((([0-9a-fA-F]))+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))(\+|-)((((([0-9a-fA-F]))+)/((([0-9a-fA-F]))+))|((([0-9a-fA-F]))+))(i|I))|(((((\+|-)?)((((([0-9a-fA-F]))+)/((([0-9a-fA-F]))+))|((([0-9a-fA-F]))+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)(i|I))|(((((\+|-)?)((((([0-9a-fA-F]))+)/((([0-9a-fA-F]))+))|((([0-9a-fA-F]))+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))(\+|-)(i|I))|(((((\+|-)?)((((([0-9a-fA-F]))+)/((([0-9a-fA-F]))+))|((([0-9a-fA-F]))+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0))@((((\+|-)?)((((([0-9a-fA-F]))+)/((([0-9a-fA-F]))+))|((([0-9a-fA-F]))+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)))|((\+|-)((((([0-9a-fA-F]))+)/((([0-9a-fA-F]))+))|((([0-9a-fA-F]))+))(i|I))|((\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)(i|I))|(((((\+|-)?)((((([0-9a-fA-F]))+)/((([0-9a-fA-F]))+))|((([0-9a-fA-F]))+)))|(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)))|((\+|-)(i|I)))))",
        to_number,
        priority=3)]
    Number(String),

    #[regex(r"\(")]
    ParenLeft,

    #[regex(r"\)")]
    ParenRight,

    #[regex(r"`")]
    Quasiquote,

    #[regex(r"'")]
    Quote,

    #[regex(r"#\(")]
    SharpOpen,

    #[regex(r"#u8\(")]
    SharpU8Open,

    #[regex(r#""([^"\\]|(\\[aA]|\\[bB]|\\[tT]|\\[nN]|\\[rR])|\\"|\\|\\( |\t)*(\r\n|\r|\n)( |\t)*|(\\x([0-9a-fA-F]+);))*""#,
        to_string)]
    String(String),

    #[regex(r"(( |\t)|(\r\n|\r|\n))")]
    Whitespace,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let s = match self {
            Token::Boolean(b) => format!("{b}"),
            Token::Character(c) => format!("{c}"),
            Token::Comma => ",".to_string(),
            Token::CommaAt => ",@".to_string(),
            Token::Comment => ";".to_string(),
            Token::Directive => "#".to_string(),
            Token::VerticalLineIdentifier(s) => format!("|{s}|"),
            Token::Dot => ".".to_string(),
            Token::Error => "error".to_string(),
            Token::Identifier(s) => s.to_string(),
            Token::Number(n) => n.to_string(),
            Token::ParenLeft => "(".to_string(),
            Token::ParenRight => ")".to_string(),
            Token::Quasiquote => "`".to_string(),
            Token::Quote => "'".to_string(),
            Token::SharpOpen => "#(".to_string(),
            Token::SharpU8Open => "#u8(".to_string(),
            Token::String(s) => s.to_string(),
            Token::Whitespace => " ".to_string(),
        };
        write!(f, "{s}")
    }
}
//
// Helper functions to get values from tokens
//
//
fn to_bool(lex: &mut Lexer<Token>) -> Option<bool> {
    match lex.slice().to_lowercase().as_str() {
        "#t" | "#true" => Some(true),
        "#f" | "#false" => Some(false),
        _ => None,
    }
}

fn to_char(lex: &mut Lexer<Token>) -> Option<char> {
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

fn to_identifier(lex: &mut Lexer<Token>) -> Option<String> {
    Some(lex.slice().to_string())
}

fn to_number(lex: &mut Lexer<Token>) -> Option<Number> {
    Some(lex.slice().to_string())
}

fn to_string(lex: &mut Lexer<Token>) -> Option<String> {
    let s = lex.slice();

    let s: String = String::from(&s[1..s.len() - 1]);
    Some(s)
}
