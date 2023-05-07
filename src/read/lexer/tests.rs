use crate::read::lexer::Token;
use crate::read::lexer::Lexer;
use std::iter::Peekable;

struct TestLexer {
    string: String,
    used: bool,
    tokens: Peekable<std::vec::IntoIter<Token>>,
}

impl TestLexer {
    fn new(string: String) -> Self {
        Self {
            string,
            tokens: vec!().into_iter().peekable(),
            used: false,
        }
    }
}
impl Lexer for TestLexer {
    fn read_line(&mut self) -> Option<String> {
        if self.used {
            None
        } else {
            self.used = true;
            Some(self.string.clone())
        }
    }

    fn get_tokens(&mut self) -> &mut Peekable<std::vec::IntoIter<Token>> {
        &mut self.tokens
    }

    fn set_tokens(&mut self, tokens: Peekable<std::vec::IntoIter<Token>>) {
        self.tokens = tokens;
    }
}

fn test_input(input: &str, expected: Vec<Token>)
{
    let mut lex = TestLexer::new(input.to_string());
    
    for token in expected {
        assert_eq!(lex.get_next_token(), Some(token));
    }
}

#[test]
fn test_boolean() {
    [
        ("#t", true),
        ("#f", false),
        ("#true", true),
        ("#false", false),
        ("#T", true),
        ("#F", false),
        ("#True", true),
        ("#False", false),
    ]
    .iter()
    .for_each(|(s, b)| test_input(s, vec![Token::Boolean(*b)]));
}

fn test_input_char(pairs: &[(&'static str, char)]) {
    pairs
        .iter()
        .for_each(|(s, c)| test_input(s, vec![Token::Character(*c)]));
}

#[test]
fn test_string_to_char() {
    let pairs = [
        ("#\\A", 'A'),
        ("#\\alarm", '\u{0007}'),
        ("#\\backspace", '\u{0008}'),
        ("#\\delete", '\u{007F}'),
        ("#\\escape", '\u{001B}'),
        ("#\\newline", '\u{000A}'),
        ("#\\null", '\u{0000}'),
        ("#\\return", '\u{000D}'),
        ("#\\space", '\u{0020}'),
        ("#\\tab", '\u{0009}'),
        ("#\\x41", 'A'),
        ("#\\x6a", 'j'),
        ("#\\x263a", '☺'),
    ];

    test_input_char(&pairs);
}

#[test]
fn test_char_simple() {
    let pairs = [("#\\a", 'a'), ("#\\\\", '\\'), ("#\\\"", '\"')];
    test_input_char(&pairs);
}

#[test]
fn test_char_named() {
    let pairs = [
        ("#\\alarm", '\u{0007}'),
        ("#\\backspace", '\u{0008}'),
        ("#\\delete", '\u{007F}'),
        ("#\\escape", '\u{001B}'),
        ("#\\newline", '\u{000A}'),
        ("#\\null", '\u{0000}'),
        ("#\\return", '\u{000D}'),
        ("#\\space", '\u{0020}'),
        ("#\\tab", '\u{0009}'),
    ];

    test_input_char(&pairs);
}

#[test]
fn test_char_hex() {
    let pairs = [("#\\x41", 'A'), ("#\\x6a", 'j'), ("#\\x263a", '☺')];
    test_input_char(&pairs);
}

#[test]
fn test_comma() {
    test_input(",", vec![Token::Comma]);
}

#[test]
fn test_comma_at() {
    test_input(",@", vec![Token::CommaAt]);
}

#[test]
fn test_dot() {
    test_input(".", vec![Token::Dot]);
}

#[test]
fn test_identifier() {
    [
        "a", "cc", "c0", "d+", "e-", "f.", "g@", "|h|", "+", "+j", "--", "+@", "+.k", "+..",
        ".lmn", "..."
    ]
    .iter()
    .for_each(|s| test_input(s, vec![Token::Identifier(s.to_string())]));
}

#[test]
fn test_number() {
    [
        "0",
        "#b1",
        "#o#e23",
        "#x#i45",
        "#d6@7",
        "#b101+111i",
        "#o#e23-45i",
        "#xabc-i",
        "#d#e+i",
        "-inf.0",
        "+NAN.0",
        "#b-nan.0i",
        "+i",
        "-i",
        "1.234e567",
    ]
    .iter()
    .for_each(|s| test_input(s, vec![Token::Number(s.to_string())]));
}

#[test]
fn test_paren_open() {
    test_input("(", vec![Token::ParenLeft]);
}

#[test]
fn test_paren_close() {
    test_input(")", vec![Token::ParenRight]);
}

#[test]
fn test_sharp_open() {
    test_input("#(", vec![Token::SharpOpen]);
}

#[test]
fn test_sharp_u8_open() {
    test_input("#u8(", vec![Token::SharpU8Open]);
}

fn test_input_string(pairs: &[(&'static str, &'static str)]) {
    pairs
        .iter()
        .for_each(|(a, b)| test_input(a, vec![Token::String(b.to_string())]));
}

#[test]
fn test_string_simple() {
    let pairs = [
        ("\"\"", ""),
        ("\"a\"", "a"),
        ("\"abc\"", "abc"),
        ("\"\\\"\"", "\\\""),
        ("\"\\\\\"", "\\\\"),
        ("\"\\a\"", "\\a"),
    ];
    test_input_string(&pairs);
}

#[test]
fn test_string_mnemonic() {
    let pairs = [
        ("\"\\b\"", "\\b"),
        ("\"\\t\"", "\\t"),
        ("\"\\n\"", "\\n"),
        ("\"\\r\"", "\\r"),
        ("\"\\a\"", "\\a"),
        ("\"\\B\"", "\\B"),
        ("\"\\T\"", "\\T"),
        ("\"\\N\"", "\\N"),
        ("\"\\R\"", "\\R"),
        ("\"\\A\"", "\\A"),
    ];

    test_input_string(&pairs);
}

#[test]
fn test_string_escaped_double_quote() {
    let pairs = [
        ("\"\\\"\"", "\\\"")
    ];

    test_input_string(&pairs);
}

#[test]
fn test_string_escaped_backslash() {
    let pairs = [
        ("\"\\\\\"", "\\\\")
    ];

    test_input_string(&pairs);
}

#[test]
fn test_string_multiline() {
    let pairs = [
        ("\"this is a multiline   \\   \nstring\"", "this is a multiline   \\   \nstring")
    ];
    
    test_input_string(&pairs);
}

// #[test]
// fn test_string_hex_escape() {
//     let pairs = [
//         ("\"\\x41;\"", "a"),
//     ];

//     test_input_string(&pairs);
// }
