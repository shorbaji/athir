mod tests {
    use crate::lexer::Token;
    use logos::Logos;

    fn test_input<T>(input: &str, expected: T)
    where
        T: IntoIterator<Item = Token>,
    {
        let mut lex = Token::lexer(input);

        for token in expected {
            assert_eq!(lex.next(), Some(token));
        }
    }

    #[test]
    fn test_boolean() {
<<<<<<< HEAD
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
=======
        [("#t", true),
        ("#f", false),
        ("#true", true),
        ("#false", false),
        ("#T", true),
        ("#F", false),
        ("#True", true),
        ("#False", false)]
>>>>>>> dd16197 (Token values (#3))
        .iter()
        .for_each(|(s, b)| test_input(s, vec![Token::Boolean(*b)]));
    }

    #[test]
    fn test_string_to_char() {
        [
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
        ]
        .iter()
        .for_each(|(s, c)| {
            test_input(s, vec![Token::Character(*c)]);
        });
    }
    #[test]
    fn test_char_simple() {
<<<<<<< HEAD
        [("#\\a", 'a'), ("#\\\\", '\\'), ("#\\\"", '\"')]
            .iter()
            .for_each(|(s, c)| test_input(s, vec![Token::Character(*c)]));
=======
        [
            ("#\\a", 'a'), 
            ("#\\\\", '\\'),
            ("#\\\"", '\"')
        ]
        .iter()
        .for_each(|(s, c)| test_input(s, vec![Token::Character(*c)]));
>>>>>>> dd16197 (Token values (#3))
    }

    #[test]
    fn test_char_named() {
        [
            ("#\\alarm", '\u{0007}'),
            ("#\\backspace", '\u{0008}'),
            ("#\\delete", '\u{007F}'),
            ("#\\escape", '\u{001B}'),
            ("#\\newline", '\u{000A}'),
            ("#\\null", '\u{0000}'),
            ("#\\return", '\u{000D}'),
            ("#\\space", '\u{0020}'),
            ("#\\tab", '\u{0009}'),
        ]
        .iter()
        .for_each(|(s, c)| test_input(s, vec![Token::Character(*c)]));
    }

    #[test]
    fn test_char_hex() {
<<<<<<< HEAD
        [("#\\x41", 'A'), ("#\\x6a", 'j'), ("#\\x263a", '☺')]
            .iter()
            .for_each(|(s, c)| test_input(s, vec![Token::Character(*c)]));
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
            ".lmn",
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
        .for_each(|s| test_input(s, vec![Token::Number(String::from(*s))]));
    }

    #[test]
    fn test_paren_open() {
        test_input("(", vec![Token::ParenOpen]);
    }

    #[test]
    fn test_paren_close() {
        test_input(")", vec![Token::ParenClose]);
    }

    #[test]
    fn test_sharp_open() {
        test_input("#(", vec![Token::SharpOpen]);
    }

    #[test]
    fn test_sharp_u8_open() {
        test_input("#u8(", vec![Token::SharpU8Open]);
    }

    #[test]
    fn test_string_simple() {
        [
            "\"\"", "\"a\"", "\"abc\"", "\"\\\"\"", "\"\\\\\"", "\"\\a\"", "\"\\b\"", "\"\\t\"",
            "\"\\n\"", "\"\\r\"", "\"\\v\"", "\"\\f\"",
        ]
        .iter()
        .for_each(|s| test_input(s, vec![Token::String(String::from(&s[1..s.len() - 1]))]));
    }

    #[test]
    fn test_string_mnemonic() {
        [
            "\"\\b\"", "\"\\t\"", "\"\\n\"", "\"\\r\"", "\"\\a\"", "\"\\B\"", "\"\\T\"", "\"\\N\"",
            "\"\\R\"", "\"\\A\"",
        ]
        .iter()
        .for_each(|s| {
            println!("{}", s);
            test_input(s, vec![Token::String(String::from(&s[1..s.len() - 1]))]);
=======
        [
            ("#\\x41", 'A'),
            ("#\\x6a", 'j'),
            ("#\\x263a", '☺'),
        ]
        .iter()
        .for_each(|(s, c)| test_input(s, vec![Token::Character(*c)]));
    }

    #[test]
    fn test_comma() { test_input(",", vec![Token::Comma]); }

    #[test]
    fn test_comma_at() { test_input(",@", vec![Token::CommaAt]); }

    #[test]
    fn test_dot() { test_input(".", vec![Token::Dot]); }

    #[test]
    fn test_identifier() {
        ["a", "cc", "c0", "d+", "e-", "f.",
        "g@", "|h|", "+", "+j", "--", "+@",
        "+.k", "+..", ".lmn"]
        .iter()
        .for_each(|s| test_input(s, vec![Token::Identifier(s.to_string())]));
    }

    #[test]
    fn test_number() {
        ["0", "#b1", "#o#e23", "#x#i45", "#d6@7", "#b101+111i",
        "#o#e23-45i", "#xabc-i", "#d#e+i", "-inf.0", "+NAN.0", "#b-nan.0i",
        "+i", "-i", "1.234e567"]
        .iter()
        .for_each(|s| test_input(s, vec![Token::Number(String::from(*s))]));
    }

    #[test]
    fn test_paren_open() { test_input("(", vec![Token::ParenOpen]); }

    #[test]
    fn test_paren_close() { test_input(")", vec![Token::ParenClose]); }

    #[test]
    fn test_sharp_open() { test_input("#(", vec![Token::SharpOpen]); }

    #[test]
    fn test_sharp_u8_open() { test_input("#u8(", vec![Token::SharpU8Open]); }

    #[test]
    fn test_string_simple() {
        ["\"\"", "\"a\"", "\"abc\"", "\"\\\"\"", "\"\\\\\"", "\"\\a\"", "\"\\b\"", "\"\\t\"", "\"\\n\"", "\"\\r\"", "\"\\v\"", "\"\\f\""]
        .iter()
        .for_each(|s| test_input(s, vec![Token::String(String::from(&s[1..s.len()-1]))]));
    }

    #[test]
    fn test_string_mnemonic() {
        ["\"\\b\"", "\"\\t\"", "\"\\n\"", "\"\\r\"", "\"\\a\"", "\"\\B\"", "\"\\T\"", "\"\\N\"", "\"\\R\"", "\"\\A\"",]
        .iter()
        .for_each(|s| {
            println!("{}", s); test_input(s, vec![Token::String(String::from(&s[1..s.len()-1]))]);
>>>>>>> dd16197 (Token values (#3))
        });
    }

    #[test]
    fn test_string_escaped_double_quote() {
        ["\"\\\"\""]
<<<<<<< HEAD
            .iter()
            .for_each(|s| test_input(s, vec![Token::String(String::from(&s[1..s.len() - 1]))]));
=======
        .iter()
        .for_each(|s| test_input(s, vec![Token::String(String::from(&s[1..s.len()-1]))]));
>>>>>>> dd16197 (Token values (#3))
    }

    #[test]
    fn test_string_escaped_backslash() {
        ["\"\\\\\""]
<<<<<<< HEAD
            .iter()
            .for_each(|s| test_input(s, vec![Token::String(String::from(&s[1..s.len() - 1]))]));
=======
        .iter()
        .for_each(|s| test_input(s, vec![Token::String(String::from(&s[1..s.len()-1]))]));
>>>>>>> dd16197 (Token values (#3))
    }

    #[test]
    fn test_string_multiline() {
        ["\"this is a multiline   \\   \nstring\""]
<<<<<<< HEAD
            .iter()
            .for_each(|s| test_input(s, vec![Token::String(String::from(&s[1..s.len() - 1]))]));
=======
        .iter()
        .for_each(|s| test_input(s, vec![Token::String(String::from(&s[1..s.len()-1]))]));
>>>>>>> dd16197 (Token values (#3))
    }

    #[test]
    fn test_string_hex_escape() {
<<<<<<< HEAD
        ["\"\\x41;\""]
            .iter()
            .for_each(|s| test_input(s, vec![Token::String(String::from(&s[1..s.len() - 1]))]));
=======
        ["\"\\x41;\"",]
        .iter()
        .for_each(|s| test_input(s, vec![Token::String(String::from(&s[1..s.len()-1]))]));
>>>>>>> dd16197 (Token values (#3))
    }
}
