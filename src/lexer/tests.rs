
mod tests {
    use crate::*;

    fn test_input<T> (input: &str, expected: T) 
    where T : IntoIterator<Item = Token>
    {
        let mut lex = Token::lexer(input);

        for token in expected {
            assert_eq!(lex.next(), Some(token));
        }
    }

    #[test]
    fn boolean() {
        ["#t", "#true", "#T", "#TRUE", "#True", "#f", "#false", "#F", "#FALSE", "#False"]
        .iter()
        .for_each(|s| test_input(s, vec![Token::Boolean]));
    }

    #[test]
    fn char_simple() {
        ["#\\\\a", "#\\\\", "#\\\""]
        .iter()
        .for_each(|s| test_input(s, vec![Token::Character]));
    }

    #[test]
    fn char_named() {
        ["#\\alarm", "#\\backspace", "#\\delete", "#\\escape", "#\\newline", "#\\null", "#\\return", "#\\space", "#\\tab"]
        .iter()
        .for_each(|s| test_input(s, vec![Token::Character]));
    }

    #[test]
    fn char_hex() {
        ["#\\x41", "#\\x4a", "#\\x4f", "#\\x5a", "#\\x61", "#\\x6a", "#\\x6f", "#\\x7a"]
        .iter()
        .for_each(|s| test_input(s, vec![Token::Character]));
    }

    #[test]
    fn comma() { test_input(",", vec![Token::Comma]); }

    #[test]
    fn comma_at() { test_input(",@", vec![Token::CommaAt]); }

    #[test]
    fn dot() { test_input(".", vec![Token::Dot]); }

    #[test]
    fn identifier() {
        ["a", "cc", "c0", "d+", "e-", "f.",
        "g@", "|h|", "+", "+j", "--", "+@",
        "+.k", "+..", ".lmn"]
        .iter()
        .for_each(|s| test_input(s, vec![Token::Identifier]));
    }

    #[test]
    fn number() {
        ["0", "#b1", "#o#e23", "#x#i45", "#d6@7", "#b101+111i",
        "#o#e23-45i", "#xabc-i", "#d#e+i", "-inf.0", "+NAN.0", "#b-nan.0i",
        "+i", "-i", "1.234e567"]
        .iter()
        .for_each(|s| test_input(s, vec![Token::Number]));
    }
    #[test]
    fn paren_open() { test_input("(", vec![Token::ParenOpen]); }

    #[test]
    fn paren_close() { test_input(")", vec![Token::ParenClose]); }

    #[test]
    fn sharp_open() { test_input("#(", vec![Token::SharpOpen]); }

    #[test]
    fn sharp_u8_open() { test_input("#u8(", vec![Token::SharpU8Open]); }

    #[test]
    fn string_simple() {
        ["\"\"", "\"a\"", "\"abc\"", "\"\\\"\"", "\"\\\\\"", "\"\\a\"", "\"\\b\"", "\"\\t\"", "\"\\n\"", "\"\\r\"", "\"\\v\"", "\"\\f\""]
        .iter()
        .for_each(|s| test_input(s, vec![Token::String]));
    }

    #[test]
    fn string_mnemonic() {
        ["\"\\b\"", "\"\\t\"", "\"\\n\"", "\"\\r\"", "\"\\a\"", "\"\\B\"", "\"\\T\"", "\"\\N\"", "\"\\R\"", "\"\\A\"",]
        .iter()
        .for_each(|s| {
            println!("{}", s); test_input(s, vec![Token::String]);
        });
    }

    #[test]
    fn string_escaped_double_quote() {
        ["\"\\\"\""]
        .iter()
        .for_each(|s| test_input(s, vec![Token::String]));
    }

    #[test]
    fn string_escaped_backslash() {
        ["\"\\\\\""]
        .iter()
        .for_each(|s| test_input(s, vec![Token::String]));
    }

    #[test]
    fn string_multiline() {
        ["\"this is a multiline   \\   \nstring\""]
        .iter()
        .for_each(|s| test_input(s, vec![Token::String]));
    }

    #[test]
    fn string_hex_escape() {
        ["\"\\x41;\"",]
        .iter()
        .for_each(|s| test_input(s, vec![Token::String]));
    }

}