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
        "+1701171446e+921634236-4250273056/3012714821i",
        "#o+inf.0",
        "#o21226303154+i",
        "#x+d3a01c54-2a4cc230/28ccecebi",
        "#e-nan.0",
        "#e-inf.0@4013825563",
        "#e#x-nan.0",
        "1919244375+i",
        "#i-nan.0-i",
        "#e+nan.0",
        "#o6051715044/5357764115-i",
        "#i#o+6374127047@-7247137232",
        "#o10664044726+21321140214i",
        "#b+inf.0+110001010110011110000110001i",
        "#e#b+inf.0@+10111111100110001000101111101101/11000110110001011111010111001010",
        "#i#d+nan.0i",
        "#i#b-inf.0+i",
        "#e#d-3211969682/1645814528-i",
        "+1603330475.e+346875357i",
        "#d+nan.0+1019426922/2896211551i",
        "#e#b-inf.0@-inf.0",
        "#d-790776998.e+533389569@-nan.0",
        "#i4127936173/3462113485",
        "#i#x-321d5eca/37ae8ef1i",
        "#i-2988293338/4114651602-1393014941i",
        "+i",
        "#d+i",
        "#i#d-i",
        "#e#x-nan.0i",
        "+inf.0+i",
        "#d-nan.0",
        "#i#d1526693955-i",
        "#e-i",
        "#e#o-i",
        "#o-14370252164/24316241505-i",
        "#i#b+1010011101000101000000110111111i",
        "+i",
        "#e#o-4137264027i",
        "#i#b-nan.0",
        "#i#x-1c6665e/27c5896d+i",
        "#e#o-10566050161/6567704257i",
        "#o10605753007/16377675656+10555306676i",
        "#x+inf.0i",
        "#e#o+inf.0@-13313536227",
        "#x-inf.0i",
        "#x52f39791/9b1676b1-75d0b6d5/4007d855i",
        "#i#b+11111100010101010100010110000101@-inf.0",
        "#o+10143610067/26416242647i",
        "#b+10010100110000101011111001111111+i",
        "#i+3946213189.457904101e+2988894423i",
        "#i#x-4ed19b21@baaeea0f/72bc4996",
        "#i#d3822203468e+3960166424@2712719761",
        "#i+inf.0+i",
        "#x-931e8cde@-inf.0",
        "#o+4351305240i",
        "#i+1104443251/937093398i",
        "#x2c17d34d/196f3e41+898bd44e/d8954c81i",
        "#x+a9c8f8ddi",
        "#e#o-i",
        "#e#o+14014470077i",
        "#i#x50495cd3/8b913076@-30b00aa3/fbca57c7",
        "#xcb8c5ab3+92de2184i",
        "#i#x-213c9e31/2ce973e5",
        "#i#d1079077688.e-1027801348+i",
        "#e#d+nan.0i",
        "#i#o-nan.0i",
        "#e#d+1171339645i",
        "#e#b-nan.0@+nan.0",
        "#i#b+11100000010110110011100101000001i",
        "#i#b-inf.0",
        "#e#d-nan.0-i",
        "#i#d-2299951191.e-873163313i",
        "#b-10001000011011101110001111000100/1011010010011111100111111100000-1000011110110110001000111101010i",
        "#d3974708279@+3391762224/2020668763",
        "#e#b111101001001010010011100110110-1101010101011101001110101111001i",
        "#e#d+i",
        "#e+inf.0",
        "#i-inf.0i",
        "#e#d+nan.0i",
        "+3027489698e-1371805969-i",
        "#b+i",
        "#o-inf.0i",
        "#i#b-i",
        "#e#b-nan.0i",
        "#x+nan.0-bdb6cc9i",
        "-3064663300/1685711659+i",
        "#i#x+4f09fb41/481dac04i",
        "#i#d+inf.0+i",
        "#e#b+inf.0i",
        "#e#d+inf.0i",
        "#e-1978293278i",
        "#o-i",
        "#i#o-2740073577/4102246766i",
        "#e#d+nan.0-2952112347/3436078223i",
        "#i#d-inf.0@+nan.0",
        "#i#o-nan.0i",
        "#d-2082793408-i",
        "#b-inf.0i",
        "#e#d1646958015.e-4291903612@1653073321/1887370896",
        "#o+inf.0+i",        
    ]
    .iter()
    .for_each(|s| {
        let mut lex = TestLexer::new(s.to_string());

        println!("Testing: {}", s);

        assert!(matches!(lex.get_next_token(), Some(Token::Number(_))))
    });
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
