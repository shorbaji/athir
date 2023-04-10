//! Regex generator
//! 
//! This tool generates regexes to be used by the lexical analyzer
//! The generated regexs are based on R7RS Scheme's lexical syntax
//! 
#![allow(dead_code)]

mod regex_generator {
    macro_rules! number_regex {
        ($digit:expr, $radix:expr, $is_decimal:expr) => {{
            let explicit_sign = r"(\+|-)";
            let i = "(i|I)";
            let exactness = "((#[eEiI])?)";
            let sign = r"((\+|-)?)";
            let infnan = r"(\+inf\.0|-inf\.0|\+nan\.0|-nan\.0|\+INF\.0|-INF\.0|\+NAN\.0|-NAN\.0)";

            let prefix = format!("(({}{})|({}{}))", $radix, exactness, exactness, $radix);
            let uinteger = format!("(({})+)", $digit);
            let mut ureal = format!("(({}/{})|{})", uinteger, uinteger, uinteger);
            if $is_decimal {
                let exponent_marker = "(e|E)";
                let suffix = format!(r"(({}{}({}+))?)", exponent_marker, sign, $digit);
                let decimal10a = format!("({}{})", uinteger, suffix);
                let decimal10b = format!(r"(\.{}+{})", $digit, suffix);
                let decimal10c = format!(r"({}+\.{}*{})", $digit, $digit, suffix);
                let decimal10 = format!("({}|{}|{})", decimal10a, decimal10b, decimal10c);
                ureal = format!("({}|{})", ureal, decimal10);
            }

            let real = format!("(({}{})|{})", sign, ureal, infnan);
            let complexa = format!(r"({})", real);
            let complexb = format!(r"({}@{})", real, real);
            let complexc = format!(r"({}{}{}{})", real, explicit_sign, ureal, i);
            let complexd = format!(r"({}{}{})", real, explicit_sign, i);
            let complexe = format!(r"({}{}{})", real, infnan, i);
            let complexf = format!(r"({}{}{})", explicit_sign, ureal, i);
            let complexg = format!(r"({}{})", infnan, i);
            let complexh = format!(r"({}{})", explicit_sign, i);

            let complex = format!(
                r"({}|{}|{}|{}|{}|{}|{}|{})",
                complexc, complexe, complexd, complexb, complexf, complexg, complexa, complexh,
            );
            let num = format!("({}{})", prefix, complex);
            num
        }};
    }

    pub fn gen_regex() {
        let explicit_sign = r"(\+|-)";

        let num2 = number_regex!("(0|1)", "(#b)", false);
        let num8 = number_regex!("[0-7]", "(#o)", false);
        let num10 = number_regex!("([0-9])", "((#d)?)", true);
        let num16 = number_regex!("([0-9a-fA-F])", "(#x)", false);
        let number = format!("({}|{}|{}|{})", num2, num8, num10, num16);

        let digit = r"[0-9]".to_string();
        let digit16 = r"[0-9a-fA-F]";
        let line_ending = r"(\r\n|\r|\n)";
        let intraline_whitespace = r"( |\t)";
        let whitespace = format!("({}|{})", intraline_whitespace, line_ending);
        let vertical_line = r"\|";
        // let delimiter = format!("({}|{}|(|)|\")", whitespace, vertical_line);
        let letter = "[a-zA-Z]";
        let special_initial = r"[!\$%&\*/:<=>\?\^_~]";
        let initial = format!("({}|{})", letter, special_initial);
        let special_subsequent = format!(r"({}|\.|@)", explicit_sign);
        let subsequent = format!("({}|{}|{})", initial, digit, special_subsequent);
        let hex_scalar_value = format!("({}+)", digit16);
        let inline_hex_escape = format!(r"(\\x{};)", hex_scalar_value);
        let mnemonic_escape = r"(\\[aA]|\\[bB]|\\[tT]|\\[nN]|\\[rR])";

        let sign_subsequent = format!(r"({}|{}|@)", initial, explicit_sign);
        let dot_subsequent = format!(r"({}|\.)", sign_subsequent);
        let peculiara = format!("{}", explicit_sign);
        let peculiarb = format!(r"({}{}{}*)", explicit_sign, sign_subsequent, subsequent);
        let peculiarc = format!(r"({}\.{}{}*)", explicit_sign, dot_subsequent, subsequent);
        let peculiard = format!(r"(\.{}{}*)", dot_subsequent, subsequent);
        let peculiar = format!("({}|{}|{}|{})", peculiara, peculiarb, peculiarc, peculiard);

        let symbol_element = format!(
            r"([^\|\\]|{}|{}|(\\\|))",
            inline_hex_escape, mnemonic_escape
        );

        let identifiera = format!("({}{}*)", initial, subsequent);
        let identifierb = format!("({}{}*{})", vertical_line, symbol_element, vertical_line);
        let identifierc = format!("({})", peculiar);

        let identifier = format!("(({})|({})|({}))", identifiera, identifierc, identifierb);

        let character_name = "(alarm|backspace|delete|escape|newline|null|return|space|tab)";
        let character = format!(
            r"((#\\x{})|(#\\{})|(#\\.))",
            hex_scalar_value, character_name
        );

        let string_element_a = "[^\"\\\\]";
        let string_element_b = mnemonic_escape.clone();
        let string_element_c = "\\\\\"";
        let string_element_d = "\\\\";
        let string_element_e = format!(
            "\\\\{}*{}{}*",
            intraline_whitespace, line_ending, intraline_whitespace
        );
        let string_element_f = inline_hex_escape.clone();

        let string_element = format!(
            "({}|{}|{}|{}|{}|{})",
            string_element_a,
            string_element_b,
            string_element_c,
            string_element_d,
            string_element_e,
            string_element_f
        );

        let string = format!("\"{}*\"", string_element);

        let comment = format!("(;[^{}]*)", line_ending);
        let directive = r"(#!fold-case)|(#!no-fold-case)";
        // let atmosphere = format!("({}|{}|{})", whitespace, comment, directive); // need to add comments
        // let intertoken_space = format!("({}*)", atmosphere);

        let boolean_letter_t = "[tT]";
        let boolean_letter_r = "[rR]";
        let boolean_letter_u = "[uU]";
        let boolean_letter_e = "[eE]";

        let boolean_letter_f = "[fF]";
        let boolean_letter_a = "[aA]";
        let boolean_letter_l = "[lL]";
        let boolean_letter_s = "[sS]";

        let boolean_short = format!("({}|{})", boolean_letter_t, boolean_letter_f);
        let boolean_long_true = format!(
            "({}{}{}{})",
            boolean_letter_t, boolean_letter_r, boolean_letter_u, boolean_letter_e
        );
        let boolean_long_false = format!(
            "({}{}{}{}{})",
            boolean_letter_f,
            boolean_letter_a,
            boolean_letter_l,
            boolean_letter_s,
            boolean_letter_e
        );

        let boolean = format!(
            "(#({}|{}|{}))",
            boolean_long_true, boolean_long_false, boolean_short
        );

        println!("boolean:\n{}\n", boolean);
        println!("character:\n{}\n", character);
        println!("identifier:\n{}\n", identifier);
        // println!("intertoken space:\n{}\n", intertoken_space);
        println!("whitespace:\n{}\n", whitespace);
        println!("comment:\n{}\n", comment);
        println!("directive:\n{}\n", directive);
        println!("number:\n{}\n", number);
        println!("string:\n{}\n", string);
    }
}

fn main() {
    regex_generator::gen_regex();
}
