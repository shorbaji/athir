use super::randomizable::Randomizable;

use rand::Rng;
use std::fmt::{self, Display, Formatter};

fn random_sequence<T>() -> Vec<T>
where
    T: Randomizable,
{
    let n = rand::random::<u8>() % 10;
    let mut v = Vec::new();
    for _ in 0..n {
        v.push(T::random());
    }
    v
}

#[derive(Debug, Clone)]
enum Identifier {
    Basic(Initial, Vec<Subsequent>),
    Vertical(Vec<SymbolElement>),
    Peculiar(Peculiar),
}

impl Randomizable for Identifier {
    fn random() -> Self {
        match rand::random::<u8>() % 3 {
            0 => Identifier::Basic(Initial::random(), random_sequence()),
            1 => Identifier::Vertical(vec![SymbolElement::random()]),
            _ => Identifier::Peculiar(Peculiar::random()),
        }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Identifier::Basic(initial, subsequent) => {
                write!(f, "{initial}")?;
                for s in subsequent {
                    write!(f, "{s}")?;
                }
                Ok(())
            }
            Identifier::Vertical(symbol_element) => {
                write!(f, "|")?;
                for s in symbol_element {
                    write!(f, "{s}")?;
                }
                write!(f, "|")
            }
            Identifier::Peculiar(peculiar) => {
                write!(f, "{peculiar}")
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Initial {
    Letter(Letter),
    SpecialInitial(SpecialInitial),
}

impl Randomizable for Initial {
    fn random() -> Self {
        match rand::random::<u8>() % 2 {
            0 => Initial::Letter(Letter::random()),
            _ => Initial::SpecialInitial(SpecialInitial::random()),
        }
    }
}

impl Display for Initial {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Initial::Letter(letter) => write!(f, "{letter}"),
            Initial::SpecialInitial(special_initial) => write!(f, "{special_initial}"),
        }
    }
}

#[derive(Debug, Clone, Copy)]

struct Letter {
    char: char,
}

impl Randomizable for Letter {
    fn random() -> Self {
        let mut rng = rand::thread_rng();
        let n: u8 = rng.gen_range(0..26);
        Self {
            char: (n + 97) as char,
        }
    }
}

impl Display for Letter {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.char)
    }
}

#[derive(Debug, Clone, Copy)]
struct SpecialInitial {
    char: char,
}

impl Randomizable for SpecialInitial {
    fn random() -> Self {
        let c = match rand::random::<u8>() % 14 {
            0 => '!',
            1 => '$',
            2 => '%',
            3 => '&',
            4 => '*',
            5 => '/',
            6 => ':',
            7 => '<',
            8 => '=',
            9 => '>',
            10 => '?',
            11 => '^',
            12 => '_',
            13 => '~',
            _ => unreachable!(),
        };

        Self { char: c }
    }
}

impl Display for SpecialInitial {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.char)
    }
}

#[derive(Debug, Clone, Copy)]
enum Subsequent {
    Initial(Initial),
    Digit(Digit),
    SpecialSubsequent(SpecialSubsequent),
}

impl Randomizable for Subsequent {
    fn random() -> Self {
        match rand::random::<u8>() % 3 {
            0 => Subsequent::Initial(Initial::random()),
            1 => Subsequent::Digit(Digit::random()),
            _ => Subsequent::SpecialSubsequent(SpecialSubsequent::random()),
        }
    }
}

impl Display for Subsequent {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Subsequent::Initial(initial) => write!(f, "{initial}"),
            Subsequent::Digit(digit) => write!(f, "{digit}"),
            Subsequent::SpecialSubsequent(special_subsequent) => {
                write!(f, "{special_subsequent}")
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Digit {
    char: char,
}

impl Randomizable for Digit {
    fn random() -> Self {
        let mut rng = rand::thread_rng();
        let n = rng.gen_range(0..10);
        Self {
            char: (n + 48_u8) as char,
        }
    }
}

impl Display for Digit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.char)
    }
}

#[derive(Debug, Clone, Copy)]
struct HexDigit {
    char: char,
}

impl Randomizable for HexDigit {
    fn random() -> Self {
        let mut rng = rand::thread_rng();
        let n: u8 = rng.gen_range(0..16);
        match n {
            0..=9 => Self {
                char: (n + 48) as char,
            },
            _ => Self {
                char: (n + 87) as char,
            },
        }
    }
}

impl Display for HexDigit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.char)
    }
}

#[derive(Debug, Clone, Copy)]
struct ExplicitSign {
    char: char,
}

impl Randomizable for ExplicitSign {
    fn random() -> Self {
        match rand::random::<u8>() % 2 {
            0 => Self { char: '+' },
            _ => Self { char: '-' },
        }
    }
}

impl Display for ExplicitSign {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.char)
    }
}

#[derive(Debug, Clone, Copy)]
enum SpecialSubsequent {
    ExplicitSign(ExplicitSign),
    Dot,
    At,
}

impl Randomizable for SpecialSubsequent {
    fn random() -> Self {
        match rand::random::<u8>() % 3 {
            0 => SpecialSubsequent::ExplicitSign(ExplicitSign::random()),
            1 => SpecialSubsequent::Dot,
            _ => SpecialSubsequent::At,
        }
    }
}

impl Display for SpecialSubsequent {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            SpecialSubsequent::ExplicitSign(explicit_sign) => write!(f, "{explicit_sign}"),
            SpecialSubsequent::Dot => write!(f, "."),
            SpecialSubsequent::At => write!(f, "@"),
        }
    }
}

#[derive(Debug, Clone)]
struct InlineHexEscape {
    x: HexDigit,
    y: Vec<HexDigit>,
}

impl Randomizable for InlineHexEscape {
    fn random() -> Self {
        Self {
            x: HexDigit::random(),
            y: random_sequence(),
        }
    }
}

impl Display for InlineHexEscape {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "#\\x{}{};",
            self.x,
            self.y.iter().map(|x| format!("{x}")).collect::<String>()
        )
    }
}

#[derive(Debug, Clone, Copy)]
enum MnemonicEscape {
    A,
    B,
    T,
    N,
    R,
}

impl Randomizable for MnemonicEscape {
    fn random() -> Self {
        match rand::random::<u8>() % 5 {
            0 => MnemonicEscape::A,
            1 => MnemonicEscape::B,
            2 => MnemonicEscape::T,
            3 => MnemonicEscape::N,
            _ => MnemonicEscape::R,
        }
    }
}

impl Display for MnemonicEscape {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            MnemonicEscape::A => write!(f, "#\\a"),
            MnemonicEscape::B => write!(f, "#\\b"),
            MnemonicEscape::T => write!(f, "#\\t"),
            MnemonicEscape::N => write!(f, "#\\n"),
            MnemonicEscape::R => write!(f, "#\\r"),
        }
    }
}

#[derive(Debug, Clone)]
enum Peculiar {
    ExplicitSign(ExplicitSign),
    A(ExplicitSign, SignSubsequent, Vec<Subsequent>),
    B(ExplicitSign, DotSubsequent, Vec<Subsequent>),
    C(DotSubsequent, Vec<Subsequent>),
}

impl Randomizable for Peculiar {
    fn random() -> Self {
        match rand::random::<u8>() % 4 {
            0 => Peculiar::ExplicitSign(ExplicitSign::random()),
            1 => loop {
                let sign_subsequent = SignSubsequent::random();
                let subsequents: Vec<Subsequent> = random_sequence();

                let c = format!("{sign_subsequent}").chars().next().unwrap();
                if matches!(c, 'i' | 'I') && subsequents.is_empty() {
                    continue;
                } else {
                    return Peculiar::A(ExplicitSign::random(), sign_subsequent, subsequents);
                }
            },
            2 => Peculiar::B(
                ExplicitSign::random(),
                DotSubsequent::random(),
                random_sequence(),
            ),
            _ => Peculiar::C(DotSubsequent::random(), random_sequence()),
        }
    }
}

impl Display for Peculiar {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Peculiar::ExplicitSign(explicit_sign) => write!(f, "{explicit_sign}"),
            Peculiar::A(explicit_sign, sign_subsequent, subsequents) => write!(
                f,
                "{}{}{}",
                explicit_sign,
                sign_subsequent,
                subsequents
                    .iter()
                    .map(|x| format!("{x}"))
                    .collect::<String>()
            ),
            Peculiar::B(explicit_sign, dot_subsequent, subsequents) => write!(
                f,
                "{}.{}{}",
                explicit_sign,
                dot_subsequent,
                subsequents
                    .iter()
                    .map(|x| format!("{x}"))
                    .collect::<String>()
            ),
            Peculiar::C(dot_subsequent, subsequents) => write!(
                f,
                ".{}{}",
                dot_subsequent,
                subsequents
                    .iter()
                    .map(|x| format!("{x}"))
                    .collect::<String>()
            ),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum DotSubsequent {
    SignSubsequent(SignSubsequent),
    Dot,
}

impl Randomizable for DotSubsequent {
    fn random() -> Self {
        match rand::random::<u8>() % 2 {
            0 => DotSubsequent::SignSubsequent(SignSubsequent::random()),
            _ => DotSubsequent::Dot,
        }
    }
}

impl Display for DotSubsequent {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            DotSubsequent::SignSubsequent(sign_subsequent) => write!(f, "{sign_subsequent}"),
            DotSubsequent::Dot => write!(f, "."),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum SignSubsequent {
    Initial(Initial),
    ExplicitSign(ExplicitSign),
    At,
}

impl Randomizable for SignSubsequent {
    fn random() -> Self {
        match rand::random::<u8>() % 3 {
            0 => SignSubsequent::Initial(Initial::random()),
            1 => SignSubsequent::ExplicitSign(ExplicitSign::random()),
            _ => SignSubsequent::At,
        }
    }
}

impl Display for SignSubsequent {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            SignSubsequent::Initial(initial) => write!(f, "{initial}"),
            SignSubsequent::ExplicitSign(explicit_sign) => write!(f, "{explicit_sign}"),
            SignSubsequent::At => write!(f, "@"),
        }
    }
}

#[derive(Debug, Clone)]
enum SymbolElement {
    ExceptBackSlashOrVertical(ExceptBackSlashOrVertical),
    InlineHexEscape(InlineHexEscape),
    MnemonicEscape(MnemonicEscape),
    SlashVertical,
}

impl Randomizable for SymbolElement {
    fn random() -> Self {
        match rand::random::<u8>() % 4 {
            0 => SymbolElement::ExceptBackSlashOrVertical(ExceptBackSlashOrVertical::random()),
            1 => SymbolElement::InlineHexEscape(InlineHexEscape::random()),
            2 => SymbolElement::MnemonicEscape(MnemonicEscape::random()),
            _ => SymbolElement::SlashVertical,
        }
    }
}

impl Display for SymbolElement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            SymbolElement::ExceptBackSlashOrVertical(except_back_slash_or_vertical) => {
                write!(f, "{except_back_slash_or_vertical}")
            }
            SymbolElement::InlineHexEscape(inline_hex_escape) => write!(f, "{inline_hex_escape}"),
            SymbolElement::MnemonicEscape(mnemonic_escape) => write!(f, "{mnemonic_escape}"),
            SymbolElement::SlashVertical => write!(f, "\\|"),
        }
    }
}

#[derive(Debug, Clone)]
struct ExceptBackSlashOrVertical {
    char: char,
}

impl Randomizable for ExceptBackSlashOrVertical {
    fn random() -> Self {
        loop {
            let c = rand::random::<u8>() as char;
            if (c != '\\') && (c != '|') {
                return Self { char: c };
            }
        }
    }
}

impl Display for ExceptBackSlashOrVertical {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.char)
    }
}

pub fn random() -> String {
    format!("{}", Identifier::random())
}
