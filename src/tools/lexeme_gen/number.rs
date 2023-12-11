use super::randomizable::Randomizable;

use std::fmt;

#[derive(Debug)]
enum Sign {
    Minus,
    Plus,
    None,
}

impl Randomizable for Sign {
    fn random() -> Self {
        match rand::random::<u8>() % 3 {
            0 => Sign::Minus,
            1 => Sign::Plus,
            2 => Sign::None,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Sign {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Sign::Minus => write!(f, "-"),
            Sign::Plus => write!(f, "+"),
            Sign::None => Ok(()),
        }
    }
}

#[derive(Debug)]
enum ExplicitSign {
    Minus,
    Plus,
}

impl Randomizable for ExplicitSign {
    fn random() -> ExplicitSign {
        match rand::random() {
            true => ExplicitSign::Minus,
            false => ExplicitSign::Plus,
        }
    }
}

impl fmt::Display for ExplicitSign {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExplicitSign::Minus => write!(f, "-"),
            ExplicitSign::Plus => write!(f, "+"),
        }
    }
}

#[derive(Debug)]
struct Suffix {
    sign: ExplicitSign,
    digits: u32,
}

impl Randomizable for Suffix {
    fn random() -> Suffix {
        Suffix {
            sign: ExplicitSign::random(),
            digits: rand::random::<u32>(),
        }
    }
}

impl fmt::Display for Suffix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "e{}{}", self.sign, self.digits)
    }
}

#[derive(Debug)]
enum Infnan {
    PlusInf,
    MinusInf,
    PlusNan,
    MinusNan,
}

impl Randomizable for Infnan {
    fn random() -> Self {
        match rand::random::<u8>() % 4 {
            0 => Infnan::PlusInf,
            1 => Infnan::MinusInf,
            2 => Infnan::PlusNan,
            3 => Infnan::MinusNan,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Infnan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Infnan::PlusInf => write!(f, "+inf.0"),
            Infnan::MinusInf => write!(f, "-inf.0"),
            Infnan::PlusNan => write!(f, "+nan.0"),
            Infnan::MinusNan => write!(f, "-nan.0"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Radix {
    Binary,
    Octal,
    Decimal,
    Hexadecimal,
    None,
}

impl Randomizable for Radix {
    fn random() -> Self {
        match rand::random::<u8>() % 5 {
            0 => Radix::Binary,
            1 => Radix::Octal,
            2 => Radix::Decimal,
            3 => Radix::Hexadecimal,
            4 => Radix::None,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Radix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Radix::Binary => write!(f, "#b"),
            Radix::Octal => write!(f, "#o"),
            Radix::Decimal => write!(f, "#d"),
            Radix::Hexadecimal => write!(f, "#x"),
            Radix::None => Ok(()),
        }
    }
}

#[derive(Debug)]
enum Exactness {
    Inexact,
    Exact,
    None,
}

impl Randomizable for Exactness {
    fn random() -> Self {
        match rand::random::<u8>() % 3 {
            0 => Exactness::Inexact,
            1 => Exactness::Exact,
            2 => Exactness::None,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Exactness {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Exactness::Inexact => write!(f, "#i"),
            Exactness::Exact => write!(f, "#e"),
            Exactness::None => Ok(()),
        }
    }
}

#[derive(Debug)]
struct Prefix {
    radix: Radix,
    exactness: Exactness,
}

impl Randomizable for Prefix {
    fn random() -> Self {
        Prefix {
            radix: Radix::random(),
            exactness: Exactness::random(),
        }
    }
}

impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.exactness, self.radix)
    }
}

type Uinteger = u64;

fn uinteger_to_string(radix: &Radix, uinteger: &Uinteger) -> String {
    match radix {
        Radix::Binary => format!("{uinteger:b}"),
        Radix::Octal => format!("{uinteger:o}"),
        Radix::Decimal => format!("{uinteger}"),
        Radix::Hexadecimal => format!("{uinteger:x}"),
        Radix::None => format!("{uinteger}"),
    }
}

impl Randomizable for Uinteger {
    fn random() -> Self {
        rand::random::<u64>()
    }
}

#[derive(Debug)]
enum Decimal {
    A(Uinteger, Suffix),
    B(Uinteger, Suffix),
    C(Uinteger, Option<Uinteger>, Suffix),
}

impl Randomizable for Decimal {
    fn random() -> Self {
        match rand::random::<u8>() % 3 {
            0 => Decimal::A(Uinteger::random(), Suffix::random()),
            1 => Decimal::B(Uinteger::random(), Suffix::random()),
            2 => Decimal::C(
                Uinteger::random(),
                if rand::random() {
                    Some(Uinteger::random())
                } else {
                    None
                },
                Suffix::random(),
            ),
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Decimal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Decimal::A(uinteger, suffix) => write!(f, "{}{}", uinteger, suffix),
            Decimal::B(uinteger, suffix) => write!(f, "{}.{}", uinteger, suffix),
            Decimal::C(uinteger1, uinteger2, suffix) => match uinteger2 {
                Some(uinteger2) => write!(f, "{}.{}{}", uinteger1, uinteger2, suffix),
                None => write!(f, "{}.{}", uinteger1, suffix),
            },
        }
    }
}

#[derive(Debug)]
enum Ureal {
    Singleton(Radix, Uinteger),
    Pair(Radix, Uinteger, Uinteger),
    Decimal(Decimal),
}

impl Randomizable for Ureal {
    fn random() -> Self {
        match rand::random::<u8>() % 3 {
            0 => Ureal::Singleton(Radix::random(), Uinteger::random()),
            1 => Ureal::Pair(Radix::random(), Uinteger::random(), Uinteger::random()),
            2 => Ureal::Decimal(Decimal::random()),
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Ureal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ureal::Singleton(radix, uinteger) => {
                write!(f, "{}", uinteger_to_string(radix, uinteger))
            }
            Ureal::Pair(radix, uinteger1, uinteger2) => {
                write!(
                    f,
                    "{}/{}",
                    uinteger_to_string(radix, uinteger1),
                    uinteger_to_string(radix, uinteger2)
                )
            }
            Ureal::Decimal(decimal) => {
                write!(f, "{}", decimal)
            }
        }
    }
}

impl Ureal {
    fn random_with_radix(radix: Radix) -> Self {
        let options = match radix {
            Radix::Decimal | Radix::None => 3,
            _ => 2,
        };
        match rand::random::<u8>() % options {
            0 => Ureal::Singleton(radix, Uinteger::random()),
            1 => Ureal::Pair(radix, Uinteger::random(), Uinteger::random()),
            2 => Ureal::Decimal(Decimal::random()),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
enum Real {
    SignUreal(Sign, Ureal),
    Infnan(Infnan),
}

impl Randomizable for Real {
    fn random() -> Self {
        match rand::random::<u8>() % 2 {
            0 => Real::SignUreal(Sign::random(), Ureal::random()),
            1 => Real::Infnan(Infnan::random()),
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Real {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Real::SignUreal(sign, ureal) => {
                write!(f, "{}{}", sign, ureal)
            }
            Real::Infnan(infnan) => {
                write!(f, "{}", infnan)
            }
        }
    }
}

impl Real {
    fn random_with_radix(radix: Radix) -> Self {
        match rand::random::<u8>() % 2 {
            0 => Real::SignUreal(Sign::random(), Ureal::random_with_radix(radix)),
            1 => Real::Infnan(Infnan::random()),
            _ => unreachable!(),
        }
    }
}
#[derive(Debug)]
enum Complex {
    Singleton(Real),
    At(Real, Real),
    RealImag(Real, ExplicitSign, Ureal),
    RealI(Real, ExplicitSign),
    Imag(ExplicitSign, Ureal),
    InfnanI(Infnan),
    I(ExplicitSign),
}

impl Randomizable for Complex {
    fn random() -> Self {
        match rand::random::<u8>() % 7 {
            0 => Complex::Singleton(Real::random()),
            1 => Complex::At(Real::random(), Real::random()),
            2 => Complex::RealImag(Real::random(), ExplicitSign::random(), Ureal::random()),
            3 => Complex::RealI(Real::random(), ExplicitSign::random()),
            4 => Complex::Imag(ExplicitSign::random(), Ureal::random()),
            5 => Complex::InfnanI(Infnan::random()),
            6 => Complex::I(ExplicitSign::random()),
            _ => unreachable!(),
        }
    }
}

impl Complex {
    fn random_with_radix(radix: Radix) -> Self {
        let radix = match radix {
            Radix::Decimal | Radix::None => Radix::Decimal,
            _ => radix,
        };

        match rand::random::<u8>() % 7 {
            0 => Complex::Singleton(Real::random_with_radix(radix)),
            1 => Complex::At(
                Real::random_with_radix(radix),
                Real::random_with_radix(radix),
            ),
            2 => Complex::RealImag(
                Real::random_with_radix(radix),
                ExplicitSign::random(),
                Ureal::random_with_radix(radix),
            ),
            3 => Complex::RealI(Real::random_with_radix(radix), ExplicitSign::random()),
            4 => Complex::Imag(ExplicitSign::random(), Ureal::random_with_radix(radix)),
            5 => Complex::InfnanI(Infnan::random()),
            6 => Complex::I(ExplicitSign::random()),
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Complex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Complex::Singleton(real) => write!(f, "{}", real),
            Complex::At(real, imag) => write!(f, "{}@{}", real, imag),
            Complex::RealImag(real, sign, ureal) => write!(f, "{}{}{}i", real, sign, ureal),
            Complex::RealI(real, sign) => write!(f, "{}{}i", real, sign),
            Complex::Imag(sign, ureal) => write!(f, "{}{}i", sign, ureal),
            Complex::InfnanI(infnan) => write!(f, "{}i", infnan),
            Complex::I(sign) => write!(f, "{}i", sign),
        }
    }
}

#[derive(Debug)]
struct Number {
    prefix: Prefix,
    complex: Complex,
}

impl Randomizable for Number {
    fn random() -> Self {
        let prefix = Prefix::random();
        let radix = prefix.radix;

        Number {
            prefix,
            complex: Complex::random_with_radix(radix),
        }
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.prefix, self.complex)
    }
}

pub fn random() -> String {
    format!("{}", Number::random())
}
