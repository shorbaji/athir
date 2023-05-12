use logos::{Lexer, Logos};
use crate::value::number::{Number, real::{Real, RealValue}};

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum NonDecimalNumberToken {
    #[token("@")]
    At,

    #[token("/")]
    Backslash,

    #[regex(r"(#e|#E)")]
    Exact,

    #[token("i")]
    I,

    #[regex(r"(#i|#I)")]
    Inexact,

    #[regex(r"(\+inf\.0|\+INF\.0)")]
    InfPlus,

    #[regex(r"((-inf\.0)|(-INF\.0))")]
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

    #[token("#x")]
    Radix16,

    #[regex(r"[0-1]+", priority = 8)]
    Uinteger2,

    #[regex(r"[0-7]+", priority = 6)]
    Uinteger8,

    #[regex(r"[0-9a-fA-F]+")]
    Uinteger16,

}

pub fn parse_nondecimal(s: &str, radix: u32) -> Option<Number> {
    let mut lex = NonDecimalNumberToken::lexer(s);

    num(&mut lex, radix)
}

fn num(lex: &mut Lexer<NonDecimalNumberToken>, r: u32) -> Option<Number> {

    let token = lex.next()?;

    match token {        
        Ok(NonDecimalNumberToken::Radix2) if r == 2 => radix(lex, r),
        Ok(NonDecimalNumberToken::Radix8) if r == 8 => radix(lex, r),
        Ok(NonDecimalNumberToken::Radix16) if r == 16 => radix(lex, r),
        Ok(NonDecimalNumberToken::Exact) => exact(lex, r, true,),
        Ok(NonDecimalNumberToken::Inexact) => exact(lex, r, false),
        Ok(NonDecimalNumberToken::Plus) => sign(lex, r, true, None),
        Ok(NonDecimalNumberToken::Minus) => sign(lex, r, false, None),
        _ => None,
    }
}

fn infnan(lex: &mut Lexer<NonDecimalNumberToken>, r: u32, value: RealValue, sign: bool) -> Option<Number> {
    real(lex, r, Real { exact: true, value: value }, Some(sign))
}

fn radix(lex: &mut Lexer<NonDecimalNumberToken>, r:u32) -> Option<Number> {
    let token = lex.next()?;
    
    match token {
        Ok(NonDecimalNumberToken::Exact) => prefix(lex, r, true),
        Ok(NonDecimalNumberToken::Inexact) => prefix(lex, r, false),
        Ok(NonDecimalNumberToken::Plus) => sign(lex, r, true, None),
        Ok(NonDecimalNumberToken::Minus) => sign(lex, r, false, None),
        Ok(NonDecimalNumberToken::InfPlus) => infnan(lex, r, RealValue::Infinity, true),
        Ok(NonDecimalNumberToken::InfMinus) => infnan(lex, r, RealValue::NegInfinity, false),
        Ok(NonDecimalNumberToken::NanMinus) => infnan(lex, r, RealValue::Nan, false),
        Ok(NonDecimalNumberToken::NanPlus) => infnan(lex, r, RealValue::Nan, true),
        Ok(NonDecimalNumberToken::Uinteger2) if (r == 2) || (r == 8) || (r ==16) => uinteger(lex, r, None, None, lex.slice()),
        Ok(NonDecimalNumberToken::Uinteger8) if (r == 8) || (r == 16) => uinteger(lex, r, None, None, lex.slice()),
        Ok(NonDecimalNumberToken::Uinteger16) if r == 16 => uinteger(lex, r, None, None, lex.slice()),
        _ => None,
    }
}

fn exact(lex: &mut Lexer<NonDecimalNumberToken>, r: u32, exact: bool) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(NonDecimalNumberToken::Radix2) if r == 2 => prefix(lex, r, exact),
        Ok(NonDecimalNumberToken::Radix8) if r == 8 => prefix(lex, r, exact),
        Ok(NonDecimalNumberToken::Radix16) if r == 16 => prefix(lex, r, exact),
        Ok(NonDecimalNumberToken::InfMinus) => infnan(lex, r, RealValue::NegInfinity, false),
        Ok(NonDecimalNumberToken::InfPlus) => infnan(lex, r, RealValue::Infinity, true),
        Ok(NonDecimalNumberToken::NanMinus) => infnan(lex, r, RealValue::Nan, false),
        Ok(NonDecimalNumberToken::NanPlus) => infnan(lex, r, RealValue::Nan, true),
        _ => None,
    }
}

fn prefix(lex: &mut Lexer<NonDecimalNumberToken>, r: u32, exact: bool) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(NonDecimalNumberToken::Plus) => sign(lex, r, true, Some(exact)),
        Ok(NonDecimalNumberToken::Minus) => sign(lex, r, false, Some(exact)),
        Ok(NonDecimalNumberToken::InfPlus) => infnan(lex, r, RealValue::Infinity, true),
        Ok(NonDecimalNumberToken::InfMinus) => infnan(lex, r, RealValue::NegInfinity, false),
        Ok(NonDecimalNumberToken::NanMinus) => infnan(lex, r, RealValue::Nan, false),
        Ok(NonDecimalNumberToken::NanPlus) => infnan(lex, r, RealValue::Nan, true),
        Ok(NonDecimalNumberToken::Uinteger2) if (r == 2) || (r == 8) || (r ==16) => uinteger(lex, r, Some(exact), None, lex.slice()),
        Ok(NonDecimalNumberToken::Uinteger8) if (r == 8) || (r == 16) => uinteger(lex, r, Some(exact), None, lex.slice()),
        Ok(NonDecimalNumberToken::Uinteger16) if r == 16 => uinteger(lex, r, Some(exact), None, lex.slice()),
        _ => None,
    }
}

fn sign(lex: &mut Lexer<NonDecimalNumberToken>, r: u32, sign: bool, exact: Option<bool>) -> Option<Number> {
    println!("sign {:?}", sign);
    let token = lex.next()?;
    println!("sign token {:?}", token);

    match token {
        Ok(NonDecimalNumberToken::Uinteger2) if (r == 2) || (r == 8) || (r ==16) => uinteger(lex, r, exact, Some(sign), lex.slice()),
        Ok(NonDecimalNumberToken::Uinteger8) if (r == 8) || (r == 16) => uinteger(lex, r, exact, Some(sign), lex.slice()),
        Ok(NonDecimalNumberToken::Uinteger16) if r == 16 => uinteger(lex, r, exact, Some(sign), lex.slice()),
        Ok(NonDecimalNumberToken::I) => Some(Number::Complex { real: Real::from(0), imag: Real::from(1) }),
        _ => None,
    }
}

fn uinteger(
    lex: &mut Lexer<NonDecimalNumberToken>,
    r: u32,
    exact: Option<bool>,
    sign: Option<bool>,
    s: &str,
) -> Option<Number> {
    println!("uinteger {:?}", sign);   
    let token = lex.next();

    let int = isize::from_str_radix(s, r).unwrap() as u32;

    let real = Real {
        exact: exact.unwrap_or(true),
        value: RealValue::Integer {
            positive: sign.unwrap_or(true),
            value: int,
        },
    };

    if None == token {
        return Some(Number::Real(real));
    }

    match token? {
        Ok(NonDecimalNumberToken::Backslash) => uinteger_backslash(lex, r, sign, exact, int),
        Ok(NonDecimalNumberToken::Minus) => real_sign(lex, r, real, false),
        Ok(NonDecimalNumberToken::Plus) => real_sign(lex, r, real, true),
        Ok(NonDecimalNumberToken::InfMinus) => real_infnan(lex, real, RealValue::NegInfinity),
        Ok(NonDecimalNumberToken::InfPlus) => real_infnan(lex, real, RealValue::Infinity),
        Ok(NonDecimalNumberToken::NanMinus) => real_infnan(lex, real, RealValue::Nan),
        Ok(NonDecimalNumberToken::NanPlus) => real_infnan(lex, real, RealValue::Nan),
        Ok(NonDecimalNumberToken::At) => real_at(lex, r, real),
        Ok(NonDecimalNumberToken::I) if sign.is_some() => Some(Number::Complex { real: real, imag: Real::from(1) }),
        _ => None,
    }
}

fn uinteger_backslash(
    lex: &mut Lexer<NonDecimalNumberToken>,
    r: u32,
    sign: Option<bool>,
    exact: Option<bool>,
    value: u32,
) -> Option<Number> {
    println!("uinteger_backslash: sign: {:?}", sign);
    let token = lex.next()?;

    match token {
        Ok(NonDecimalNumberToken::Uinteger2) if (r == 2) || (r == 8) || (r ==16) => uinteger_backslash_uinteger(lex, r, exact, sign, value, lex.slice()),
        Ok(NonDecimalNumberToken::Uinteger8) if (r == 8) || (r == 16) => uinteger_backslash_uinteger(lex, r, exact, sign, value, lex.slice()),
        Ok(NonDecimalNumberToken::Uinteger16) if (r == 16) => uinteger_backslash_uinteger(lex, r, exact, sign, value, lex.slice()),
        _ => None,
    }
}

fn uinteger_backslash_uinteger(
    lex: &mut Lexer<NonDecimalNumberToken>,
    r: u32,
    exact: Option<bool>,
    sign: Option<bool>,
    value: u32,
    s: &str,
) -> Option<Number> {
    println!("uinteger_backslash_uinteger: sign: {:?}", sign);
    println!("uinteger_backslash_uinteger: s: {:?}", s);
    println!("uinteger_backslash_uinteger: value: {:?}", value);
    println!("uinteger_backslash_uinteger: r: {:?}", r);
    println!("uinteger_backslash_uinteger: exact: {:?}", exact);

    let num = value;
    let den = isize::from_str_radix(s, r).unwrap() as u32;

    let value = RealValue::Rational {
        positive: sign.unwrap_or(true),
        num: num,
        den: den,
    };

    let real = Real {
        exact: exact.unwrap_or(true),
        value: value,
    };

    let token = lex.next();

    if None == token         {
        return Some(Number::Real(real));
    }

    match token? {
        Ok(NonDecimalNumberToken::At) => real_at(lex, r, real),
        Ok(NonDecimalNumberToken::I) if sign.is_some() => Some(Number::Complex { real: Real::from(0) , imag: real }),
        Ok(NonDecimalNumberToken::Minus) => real_sign(lex, r, real, false),
        Ok(NonDecimalNumberToken::Plus) => real_sign(lex, r, real, true),
        _ => None,
    }
}

fn real(lex: &mut Lexer<NonDecimalNumberToken>, r: u32, real: Real, sign: Option<bool>) -> Option<Number> {
    let token = lex.next();

    if None == token {
        return Some(Number::Real(real));
    }

    match token? {
        Ok(NonDecimalNumberToken::Minus) => real_sign(lex, r, real, false),
        Ok(NonDecimalNumberToken::Plus) => real_sign(lex, r, real, true),
        Ok(NonDecimalNumberToken::InfMinus) => real_infnan(lex, real, RealValue::NegInfinity),
        Ok(NonDecimalNumberToken::InfPlus) => real_infnan(lex, real, RealValue::Infinity),
        Ok(NonDecimalNumberToken::NanMinus) => real_infnan(lex, real, RealValue::Nan),
        Ok(NonDecimalNumberToken::NanPlus) => real_infnan(lex, real, RealValue::Nan),
        Ok(NonDecimalNumberToken::At) => real_at(lex, r, real),
        Ok(NonDecimalNumberToken::I) if sign.is_some() => Some(Number::Complex {
            real: Real::from(0),
            imag: Real::from(1),
        }),
        Ok(NonDecimalNumberToken::Uinteger2) if (r == 2) || (r == 8) || (r ==16) => real_sign_uinteger(lex, r, real, sign.unwrap_or(true), lex.slice()),
        Ok(NonDecimalNumberToken::Uinteger8) if (r == 8) || (r == 16) => real_sign_uinteger(lex, r, real, sign.unwrap_or(true), lex.slice()),
        Ok(NonDecimalNumberToken::Uinteger16) if r == 16 => real_sign_uinteger(lex, r, real, sign.unwrap_or(true), lex.slice()),
        _ => None,
    }
}

fn real_sign(lex: &mut Lexer<NonDecimalNumberToken>, r: u32, real: Real, sign: bool) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(NonDecimalNumberToken::I) => Some(Number::Complex {
                real: real.clone(),
                imag: Real {
                    exact: real.exact,
                    value: RealValue::Integer {
                        positive: sign,
                        value: 1,
                    },
                },
        }),
        Ok(NonDecimalNumberToken::Uinteger2) if (r == 2) || (r == 8) || (r ==16) => real_sign_uinteger(lex, r, real, sign, lex.slice()),
        Ok(NonDecimalNumberToken::Uinteger8) if (r == 8) || (r == 16) => real_sign_uinteger(lex, r, real, sign, lex.slice()),
        Ok(NonDecimalNumberToken::Uinteger16) if r == 16 => real_sign_uinteger(lex, r, real, sign, lex.slice()),
        _ => None,
    }
}

fn real_sign_uinteger(
    lex: &mut Lexer<NonDecimalNumberToken>,
    r: u32,
    real: Real,
    sign: bool,
    s: &str,
) -> Option<Number> {
    let token = lex.next();

    if None == token {
        return Some(Number::Complex {
            real: real.clone(),
            imag: Real {
                exact: real.exact,
                value: RealValue::Integer {
                    positive: sign,
                    value: isize::from_str_radix(s, r).unwrap() as u32,
                },
            },
        });
    };

    match token? {
        Ok(NonDecimalNumberToken::Backslash) => real_sign_uinteger_backslash(lex, r, real, sign, s),
        Ok(NonDecimalNumberToken::I) => Some(Number::Complex {
            real: real.clone(),
            imag: Real {
                exact: real.exact,
                value: RealValue::Integer {
                    positive: sign,
                    value: isize::from_str_radix(s, r).unwrap() as u32,
                },
            },
        }),
        _ => None,
    }
}

fn real_sign_uinteger_backslash(
    lex: &mut Lexer<NonDecimalNumberToken>,
    r: u32,
    real: Real,
    sign: bool,
    s: &str,
) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(NonDecimalNumberToken::Uinteger2) if (r == 2) || (r == 8) || (r ==16) => real_sign_uinteger_backslash_uinteger(lex, r, real, sign, s, lex.slice()),
        Ok(NonDecimalNumberToken::Uinteger8) if (r == 8) || (r == 16) => real_sign_uinteger_backslash_uinteger(lex, r, real, sign, s, lex.slice()),
        Ok(NonDecimalNumberToken::Uinteger16) if r == 16 => real_sign_uinteger_backslash_uinteger(lex, r, real, sign, s, lex.slice()),
        _ => None,
    }
}

fn real_sign_uinteger_backslash_uinteger(
    lex: &mut Lexer<NonDecimalNumberToken>,
    r: u32,
    real: Real,
    sign: bool,
    s1: &str,
    s2: &str,
) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(NonDecimalNumberToken::I) => Some(Number::Complex {
            real: real.clone(),
            imag: Real {
                exact: real.exact,
                value: RealValue::Rational {
                    positive: sign,
                    num: isize::from_str_radix(s1, r).unwrap() as u32,
                    den: isize::from_str_radix(s2, r).unwrap() as u32,
                },
            },
        }),
        _ => None,
    }
}

fn real_infnan(
    lex: &mut Lexer<NonDecimalNumberToken>,
    real: Real,
    value: RealValue,
) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(NonDecimalNumberToken::I) => Some(Number::Complex {
            real: real.clone(),
            imag: Real {
                exact: real.exact,
                value: value,
            },
        }),
        _ => None,
    }
}

fn real_at(lex: &mut Lexer<NonDecimalNumberToken>, r: u32, real: Real) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(NonDecimalNumberToken::InfMinus) => real_at_infnan(real, RealValue::NegInfinity),
        Ok(NonDecimalNumberToken::InfPlus) => real_at_infnan(real, RealValue::Infinity),
        Ok(NonDecimalNumberToken::NanMinus) => real_at_infnan(real, RealValue::Nan),
        Ok(NonDecimalNumberToken::NanPlus) => real_at_infnan(real, RealValue::Nan),
        Ok(NonDecimalNumberToken::Minus) => real_at_sign(lex, r, real, false),
        Ok(NonDecimalNumberToken::Plus) => real_at_sign(lex, r, real, true),
        Ok(NonDecimalNumberToken::Uinteger2) if (r == 2) || (r == 8) || (r ==16) => real_at_uinteger(lex, r, real, None, lex.slice()),
        Ok(NonDecimalNumberToken::Uinteger8) if (r == 8) || (r == 16) => real_at_uinteger(lex, r, real, None, lex.slice()),
        Ok(NonDecimalNumberToken::Uinteger16) if r == 16 => real_at_uinteger(lex, r, real, None, lex.slice()),
        _ => None,
    }
}

fn real_at_infnan(
    real: Real,
    value: RealValue,
) -> Option<Number> {

    Some(Number::Complex {
        real: real.clone(),
        imag: Real {
            exact: real.exact,
            value: value,
        },
    })
}

fn real_at_sign(lex: &mut Lexer<NonDecimalNumberToken>, r: u32, real: Real, sign: bool) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(NonDecimalNumberToken::Uinteger2) if (r == 2) || (r == 8) || (r ==16) => real_at_uinteger(lex, r, real, Some(sign), lex.slice()),
        Ok(NonDecimalNumberToken::Uinteger8) if (r == 8) || (r == 16) => real_at_uinteger(lex, r, real, Some(sign), lex.slice()),
        Ok(NonDecimalNumberToken::Uinteger16) if r == 16 => real_at_uinteger(lex, r, real, Some(sign), lex.slice()),
        _ => None,
    }
}

fn real_at_uinteger(
    lex: &mut Lexer<NonDecimalNumberToken>,
    r: u32,
    real: Real,
    sign: Option<bool>,
    s: &str,
) -> Option<Number> {
    let token = lex.next();

    if None == token {
        return Some(Number::Complex {
            real: real.clone(),
            imag: Real {
                exact: real.exact,
                value: RealValue::Integer {
                    positive: sign.unwrap_or(true),
                    value: isize::from_str_radix(s, r).unwrap() as u32,
                },
            },
        });
    };

    match token? {
        Ok(NonDecimalNumberToken::Backslash) => real_at_uinteger_backslash(lex, r, real, sign, s),
        _ => None,
    }
}

fn real_at_uinteger_backslash(
    lex: &mut Lexer<NonDecimalNumberToken>,
    r: u32,
    real: Real,
    sign: Option<bool>,
    s: &str,
) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(NonDecimalNumberToken::Uinteger2)
        | Ok(NonDecimalNumberToken::Uinteger8)
        | Ok(NonDecimalNumberToken::Uinteger16) => Some(Number::Complex {
            real: real.clone(),
            imag: Real {
                exact: real.exact,
                value: RealValue::Rational {
                    positive: sign.unwrap_or(true),
                    num: isize::from_str_radix(s, r).unwrap() as u32,
                    den: isize::from_str_radix(lex.slice(), r).unwrap() as u32,
                },
            },
        }),
        _ => None,
    }
}
