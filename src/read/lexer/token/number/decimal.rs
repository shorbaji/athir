use logos::{Lexer, Logos};
use crate::value::number::{Number, real::{Real, RealValue}};

pub fn parse_decimal(s: &str) -> Option<Number> {
    let mut lex = DecimalNumberToken::lexer(s);

    num(&mut lex)
}

fn num(lex: &mut Lexer<DecimalNumberToken>) -> Option<Number> {
    let token = lex.next()?;
    match token {        
        Ok(DecimalNumberToken::Radix) => radix(lex),
        Ok(DecimalNumberToken::Exact) => exact(lex, true,),
        Ok(DecimalNumberToken::Inexact) => exact(lex, false),
        Ok(DecimalNumberToken::Plus) => sign(lex, true, None),
        Ok(DecimalNumberToken::Minus) => sign(lex, false, None),
        Ok(DecimalNumberToken::InfPlus) => infnan(lex, RealValue::Infinity, true),
        Ok(DecimalNumberToken::InfMinus) => infnan(lex, RealValue::NegInfinity, false),
        Ok(DecimalNumberToken::NanMinus) => infnan(lex, RealValue::Nan, false),
        Ok(DecimalNumberToken::NanPlus) => infnan(lex, RealValue::Nan, true),
        Ok(DecimalNumberToken::Uinteger) => uinteger(lex, None, None, lex.slice()),
        Ok(DecimalNumberToken::Dot) => uinteger_dot(lex, None, None, None), // got a dot with no exactness, no sign and no pre-dot digits
        _ => None,
    }
}

fn radix(lex: &mut Lexer<DecimalNumberToken>) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(DecimalNumberToken::Exact) => prefix(lex, true),
        Ok(DecimalNumberToken::Inexact) => prefix(lex, false),
        Ok(DecimalNumberToken::Plus) => sign(lex, true, None),
        Ok(DecimalNumberToken::Minus) => sign(lex, false, None),
        Ok(DecimalNumberToken::InfPlus) => infnan(lex, RealValue::Infinity, true),
        Ok(DecimalNumberToken::InfMinus) => infnan(lex, RealValue::NegInfinity, false),
        Ok(DecimalNumberToken::NanMinus) => infnan(lex, RealValue::Nan, false),
        Ok(DecimalNumberToken::NanPlus) => infnan(lex, RealValue::Nan, true),
        Ok(DecimalNumberToken::Uinteger) => uinteger(lex, None, None, lex.slice()),
        Ok(DecimalNumberToken::Dot) => uinteger_dot(lex, None, None, None), // got a dot with no exactness, no sign and no pre-dot digits
        _ => None,
    }
}

fn exact(lex: &mut Lexer<DecimalNumberToken>, exact: bool) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(DecimalNumberToken::Radix) => prefix(lex, exact),
        Ok(DecimalNumberToken::Dot) => uinteger_dot(lex, Some(exact), None, None), // got a dot with no sign and no pre-dot digits
        Ok(DecimalNumberToken::InfMinus) => infnan(lex, RealValue::NegInfinity, false),
        Ok(DecimalNumberToken::InfPlus) => infnan(lex, RealValue::Infinity, true),
        Ok(DecimalNumberToken::NanMinus) => infnan(lex, RealValue::Nan, false),
        Ok(DecimalNumberToken::NanPlus) => infnan(lex, RealValue::Nan, true),
        Ok(DecimalNumberToken::Plus) => sign(lex, true, Some(exact)),
        Ok(DecimalNumberToken::Minus) => sign(lex, false, Some(exact)),
        Ok(DecimalNumberToken::Uinteger) => uinteger(lex, Some(exact), None, lex.slice()),
        _ => None,
    }
}

fn prefix(lex: &mut Lexer<DecimalNumberToken>, exact: bool) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(DecimalNumberToken::Plus) => sign(lex, true, Some(exact)),
        Ok(DecimalNumberToken::Minus) => sign(lex, false, Some(exact)),
        Ok(DecimalNumberToken::InfPlus) => infnan(lex, RealValue::Infinity, true),
        Ok(DecimalNumberToken::InfMinus) => infnan(lex, RealValue::NegInfinity, false),
        Ok(DecimalNumberToken::NanMinus) => infnan(lex, RealValue::Nan, false,),
        Ok(DecimalNumberToken::NanPlus) => infnan(lex, RealValue::Nan, true),
        Ok(DecimalNumberToken::Dot) => uinteger_dot(lex, Some(exact), None, None), // got a dot with no sign and no pre-dot digits
        Ok(DecimalNumberToken::Uinteger) => uinteger(lex, Some(exact), None, lex.slice()),
        _ => None,
    }
}

fn infnan(lex: &mut Lexer<DecimalNumberToken>, value: RealValue, sign: bool) -> Option<Number> {
    real(lex, Real { exact: true, value: value }, Some(sign))
}

fn sign(lex: &mut Lexer<DecimalNumberToken>, sign: bool, exact: Option<bool>) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(DecimalNumberToken::Uinteger) => uinteger(lex, exact, Some(sign), lex.slice()),
        Ok(DecimalNumberToken::I) => Some(Number::Complex {
            real: Real::from(0),
            imag: Real::from(1),
        }),
        Ok(DecimalNumberToken::Dot) => uinteger_dot(lex, exact, Some(sign), None), // got a dot with no pre-dot digits
        _ => None,
    }
}

fn uinteger(
    lex: &mut Lexer<DecimalNumberToken>,
    exact: Option<bool>,
    sign: Option<bool>,
    s: &str,
) -> Option<Number> {
    let token = lex.next();

    let int: u32 = s.parse().unwrap();

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
        Ok(DecimalNumberToken::Backslash) => uinteger_backslash(lex, sign, exact, int),
        Ok(DecimalNumberToken::Minus) => real_sign(lex, real, false),
        Ok(DecimalNumberToken::Plus) => real_sign(lex, real, true),
        Ok(DecimalNumberToken::InfMinus) => real_infnan(lex, real, RealValue::NegInfinity),
        Ok(DecimalNumberToken::InfPlus) => real_infnan(lex, real, RealValue::Infinity),
        Ok(DecimalNumberToken::NanMinus) => real_infnan(lex, real, RealValue::Nan),
        Ok(DecimalNumberToken::NanPlus) => real_infnan(lex, real, RealValue::Nan),
        Ok(DecimalNumberToken::At) => real_at(lex, real),
        Ok(DecimalNumberToken::I) if sign.is_some() => Some(Number::Complex { real: real, imag: Real::from(1) }),
        Ok(DecimalNumberToken::Dot) => uinteger_dot(lex, exact, sign, Some(s)),
        Ok(DecimalNumberToken::E) => uinteger_dot_uinteger_e(lex, exact, sign, Some(s), None),
        _ => None,
    }
}

fn uinteger_dot(
    lex: &mut Lexer<DecimalNumberToken>,
    exact: Option<bool>,
    sign: Option<bool>,
    s: Option<&str>,
) -> Option<Number> {
    let token = lex.next();

    if None == token && s.is_some() {
        return Some(Number::Real(Real {
            exact: exact.unwrap_or(true),
            value: RealValue::Irrational(s.unwrap().parse().unwrap())
        }));
    }

    match token? {
        Ok(DecimalNumberToken::E) if s.is_some() => uinteger_dot_uinteger_e(lex, exact, sign, s, None),
        Ok(DecimalNumberToken::Uinteger) => uinteger_dot_uinteger(lex, exact, sign, s, Some(lex.slice())),
        _ => None,
    }
}

fn uinteger_dot_uinteger(
    lex: &mut Lexer<DecimalNumberToken>,
    exact: Option<bool>,
    sign: Option<bool>,
    s: Option<&str>,
    t: Option<&str>,
) -> Option<Number> {
    let token = lex.next();

    let r = Real { exact: exact.unwrap_or(true), value: RealValue::Irrational(format!("{}.{}", s.unwrap(), t.unwrap()).parse().unwrap()) };

    if None == token && s.is_some() && t.is_some() {
        return Some(Number::Real(r));
    }

    match token? {
        Ok(DecimalNumberToken::E) if s.is_some() || t.is_some() => uinteger_dot_uinteger_e(lex, exact, sign, s, t),
        Ok(DecimalNumberToken::Plus) => real(lex, r, Some(true)),
        Ok(DecimalNumberToken::Minus) => real(lex, r, Some(false)),
        _ => None,
    }
}

fn uinteger_dot_uinteger_e(
    lex: &mut Lexer<DecimalNumberToken>,
    exact: Option<bool>,
    sign: Option<bool>,
    s: Option<&str>,
    t: Option<&str>,
) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(DecimalNumberToken::Plus) => uinteger_dot_uinteger_e_sign(lex, exact, sign, s, t, Some(true)),
        Ok(DecimalNumberToken::Minus) => uinteger_dot_uinteger_e_sign(lex, exact, sign, s, t, Some(false)),
        Ok(DecimalNumberToken::Uinteger) => uinteger_dot_uinteger_e_uinteger(lex, exact, sign, s, t, None, Some(lex.slice())),
        _ => None,
    }
}

fn uinteger_dot_uinteger_e_sign(
    lex: &mut Lexer<DecimalNumberToken>,
    exact: Option<bool>,
    sign: Option<bool>,
    s: Option<&str>,
    t: Option<&str>,
    e_sign: Option<bool>,
) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(DecimalNumberToken::Uinteger) => uinteger_dot_uinteger_e_uinteger(lex, exact, sign, s, t, e_sign, Some(lex.slice())),
        _ => None,
    }
}

fn uinteger_dot_uinteger_e_uinteger(
    lex: &mut Lexer<DecimalNumberToken>,
    exact: Option<bool>,
    sign: Option<bool>,
    s: Option<&str>,
    t: Option<&str>,
    e_sign: Option<bool>,
    u: Option<&str>,
) -> Option<Number> {

    let s = format!(
        "{}{}.{}e{}{}",
        sign.map(|s| if s { "" } else { "-" }).unwrap_or(""),
        s.unwrap_or(""),
        t.unwrap_or(""),
        e_sign.map(|s| if s { "+" } else { "-" }).unwrap_or(""),
        u.unwrap_or("")
    );

    let r = Real {
        exact: exact.unwrap_or(true),
        value: RealValue::Irrational(s.parse().unwrap()),
    };

    real(lex, r, sign)
}

fn uinteger_backslash(
    lex: &mut Lexer<DecimalNumberToken>,
    exact: Option<bool>,
    sign: Option<bool>,
    value: u32,
) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(DecimalNumberToken::Uinteger) => {
            let num = value;
            let den = lex.slice().parse().unwrap();

            let value = RealValue::Rational {
                positive: sign.unwrap_or(true),
                num: num,
                den: den,
            };

            let real = Real {
                exact: exact.unwrap_or(true),
                value: value,
            };

            Some(Number::Real(real))
        }
        _ => None,
    }
}

fn real(lex: &mut Lexer<DecimalNumberToken>, real: Real, sign: Option<bool>) -> Option<Number> {
    let token = lex.next();

    if None == token {
        return Some(Number::Real(real));
    }

    match token? {
        Ok(DecimalNumberToken::Minus) => real_sign(lex, real, false),
        Ok(DecimalNumberToken::Plus) => real_sign(lex, real, true),
        Ok(DecimalNumberToken::InfMinus) => real_infnan(lex, real, RealValue::NegInfinity),
        Ok(DecimalNumberToken::InfPlus) => real_infnan(lex, real, RealValue::Infinity),
        Ok(DecimalNumberToken::NanMinus) => real_infnan(lex, real, RealValue::Nan),
        Ok(DecimalNumberToken::NanPlus) => real_infnan(lex, real, RealValue::Nan),
        Ok(DecimalNumberToken::At) => real_at(lex, real),
        Ok(DecimalNumberToken::I) if sign.is_some() => Some(Number::Complex {
            real: Real::from(0),
            imag: Real::from(1),
        }),
        Ok(DecimalNumberToken::Uinteger) => real_sign_uinteger(lex, real, sign.unwrap_or(true), lex.slice()),
        _ => None,
    }
}

fn real_sign(lex: &mut Lexer<DecimalNumberToken>, real: Real, sign: bool) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(DecimalNumberToken::I) => Some(Number::Complex {
                real: real.clone(),
                imag: Real {
                    exact: real.exact,
                    value: RealValue::Integer {
                        positive: sign,
                        value: 1,
                    },
                },
        }),
        Ok(DecimalNumberToken::Uinteger) => real_sign_uinteger(lex, real, sign, lex.slice()),
        Ok(DecimalNumberToken::Dot) => real_sign_uinteger_dot(lex, Some(real.exact), real.clone(), Some(sign), None), // No pre-decimal digits
        _ => None,
    }
}

fn real_sign_uinteger(
    lex: &mut Lexer<DecimalNumberToken>,
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
                    value: s.parse().unwrap(),
                },
            },
        });
    };

    match token? {
        Ok(DecimalNumberToken::Backslash) => real_sign_uinteger_backslash(lex, real, sign, s),
        Ok(DecimalNumberToken::Dot) => real_sign_uinteger_dot(lex, Some(real.exact), real.clone(), Some(sign), Some(s)),
        Ok(DecimalNumberToken::I) => Some(Number::Complex {
            real: real.clone(),
            imag: Real {
                exact: real.exact,
                value: RealValue::Integer {
                    positive: sign,
                    value: s.parse().unwrap(),
                },
            },
        }),
        Ok(DecimalNumberToken::E) => real_sign_uinteger_dot_uinteger_e(lex, Some(real.exact), real.clone(), Some(sign), Some(s), None),
        _ => None,
    }
}

fn real_sign_uinteger_dot(
    lex: &mut Lexer<DecimalNumberToken>,
    exact: Option<bool>,
    real: Real,
    sign: Option<bool>,
    s: Option<&str>,
) -> Option<Number> {
    let token = lex.next();

    if None == token && s.is_some() {
        return Some(Number::Complex {
            real: real,
            imag: Real {
                exact: exact.unwrap_or(true),
                value: RealValue::Irrational(s.unwrap().parse().unwrap())
            }
        });
    }

    match token? {
        Ok(DecimalNumberToken::E) if s.is_some() => real_sign_uinteger_dot_uinteger_e(lex, exact, real, sign, s, None),
        Ok(DecimalNumberToken::Uinteger) => real_sign_uinteger_dot_uinteger(lex, exact, real, sign, s, Some(lex.slice())),
        _ => None,
    }
}

fn real_sign_uinteger_dot_uinteger(
    lex: &mut Lexer<DecimalNumberToken>,
    exact: Option<bool>,
    real: Real,
    sign: Option<bool>,
    s: Option<&str>,
    t: Option<&str>,
) -> Option<Number> {
    let token = lex.next();

    if None == token && s.is_some() && t.is_some() {
        return Some(Number::Complex {
            real: real,
            imag: Real {
                exact: exact.unwrap_or(true),
                value: RealValue::Irrational(format!("{}.{}", s.unwrap(), t.unwrap()).parse().unwrap())
            }    
        });
    }

    match token? {
        Ok(DecimalNumberToken::E) if s.is_some() || t.is_some() => real_sign_uinteger_dot_uinteger_e(lex, exact, real, sign, s, t),
        _ => None,
    }
}

fn real_sign_uinteger_dot_uinteger_e(
    lex: &mut Lexer<DecimalNumberToken>,
    exact: Option<bool>,
    real: Real,
    sign: Option<bool>,
    s: Option<&str>,
    t: Option<&str>,
) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(DecimalNumberToken::Plus) => real_sign_uinteger_dot_uinteger_e_sign(lex, exact, real, sign, s, t, Some(true)),
        Ok(DecimalNumberToken::Minus) => real_sign_uinteger_dot_uinteger_e_sign(lex, exact, real, sign, s, t, Some(false)),
        Ok(DecimalNumberToken::Uinteger) => real_sign_uinteger_dot_uinteger_e_uinteger(lex, exact, real, sign, s, t, None, Some(lex.slice())),
        _ => None,
    }
}

fn real_sign_uinteger_dot_uinteger_e_sign(
    lex: &mut Lexer<DecimalNumberToken>,
    exact: Option<bool>,
    real: Real,
    sign: Option<bool>,
    s: Option<&str>,
    t: Option<&str>,
    e_sign: Option<bool>,
) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(DecimalNumberToken::Uinteger) => real_sign_uinteger_dot_uinteger_e_uinteger(lex, exact, real, sign, s, t, e_sign, Some(lex.slice())),
        _ => None,
    }
}

fn real_sign_uinteger_dot_uinteger_e_uinteger(
    _lex: &mut Lexer<DecimalNumberToken>,
    exact: Option<bool>,
    real: Real,
    sign: Option<bool>,
    s: Option<&str>,
    t: Option<&str>,
    e_sign: Option<bool>,
    u: Option<&str>,
) -> Option<Number> {

    let s = format!(
        "{}{}.{}e{}{}",
        sign.map(|s| if s { "" } else { "-" }).unwrap_or(""),
        s.unwrap_or(""),
        t.unwrap_or(""),
        e_sign.map(|s| if s { "+" } else { "-" }).unwrap_or(""),
        u.unwrap_or("")
    );

    let imag = Real {
        exact: exact.unwrap_or(true),
        value: RealValue::Irrational(s.parse().unwrap()),
    };

    let token = _lex.next()?;

    match token {
        Ok(DecimalNumberToken::I) => Some(Number::Complex { real: real, imag }),
        _ => None,
    }
}

fn real_sign_uinteger_backslash(
    lex: &mut Lexer<DecimalNumberToken>,   
    real: Real,
    sign: bool,
    s: &str,
) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(DecimalNumberToken::Uinteger) => real_sign_uinteger_backslash_uinteger(lex, real, sign, s, lex.slice()),
        _ => None,
    }
}

fn real_sign_uinteger_backslash_uinteger(
    lex: &mut Lexer<DecimalNumberToken>,
    real: Real,
    sign: bool,
    s1: &str,
    s2: &str,
) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(DecimalNumberToken::I) => Some(Number::Complex {
            real: real.clone(),
            imag: Real {
                exact: real.exact,
                value: RealValue::Rational {
                    positive: sign,
                    num: s1.parse().unwrap(),
                    den: s2.parse().unwrap(),
                },
            },
        }),
        _ => None,
    }
}

fn real_infnan(
    lex: &mut Lexer<DecimalNumberToken>,
    real: Real,
    value: RealValue,
) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(DecimalNumberToken::I) => Some(Number::Complex {
            real: real.clone(),
            imag: Real {
                exact: real.exact,
                value: value,
            },
        }),
        _ => None,
    }
}

fn real_at(lex: &mut Lexer<DecimalNumberToken>, real: Real) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(DecimalNumberToken::InfMinus) => real_at_infnan(real, RealValue::NegInfinity),
        Ok(DecimalNumberToken::InfPlus) => real_at_infnan(real, RealValue::Infinity),
        Ok(DecimalNumberToken::NanMinus) => real_at_infnan(real, RealValue::Nan),
        Ok(DecimalNumberToken::NanPlus) => real_at_infnan(real, RealValue::Nan),
        Ok(DecimalNumberToken::Minus) => real_at_sign(lex, real, false),
        Ok(DecimalNumberToken::Plus) => real_at_sign(lex, real, true),
        Ok(DecimalNumberToken::Uinteger) => real_at_uinteger(lex, real, None, lex.slice()),        
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

fn real_at_sign(lex: &mut Lexer<DecimalNumberToken>, real: Real, sign: bool) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(DecimalNumberToken::Uinteger) => real_at_uinteger(lex, real, Some(sign), lex.slice()),
        _ => None,
    }
}

fn real_at_uinteger(
    lex: &mut Lexer<DecimalNumberToken>,   
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
                    value: s.parse().unwrap(),
                },
            },
        });
    };

    match token? {
        Ok(DecimalNumberToken::Backslash) => real_at_uinteger_backslash(lex, real, sign, s),
        Ok(DecimalNumberToken::Dot) => real_at_uinteger_dot(lex, real, sign, s),
        Ok(DecimalNumberToken::E) => real_at_uinteger_dot_uinteger_e(lex, real, sign, Some(s), None),
        _ => None,
    }
}

fn real_at_uinteger_dot(
    lex: &mut Lexer<DecimalNumberToken>,
   
    real: Real,
    sign: Option<bool>,
    s: &str,
) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(DecimalNumberToken::Uinteger) => real_at_uinteger_dot_uinteger(lex, real, sign, s, lex.slice()),
        Ok(DecimalNumberToken::E) => real_at_uinteger_dot_uinteger_e(lex, real, sign, Some(s), None),
        _ => None,
    }
}

fn real_at_uinteger_dot_uinteger(
    lex: &mut Lexer<DecimalNumberToken>,
    real: Real,
    sign: Option<bool>,
    s1: &str,
    s2: &str,
) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(DecimalNumberToken::E) => real_at_uinteger_dot_uinteger_e(lex, real, sign, Some(s1), Some(s2)),
        _ => None,
    }
}

fn real_at_uinteger_dot_uinteger_e(
    lex: &mut Lexer<DecimalNumberToken>,
    real: Real,
    sign: Option<bool>,
    s1: Option<&str>,
    s2: Option<&str>,
) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(DecimalNumberToken::Minus) => real_at_uinteger_dot_uinteger_e_sign(lex, real, sign, s1, s2, false),
        Ok(DecimalNumberToken::Plus) => real_at_uinteger_dot_uinteger_e_sign(lex, real, sign, s1, s2, true),
        Ok(DecimalNumberToken::Uinteger) => real_at_uinteger_dot_uinteger_e_uinteger(lex, real, sign, s1, s2, None, Some(lex.slice())),
        _ => None,
    }
}

fn real_at_uinteger_dot_uinteger_e_sign(
    lex: &mut Lexer<DecimalNumberToken>,
    real: Real,
    sign: Option<bool>,
    s1: Option<&str>,
    s2: Option<&str>,
    sign2: bool,
) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(DecimalNumberToken::Uinteger) => real_at_uinteger_dot_uinteger_e_uinteger(lex, real, sign, s1, s2, Some(sign2), Some(lex.slice())),
        _ => None,
    }
}

fn real_at_uinteger_dot_uinteger_e_uinteger(
    _lex: &mut Lexer<DecimalNumberToken>,
    real: Real,
    sign: Option<bool>,
    s: Option<&str>,
    t: Option<&str>,
    e_sign: Option<bool>,
    u: Option<&str>,
) -> Option<Number> {

    let s = format!(
        "{}{}.{}e{}{}",
        sign.map(|s| if s { "" } else { "-" }).unwrap_or(""),
        s.unwrap_or(""),
        t.unwrap_or(""),
        e_sign.map(|s| if s { "+" } else { "-" }).unwrap_or(""),
        u.unwrap_or("")
    );

    let imag = Real {
        exact: real.clone().exact,
        value: RealValue::Irrational(s.parse().unwrap()),
    };

    Some(Number::Complex {
        real: real,
        imag: imag,
    })
}

fn real_at_uinteger_backslash(
    lex: &mut Lexer<DecimalNumberToken>,
   
    real: Real,
    sign: Option<bool>,
    s: &str,
) -> Option<Number> {
    let token = lex.next()?;

    match token {
        Ok(DecimalNumberToken::Uinteger) => Some(Number::Complex {
            real: real.clone(),
            imag: Real {
                exact: real.exact,
                value: RealValue::Rational {
                    positive: sign.unwrap_or(true),
                    num: s.parse().unwrap(),
                    den: lex.slice().parse().unwrap(),
                },
            },
        }),
        _ => None,
    }
}



#[derive(Logos, Debug, PartialEq, Clone)]
pub enum DecimalNumberToken {
    #[token("@")]
    At,

    #[token("/")]
    Backslash,

    #[token(".")]
    Dot,

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

    #[token("#d")]
    Radix,

    #[regex(r"(e|E)")]
    E,

    #[regex(r"[0-9]+")]
    Uinteger,

}
