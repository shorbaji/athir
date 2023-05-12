#[cfg(test)]
mod tests;
pub mod real;

use std::ops::{Add, Sub, Mul, Div, Neg};
use real::{Real, Integer, Irrational};

pub trait Exact {
    fn is_exact(&self) -> bool;
    fn exact(&self) -> Self;
}

#[derive(Debug, Clone)]
pub enum Number {
    Real(Real),
    Complex{real: Real, imag: Real},
}

impl Number {
    pub fn is_complex(&self) -> bool {
        match self {
            Number::Complex{..} => true,
            _ => false,
        }
    }

    pub fn is_real(&self) -> bool {
        match self {
            Number::Real(_) => true,
            Number::Complex{..} => false,
        }
    }

    pub fn is_rational(&self) -> bool {
        match self {
            Number::Real(r) => r.is_rational(),
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Number::Real(r) => r.is_integer(),
            _ => false,
        }
    }

    pub fn is_exact(&self) -> bool {
        match self {
            Number::Real(r) => r.is_exact(),
            Number::Complex{real, imag} => real.is_exact() && imag.is_exact(),
        }
    }

    pub fn is_inexact(&self) -> bool {
        !self.is_exact()
    }

    pub fn is_exact_integer(&self) -> bool {
        match self {
            Number::Real(r) => r.is_exact_integer(),
            _ => false,
        }
    }

    pub fn is_finite(&self) -> bool {
        match self {
            Number::Real(r) => r.is_finite(),
            Number::Complex{real, imag} => real.is_finite() && imag.is_finite(),
        }
    }

    pub fn is_infinite(&self) -> bool {
        !self.is_finite()
    }

    pub fn is_nan(&self) -> bool {
        match self {
            Number::Real(r) => r.is_nan(),
            Number::Complex{real, imag} => real.is_nan() || imag.is_nan(),
        }
    }
    
    pub fn is_zero(&self) -> bool {
        match self {
            Number::Real(r) => r.is_zero(),
            Number::Complex{real, imag} => real.is_zero() && imag.is_zero(),
        }
    }

    pub fn is_positive(&self) -> bool {
        match self {
            Number::Real(r) => r.is_positive(),
            Number::Complex{real, imag} => real.is_positive() || imag.is_positive(),
        }
    }

    pub fn is_negative(&self) -> bool {
        match self {
            Number::Real(r) => r.is_negative(),
            Number::Complex{real, imag} => real.is_negative() || imag.is_negative(),
        }
    }

    pub fn is_odd(&self) -> bool {
        match self {
            Number::Real(r) => r.is_odd(),
            _ => false,
        }
    }

    pub fn is_even(&self) -> bool {
        match self {
            Number::Real(r) => r.is_even(),
            _ => false,
        }
    }

    pub fn abs(self) -> Self {
        match self {
            Number::Real(r) => Number::Real(r.abs()),
            Number::Complex{real, imag} => Number::Real(real.clone() * real + imag.clone() * imag).sqrt(),
        }
    }

    pub fn square(self) -> Self {
        match self {
            Number::Real(r) => Number::Real(r.square()),
            Number::Complex{real, imag} => Number::Complex{real: real.clone() * real.clone() - imag.clone() * imag.clone(), imag: real.clone() * imag.clone() + real * imag},
        }
    }

    pub fn sqrt(&self) -> Self {
        match self {
            Number::Real(r) => Number::Real(r.sqrt()),
            Number::Complex{..} => panic!("Not implemented yet"),
        }
    }

    pub fn simplify(&self) -> Self {
        match self {
            Number::Real(r) => Number::Real(r.simplify()),
            Number::Complex{real, imag} => Number::Complex{real: real.simplify(), imag: imag.simplify()},
        }
    }
}

impl Exact for Number {
    fn is_exact(&self) -> bool {
        match self {
            Number::Real(r) => r.is_exact(),
            Number::Complex{real, imag} => real.is_exact() && imag.is_exact(),
        }
    }

    fn exact(&self) -> Self {
        match self {
            Number::Real(r) => Number::Real(r.exact()),
            Number::Complex{real, imag} => Number::Complex{real: real.exact(), imag: imag.exact()},
        }
    }
}

impl From<Integer> for Number {
    fn from(i: Integer) -> Self {
        Number::Real(Real::from(i))
    }
}

impl From<Irrational> for Number {
    fn from(i: Irrational) -> Self {
        Number::Real(Real::from(i))
    }
}

impl Add for Number {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs.clone()) {
            (Number::Real(lhs), Number::Real(rhs)) => Number::Real(lhs + rhs),
            (Number::Real(lhs), Number::Complex{real, imag}) => Number::Complex{real: lhs + real, imag},
            (Number::Complex{real, imag}, Number::Real(rhs)) => Number::Complex{real: real + rhs, imag: imag},
            (Number::Complex{real: r1, imag: i1}, Number::Complex{real: r2, imag: i2}) => Number::Complex{real: r1 + r2, imag: i1 + i2},
        }.simplify()
    }
}

impl Sub for Number {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match(self, rhs) {
            (Number::Real(lhs), Number::Real(rhs)) => Number::Real(lhs - rhs),
            (Number::Real(lhs), Number::Complex{real, imag}) => Number::Complex{real: lhs - real, imag: -imag},
            (Number::Complex{real, imag}, Number::Real(rhs)) => Number::Complex{real: real - rhs, imag},
            (Number::Complex{real: r1, imag: i1}, Number::Complex{real: r2, imag: i2}) => Number::Complex{real: r1 - r2, imag: i1 - i2},
        }.simplify()
    }
}

impl Mul for Number {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match(self, rhs) {
            (Number::Real(lhs), Number::Real(rhs)) => Number::Real(lhs * rhs),
            (Number::Real(lhs), Number::Complex{real, imag}) => Number::Complex{real: lhs.clone() * real, imag: lhs * imag},
            (Number::Complex{real, imag}, Number::Real(rhs)) => Number::Complex{real: real * rhs.clone(), imag: imag * rhs},
            (Number::Complex{real: r1, imag: i1}, Number::Complex{real: r2, imag: i2}) => Number::Complex{real: r1.clone() * r2.clone() - i1.clone() * i2.clone(), imag: r1 * i2 + i1 * r2},
        }.simplify()
    }
}

impl Div for Number {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Real(lhs), Number::Real(rhs)) => Number::Real(lhs / rhs),
            (Number::Real(lhs), Number::Complex{real, imag}) => {
                let divisor = real.clone() * real.clone() + imag.clone() * imag.clone();

                Number::Complex{real: lhs.clone() * real.clone() / divisor.clone(), imag: -lhs * imag / divisor}
            },
            (Number::Complex{real, imag}, Number::Real(rhs)) => Number::Complex{real: real / rhs.clone(), imag: imag / rhs},
            (Number::Complex{real: r1, imag: i1}, Number::Complex{real: r2, imag: i2}) => {
                let divisor = r2.clone() * r2.clone() + i2.clone() * i2.clone();

                Number::Complex{real: (r1.clone() * r2.clone() + i1.clone() * i2.clone()) / divisor.clone(), imag: (i1 * r2 - r1 * i2) / divisor}
            },
        }.simplify()
    }
}

impl Neg for Number {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Number::Real(r) => Number::Real(-r),
            Number::Complex{real, imag} => Number::Complex{real: -real, imag: -imag},
        }
    }
}


impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Number::Real(lhs), Number::Real(rhs)) => lhs == rhs,
            (Number::Real(lhs), Number::Complex{real, imag}) => lhs == real && imag.is_zero(),
            (Number::Complex{real, imag}, Number::Real(rhs)) => real == rhs && imag.is_zero(),
            (Number::Complex{real: r1, imag: i1}, Number::Complex{real: r2, imag: i2}) => r1 == r2 && i1 == i2,
        }
    }
}

