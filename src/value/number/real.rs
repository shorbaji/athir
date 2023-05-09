use std::ops::{Add, Sub, Mul, Div, Neg};
use super::Exact;
use gcd::Gcd;

#[derive(Debug, Clone,)]
pub struct Real {
    pub exact: bool,
    pub value: RealValue,
}

impl Exact for Real {
    fn is_exact(&self) -> bool {
        self.exact
    }

    fn exact(&self) -> Self {
        Real{exact: true, value: self.value.clone()}
    }
}

impl Real {
    pub fn is_integer(&self) -> bool {
        match self.value {
            RealValue::Integer{..} => true,
            _ => false,
        }
    }

    pub fn is_rational(&self) -> bool {
        match self.value {
            RealValue::Rational{..} => true,
            _ => false,
        }
    }

    pub fn is_irrational(&self) -> bool {
        match self.value {
            RealValue::Irrational(_) => true,
            _ => false,
        }
    }

    pub fn is_exact_integer(&self) -> bool {
        self.exact && matches!(self.value, RealValue::Integer{..})
    }

    pub fn is_finite(&self) -> bool {
        self.value.is_finite()
    }

    pub fn is_nan(&self) -> bool {
        self.value.is_nan()
    }

    pub fn is_zero(&self) -> bool {
        match self.value {
            RealValue::Integer{positive: _, value: i} => i==0,
            RealValue::Rational{ref num, ..} => *num == 0,
            RealValue::Irrational(ref i) => *i==0.0,
            _ => false,
        }
    }

    pub fn is_positive(&self) -> bool {
        self.value.is_positive()   
    }

    pub fn is_negative(&self) -> bool {
        self.value.is_negative()
    }

    pub fn is_odd(&self) -> bool {
        self.value.is_odd()
    }

    pub fn is_even(&self) -> bool {
        self.value.is_even()
    }

    pub fn abs(&self) -> Self {
        Self {exact: self.exact, value: self.value.abs()}
    }

    pub fn square(self) -> Self {
        Self {exact: self.exact, value: self.value.square()}
    }

    pub fn sqrt(&self) -> Self {
        Self {exact: false, value: self.value.sqrt()}
    }

    pub fn simplify(&self) -> Self {
        Real{exact: self.exact, value: self.value.simplify()}
    }
}

impl PartialEq for Real {
    fn eq(&self, other: &Self) -> bool {
        self.exact && other.exact && self.value.simplify() == other.value.simplify()
    }
}
impl From<Integer> for Real {
    fn from(i: Integer) -> Self {
        Real{ exact: true, value: RealValue::Integer{positive: true, value: i} }
    }
}

impl From<Irrational> for Real {
    fn from(i: Irrational) -> Self {
        Real{ exact: false, value: RealValue::Irrational(i) }
    }
}

impl Add for Real {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        Self { exact: !self.is_irrational() && !rhs.is_irrational() && self.exact && rhs.exact, value: self.value + rhs.value }
    }
}

impl Sub for Real {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        Self { exact: !self.is_irrational() && !rhs.is_irrational() && self.exact && rhs.exact, value: self.value - rhs.value }
    }
}

impl Mul for Real {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self {
        Self { exact: !self.is_irrational() && !rhs.is_irrational() && self.exact && rhs.exact, value: self.value * rhs.value }
    }
}

impl Div for Real {
    type Output = Self;

    fn div(self, rhs: Self) -> Self {
        Self { exact: !self.is_irrational() && !rhs.is_irrational() && self.exact && rhs.exact, value: self.value / rhs.value }
    }
}

impl Neg for Real {
    type Output = Self;

    fn neg(self) -> Self {
        Self { exact: self.exact, value: -self.value }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum RealValue {
    Integer{positive: bool, value: Integer},
    Irrational(Irrational),
    Rational{positive: bool, num: Integer, den: Integer},
    Infinity,
    NegInfinity,
    Nan,
}

impl RealValue {
    fn reciprocal(&self) -> Self {
        match self {
            RealValue::Integer{positive: p, value: i} => RealValue::Rational{positive: *p, num: 1, den: *i},
            RealValue::Rational{positive: p, num, den} => RealValue::Rational{positive: *p, num: *den, den: *num},
            RealValue::Irrational(i) => RealValue::Irrational(1.0 / i),
            RealValue::Infinity => RealValue::Integer{positive: true, value: 0},
            RealValue::NegInfinity => RealValue::Integer{positive: false, value: 0},
            RealValue::Nan => RealValue::Nan,
        }
    }

    fn is_finite(&self) -> bool {
        match self {
            RealValue::Infinity => false,
            RealValue::NegInfinity => false,
            RealValue::Nan => false,
            _ => true,
        }
    }

    fn is_nan(&self) -> bool {
        match self {
            RealValue::Nan => true,
            _ => false,
        }
    }

    fn is_zero(&self) -> bool {
        match self {
            RealValue::Integer{positive: true, value: i} => *i== 0 as u32,
            RealValue::Rational{num, ..} => *num == 0,
            RealValue::Irrational(i) => *i==0.0,
            _ => false,
        }
    }

    fn is_positive(&self) -> bool {
        match self {
            RealValue::Integer{positive: p, value: _} => *p,
            RealValue::Rational{positive: p, ..} => *p,
            RealValue::Irrational(i) => *i > 0.0,
            RealValue::Infinity => true,
            RealValue::NegInfinity => false,
            RealValue::Nan => true,
        }
    }

    fn is_negative(&self) -> bool {
        match self {
            RealValue::Integer{positive: p, ..} => !*p,
            RealValue::Rational{positive: p, ..} => !*p,
            RealValue::Irrational(i) => *i < 0.0,
            RealValue::Infinity => false,
            RealValue::NegInfinity => true,
            RealValue::Nan => false,
        }
    }

    fn is_odd(&self) -> bool {
        match self {
            RealValue::Integer{positive: _, value: i}  => i % 2 == 1,
            _ => false,
        }
    }

    fn is_even(&self) -> bool {
        match self {
            RealValue::Integer{positive: _, value: i}  => i % 2 == 0,
            _ => false,
        }
    }

    fn abs(&self) -> Self {
        match self {
            RealValue::Integer{positive: _, value: i} => RealValue::Integer{positive: true, value: i.clone()},
            RealValue::Rational{positive: _, num, den} => RealValue::Rational{positive: true, num: num.clone(), den: den.clone()},
            RealValue::Irrational(i) => RealValue::Irrational(i.abs()),
            RealValue::Infinity => RealValue::Infinity,
            RealValue::NegInfinity => RealValue::Infinity,
            RealValue::Nan => RealValue::Nan,
        }
    }

    fn square(&self) -> Self {
        match self {
            RealValue::Integer{positive: _, value: i} => RealValue::Integer{positive: true, value: i.clone() * i.clone()},
            RealValue::Rational{positive: _, num, den} => RealValue::Rational{positive: true, num: num.clone() * num.clone(), den: den.clone() * den.clone()},
            RealValue::Irrational(i) => RealValue::Irrational(i * i),
            RealValue::Infinity => RealValue::Infinity,
            RealValue::NegInfinity => RealValue::Infinity,
            RealValue::Nan => RealValue::Nan,
        }
    }

    fn sqrt(&self) -> Self {
        match self {
            RealValue::Integer{..} => {
                panic!("Cannot take square root of integer")
            },
            RealValue::Rational{..} => {
                panic!("Cannot take square root of rational")
            },
            RealValue::Irrational(i) => RealValue::Irrational(i.sqrt()),
            RealValue::Infinity => RealValue::Infinity,
            RealValue::NegInfinity => RealValue::Infinity,
            RealValue::Nan => RealValue::Nan,
        }
    }
    fn simplify(&self) -> Self {
        match self {
            RealValue::Rational{positive: _, num, den: _} if *num == 0 => RealValue::Integer{positive: true, value: 0},
            RealValue::Rational{positive: p, num, den} if *den == 1 => RealValue::Integer{positive: *p, value: num.clone()},
            RealValue::Rational{positive: _, num, den} if *num == *den => RealValue::Integer{positive: true, value: 1},
            RealValue::Rational{positive: p, num, den} => {
                let gcd = num.gcd(den.clone());

                if gcd == den.clone() {
                    RealValue::Integer{positive: *p, value: num.clone() / gcd}
                } else {
                    RealValue::Rational{positive: *p, num: num.clone() / gcd.clone(), den: den.clone() / gcd}
                }
            },
            _ => self.clone(),
        }
    }
}

impl Add for RealValue {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self.clone(), rhs.clone()) {
            // integer + integer
            (RealValue::Integer{positive: true, value: lhs}, RealValue::Integer{positive: true, value: rhs}) => RealValue::Integer{positive: true, value: lhs + rhs},
            (RealValue::Integer{positive: true, value: lhs}, RealValue::Integer{positive: false, value: rhs}) => {
                if lhs > rhs {
                    RealValue::Integer{positive: true, value: lhs - rhs}
                } else if lhs < rhs {
                    RealValue::Integer{positive: false, value: rhs - lhs}
                } else {
                    RealValue::Integer{positive: true, value: 0}
                }
            },
            (RealValue::Integer{positive: false, value: lhs}, RealValue::Integer{positive: true, value: rhs}) => {
                if lhs > rhs {
                    RealValue::Integer{positive: false, value: lhs - rhs}
                } else if lhs < rhs {
                    RealValue::Integer{positive: true, value: rhs - lhs}
                } else {
                    RealValue::Integer{positive: true, value: 0}
                }
            },
            (RealValue::Integer{positive: false, value: lhs}, RealValue::Integer{positive: false, value: rhs}) => RealValue::Integer{positive: false, value: lhs + rhs},

            // Integer + rational
            (RealValue::Integer{positive: true, value: lhs}, RealValue::Rational{positive: true, num: numerator, den: denominator}) => RealValue::Rational{positive: true, num: lhs * denominator + numerator, den: denominator},
            (RealValue::Integer{positive: true, value: lhs}, RealValue::Rational{positive: false, num: numerator, den: denominator}) => {
                if lhs * denominator > numerator {
                    RealValue::Rational{positive: true, num: lhs * denominator - numerator, den: denominator}
                } else if lhs * denominator < numerator {
                    RealValue::Rational{positive: false, num: numerator - lhs * denominator, den: denominator}
                } else {
                    RealValue::Integer{positive: true, value: 0}
                }
            },

            (RealValue::Integer{positive: false, value: lhs}, RealValue::Rational{positive: true, num: numerator, den: denominator}) => {
                if lhs * denominator > numerator {
                    RealValue::Rational{positive: false, num: lhs * denominator - numerator, den: denominator}
                } else if lhs * denominator < numerator {
                    RealValue::Rational{positive: true, num: numerator - lhs * denominator, den: denominator}
                } else {
                    RealValue::Integer{positive: true, value: 0}
                }
            },
            (RealValue::Integer{positive: false, value: lhs}, RealValue::Rational{positive: false, num: numerator, den: denominator}) => RealValue::Rational{positive: false, num: lhs * denominator + numerator, den: denominator},
            
            // Integer + Irrational
            (RealValue::Integer{positive: true, value: lhs}, RealValue::Irrational(rhs)) => RealValue::Irrational(rhs + lhs as Irrational),
            (RealValue::Integer{positive: false, value: lhs}, RealValue::Irrational(rhs)) => RealValue::Irrational(rhs - lhs as Irrational),

            // Rational + Integer
            (RealValue::Rational {..}, RealValue::Integer {..}) => rhs + self,
            // Rational + Rational
            (RealValue::Rational{positive: true, num: n1, den: d1}, RealValue::Rational{positive: true, num: n2, den: d2}) => RealValue::Rational{positive: true, num: n1 * d2 + n2 * d1, den: d1 * d2},
            (RealValue::Rational{positive: true, num: n1, den: d1}, RealValue::Rational{positive: false, num: n2, den: d2}) => {
                if n1 * d2 > n2 * d1 {
                    RealValue::Rational{positive: true, num: n1 * d2 - n2 * d1, den: d1 * d2}
                } else if n1 * d2 < n2 * d1 {
                    RealValue::Rational{positive: false, num: n2 * d1 - n1 * d2, den: d1 * d2}
                } else {
                    RealValue::Integer{positive: true, value: 0}
                }
            },
            (RealValue::Rational{positive: false, num: n1, den: d1}, RealValue::Rational{positive: true, num: n2, den: d2}) => {
                if n1 * d2 > n2 * d1 {
                    RealValue::Rational{positive: false, num: n1 * d2 - n2 * d1, den: d1 * d2}
                } else if n1 * d2 < n2 * d1 {
                    RealValue::Rational{positive: true, num: n2 * d1 - n1 * d2, den: d1 * d2}
                } else {
                    RealValue::Integer{positive: true, value: 0}
                }
            },
            (RealValue::Rational{positive: false, num: n1, den: d1}, RealValue::Rational{positive: false, num: n2, den: d2}) => RealValue::Rational{positive: false, num: n1 * d2 + n2 * d1, den: d1 * d2},

            // Rational + Irrational
            (RealValue::Rational{positive: true, num: numerator, den: denominator}, RealValue::Irrational(rhs)) => RealValue::Irrational(rhs + (numerator as Irrational/ denominator as Irrational)),
            (RealValue::Rational{positive: false, num: numerator, den: denominator}, RealValue::Irrational(rhs)) => RealValue::Irrational(rhs - (numerator as Irrational/ denominator as Irrational)),

            // Irrational + Integer
            (RealValue::Irrational(_), RealValue::Integer{..}) => rhs + self,

            // Irrational + Rational
            (RealValue::Irrational(lhs), RealValue::Rational{positive: true, num: numerator, den: denominator}) => RealValue::Irrational(lhs + (numerator as Irrational/ denominator as Irrational)),
            (RealValue::Irrational(lhs), RealValue::Rational{positive: false, num: numerator, den: denominator}) => RealValue::Irrational(lhs - (numerator as Irrational/ denominator as Irrational)),

            // Irrational + Irrational
            (RealValue::Irrational(lhs), RealValue::Irrational(rhs)) => RealValue::Irrational(lhs + rhs),
            (RealValue::Infinity, _) => RealValue::Infinity,
            (_, RealValue::Infinity) => RealValue::Infinity,
            (RealValue::NegInfinity, _) => RealValue::NegInfinity,
            (_, RealValue::NegInfinity) => RealValue::NegInfinity,
            (RealValue::Nan, _) => RealValue::Nan,
            (_, RealValue::Nan) => RealValue::Nan,
        }
        
    }
}

impl Sub for RealValue {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self + (-rhs)
    }
}

impl Neg for RealValue {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            RealValue::Integer{positive: p, value: v} => RealValue::Integer{positive: !p, value: v},
            RealValue::Rational{positive: p, num, den} => RealValue::Rational{positive: !p, num, den},
            RealValue::Irrational(i) => RealValue::Irrational(-i),
            RealValue::Infinity => RealValue::NegInfinity,
            RealValue::NegInfinity => RealValue::Infinity,
            RealValue::Nan => RealValue::Nan,
        }
    }
}

impl Mul for RealValue {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self.clone(), rhs.clone()) {
            (RealValue::Integer{positive: p1, value: v1}, RealValue::Integer{positive: p2, value: v2}) => RealValue::Integer{positive: p1 == p2, value: v1 * v2},
            (RealValue::Integer { positive: p1, value: v }, RealValue::Rational { positive: p2, num: n, den: d }) => RealValue::Rational { positive: p1 == p2, num: v * n, den: d },
            (RealValue::Integer{positive: p, value: v}, RealValue::Irrational(i)) => RealValue::Irrational(v as Irrational * i * if p {1 as Irrational} else {-1 as Irrational}),    

            (RealValue::Rational{..}, RealValue::Integer{..}) => rhs * self,
            (RealValue::Rational{positive: p1, num: n1, den: d1}, RealValue::Rational{positive: p2, num: n2, den: d2}) => RealValue::Rational{positive: p1 == p2, num: n1 * n2, den: d1 * d2},
            (RealValue::Rational{positive: p, num, den}, RealValue::Irrational(rhs)) => RealValue::Irrational(rhs * (num as Irrational/ den as Irrational) * if p {1 as Irrational} else {-1 as Irrational}),            

            (RealValue::Irrational(_), RealValue::Integer{..}) => rhs * self,
            (RealValue::Irrational(_), RealValue::Rational{..}) => rhs * self,
            (RealValue::Irrational(lhs), RealValue::Irrational(rhs)) => RealValue::Irrational(lhs * rhs),
            (RealValue::Infinity, _) => if rhs > RealValue::from(0) {RealValue::Infinity} else {RealValue::NegInfinity},
            (_, RealValue::Infinity) => if self > RealValue::from(0) {RealValue::Infinity} else {RealValue::NegInfinity},
            (RealValue::NegInfinity, _) => if rhs > RealValue::from(0) {RealValue::NegInfinity} else {RealValue::Infinity},
            (_, RealValue::NegInfinity) => if self > RealValue::from(0) {RealValue::NegInfinity} else {RealValue::Infinity},
            (RealValue::Nan, _) => RealValue::Nan,
            (_, RealValue::Nan) => RealValue::Nan,
        }
    }
}

impl Div for RealValue {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        if rhs.is_zero() {
            if self > RealValue::from(0) {
                RealValue::Infinity
            } else {
                RealValue::NegInfinity
            }
        } else {
            self * rhs.reciprocal()
        }
    }
}

impl From<Integer> for RealValue {
    fn from(i: Integer) -> Self {
        RealValue::Integer{positive: true, value: i}
    }
}

impl From<Irrational> for RealValue {
    fn from(i: Irrational) -> Self {
        RealValue::Irrational(i)
    }
}

pub type Integer = u32;
pub type Irrational = f32;
