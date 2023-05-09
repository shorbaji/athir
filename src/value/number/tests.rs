use super::{Number, real::{Real, RealValue}};

#[test]
fn test_add_complex_complex() {
    let a = Number::Complex { real: Real::from(7), imag: Real::from(42) };
    let b = Number::Complex { real: Real::from(7), imag: Real::from(42) };

    let c = a + b;
    assert_eq!(c, Number::Complex { real: Real::from(14), imag: Real::from(84) });
}

#[test]
fn test_add_complex_integer() {
    let a = Number::Complex { real: Real::from(7), imag: Real::from(42) };
    let b = Number::from(1);

    let c = a + b;
    assert_eq!(c, Number::Complex { real: Real::from(8), imag: Real::from(42) });   
}

#[test]
fn test_add_complex_irrational() {
    let a = Number::Complex { real: Real::from(7), imag: Real::from(42) };
    let b = Number::Real(Real{exact: false, value: RealValue::from(1.0)});

    let c = a + b;

    assert!(matches!(c, Number::Complex {real: Real{..}, imag: Real{..}}));
}

#[test]
fn test_add_complex_rational() {
    let a = Number::Complex { real: Real::from(7), imag: Real::from(42) };
    let b = Number::Real(Real{exact: true, value: RealValue::Rational {positive: true, num: 1, den: 3 }});

    let c = a + b;

    assert_eq!(c, Number::Complex { real: Real{exact: true, value: RealValue::Rational {positive: true, num: 22, den: 3 }}, imag: Real::from(42) });
}
 
#[test]
fn test_add_integer_complex() {
    let a = Number::from(1);
    let b = Number::Complex { real: Real::from(7), imag: Real::from(42) };

    let c = a + b;
    assert_eq!(c, Number::Complex { real: Real::from(8), imag: Real::from(42) });
}

#[test]
fn test_add_integer_integer() {
    let a = Number::from(1);
    let b = Number::from(2);

    let c = a + b;
    assert_eq!(c, Number::from(3));
}

#[test]
fn test_add_integer_irrational() {
    let a = Number::from(1);
    let b = Number::Real(Real{exact: true, value: RealValue::from(1.0)});

    let c = a + b;

    assert!(matches!(c, Number::Real{..}));
}

#[test]
fn test_add_integer_rational() {
    let a = Number::from(1);
    let b = Number::Real(Real{exact: true, value: RealValue::Rational {positive: true, num: 1, den: 3}});

    let c = a + b;

    assert_eq!(c, Number::Real(Real{exact: true, value: RealValue::Rational {positive: true, num: 4, den: 3}}));
}

#[test]
fn test_add_irrational_complex() {
    let a = Number::Real(Real{exact: true, value: RealValue::from(1.0)});
    let b = Number::Complex { real: Real::from(7), imag: Real::from(42) };

    let c = a + b;

    assert!(matches!(c, Number::Complex {real: Real{..}, imag: Real{..}}));
}

#[test]
fn test_add_irrational_integer() {
    let a = Number::Real(Real{exact: true, value: RealValue::from(1.0)});
    let b = Number::from(1);

    let c = a + b;
    assert!(matches!(c, Number::Real(Real{exact: false, ..})));
}

#[test]
fn test_add_irrational_irrational() {
    let a = Number::Real(Real{exact: true, value: RealValue::from(1.0)});
    let b = Number::Real(Real{exact: true, value: RealValue::from(1.0)});

    let c = a + b;

    assert!(matches!(c, Number::Real(Real{exact: false, ..})))
}

#[test]
fn test_add_irrational_rational() {
    let a = Number::Real(Real{exact: true, value: RealValue::from(1.0)});
    let b = Number::Real(Real{exact: true, value: RealValue::from(1.0)});

    let c = a + b;

    assert!(matches!(c, Number::Real(Real{exact: false, ..})));
}

#[test]
fn test_add_rational_complex() {
    let a = Number::Real(Real{exact: true, value: RealValue::Rational {positive: true, num: 1, den: 3 }});
    let b = Number::Complex { real: Real::from(7), imag: Real::from(42) };

    let c = a + b;
    assert_eq!(c, Number::Complex { real: Real{exact: true, value: RealValue::Rational { positive: true, num: 22, den: 3 }}, imag: Real::from(42) });
}

#[test]
fn test_add_rational_integer() {
    let a = Number::Real(Real{exact: true, value: RealValue::Rational {positive: true, num: 1, den: 3 }});
    let b = Number::from(1);

    let c = a + b;
    assert_eq!(c, Number::Real(Real{exact: true, value: RealValue::Rational {positive: true, num: 4, den: 3}}));
}

#[test]
fn test_add_rational_irrational() {
    let a = Number::Real(Real{exact: true, value: RealValue::from(1.0)});
    let b = Number::Real(Real{exact: true, value: RealValue::from(1.0)});

    let c = a + b;
    assert!(matches!(c, Number::Real(Real{exact: false, ..})));
}

#[test]
fn test_add_rational_rational() {
    let a = Number::Real(Real{exact: true, value: RealValue::Rational { positive: true, num: 1, den: 3 }});
    let b = Number::Real(Real{exact: true, value: RealValue::Rational { positive: true, num: 7, den: 4 }});

    let c = a + b;

    assert_eq!(c, Number::Real(Real{exact: true, value: RealValue::Rational { positive: true, num: 25, den: 12 }}));
}

#[test]
fn test_div_complex_complex() {
    let a = Number::Complex { real: Real::from(7), imag: Real::from(42) };
    let b = Number::Complex { real: Real::from(1), imag: Real::from(3) };

    let c = a / b;

    assert_eq!(c, Number::Complex { real: Real{exact: true, value: RealValue::Rational{ positive: true, num: 133, den: 10 }}, imag: Real{exact: true, value: RealValue::Rational{ positive: true, num: 21, den: 10 } }});
}

#[test]
fn test_div_complex_integer() {
    let a = Number::Complex { real: Real::from(7), imag: Real::from(42) };
    let b = Number::from(7);

    let c = a / b;
    assert_eq!(c, Number::Complex { real: Real::from(1), imag: Real::from(6) });
}

#[test]
fn test_div_complex_irrational() {
    let a = Number::Complex { real: Real::from(7), imag: Real::from(42) };
    let b = Number::Real(Real{exact: true, value: RealValue::from(1.0)});

    let c = a / b;
    assert!(matches!(c, Number::Complex {
        real: Real{exact: false, ..},
        imag: Real{exact: false, ..}}));
}

#[test]
fn test_div_complex_rational() {
    let a = Number::Complex { real: Real::from(7), imag: Real::from(42) };
    let b = Number::Real(Real{exact: true, value: RealValue::Rational{ positive: true, num: 7, den: 42 }});

    let c = a / b;
    assert_eq!(c, Number::Complex { real: Real::from(42), imag: Real{exact: true, value: RealValue::from(252) }});
}

#[test]
fn test_div_integer_complex() {
    let a = Number::from(7);
    let b = Number::Complex { real: Real::from(7), imag: Real::from(42) };

    let c = a / b;

    assert_eq!(c, Number::Complex {
        real: Real {
            exact: true,
            value: RealValue::Rational {
                positive: true,
                num: 1,
                den: 37
            }
        },
        imag: Real {
            exact: true,
            value: RealValue::Rational {
                positive: false,
                num: 6,
                den: 37
            }
        }
    });
}

#[test]
fn test_div_integer_integer() {
    let a = Number::from(7);
    let b = Number::from(42);

    let c = a / b;

    assert_eq!(c, Number::Real(Real{exact: true, value: RealValue::Rational{ positive: true, num: 1, den: 6 }}));

}

#[test]
fn test_div_integer_irrational() {
    let a = Number::from(7);
    let b = Number::Real(Real{exact: true, value: RealValue::from(1.0)});

    let c = a / b;

    assert!(matches!(c, Number::Real(Real{exact: false, ..})));
}

#[test]
fn test_div_integer_rational() {
    let a = Number::from(7);
    let b = Number::Real(Real{exact: true, value: RealValue::Rational{ positive: true, num: 7, den: 42 }});

    let c = a / b;

    assert_eq!(c, Number::Real(Real{exact: true, value: RealValue::from(42)}));
}

#[test]
fn test_div_rational_irrational() {
    let a = Number::Real(Real{exact: true, value: RealValue::Rational{ positive: true, num: 7, den: 42 }});
    let b = Number::Real(Real{exact: true, value: RealValue::from(1.0)});

    let c = a / b;

    assert!(matches!(c, Number::Real(Real{exact: false, ..})));
}

#[test]
fn test_div_irrational_complex() {
    let a = Number::Real(Real::from(1.0));
    let b = Number::Complex { real: Real::from(7), imag: Real::from(42) };

    let c = a / b;

    assert!(matches!(c, Number::Complex { real: Real{exact: false, value: RealValue::Irrational(_)}, imag: Real{exact: false, value: RealValue::Irrational(_)} }));
}

#[test]
fn test_div_irrational_integer() {
    let a = Number::Real(Real{exact: true, value: RealValue::from(1.0)});
    let b = Number::from(7);

    let c = a / b;

    assert!(matches!(c, Number::Real(Real{exact: false, ..})));
}

#[test]
fn test_div_irrational_irrational() {
    let a = Number::Real(Real{exact: false, value: RealValue::from(1.0)});
    let b = Number::Real(Real{exact: false, value: RealValue::from(1.0)});

    let c = a / b;

    assert!(matches!(c, Number::Real(Real{exact: false, ..})));
}

#[test]
fn test_div_irrational_rational() {
    let a = Number::Real(Real{exact: true, value: RealValue::from(1.0)});
    let b = Number::Real(Real{exact: true, value: RealValue::Rational{ positive: true, num: 7, den: 42 }});

    let c = a / b;

    assert!(matches!(c, Number::Real(Real{exact: false, ..})));
}

#[test]
fn test_div_rational_complex() {
    let a = Number::Real(Real{exact: true, value: RealValue::Rational{ positive: true, num: 7, den: 42 }});
    let b = Number::Complex { real: Real::from(7), imag: Real::from(42) };

    let c = a / b;

    assert_eq!(c, Number::Complex {
        real: Real {
            exact: true,
            value: RealValue::Rational {
                positive: true,
                num: 1,
                den: 1554
            }
        },
        imag: Real {
            exact: true,
            value: RealValue::Rational {
                positive: false,
                num: 1,
                den: 259,
            }
        }
    });
}

#[test]
fn test_div_rational_integer() {
    let a = Number::Real(Real{exact: true, value: RealValue::Rational{ positive: true, num: 7, den: 42 }});
    let b = Number::from(7);

    let c = a / b;

    assert_eq!(c, Number::Real(Real{exact: true, value: RealValue::Rational{ positive: true, num: 1, den: 42 }}));
}

#[test]
fn test_div_rational_rational() {
    let a = Number::Real(Real{exact: true, value: RealValue::Rational{ positive: true, num: 7, den: 42 }});
    let b = Number::Real(Real{exact: true, value: RealValue::Rational{ positive: true, num: 7, den: 42 }});

    let c = a / b;

    assert_eq!(c, Number::Real(Real{exact: true, value: RealValue::from(1)}));
}

#[test]
fn test_mul_complex_complex() {
    let a = Number::Complex { real: Real::from(7), imag: Real::from(42) };
    let b = Number::Complex { real: Real::from(7), imag: Real::from(42) };

    let c = a * b;

    assert_eq!(c, Number::Complex {
        real: Real { exact: true, value: RealValue::Integer{positive: false, value: 1715} },
        imag: Real::from(588) });
}

#[test]
fn test_mul_complex_integer() {
    let a = Number::Complex { real: Real::from(7), imag: Real::from(42) };
    let b = Number::from(1);

    let c = a * b;

    assert_eq!(c, Number::Complex { real: Real::from(7), imag: Real::from(42) });
}

#[test]
fn test_mul_complex_irrational() {
    let a = Number::Complex { real: Real::from(7), imag: Real::from(42) };
    let b = Number::Real(Real{exact: true, value: RealValue::from(1.0)});

    let c = a * b;

    assert!(matches!(c, Number::Complex {real: Real{..}, imag: Real{..}}));
}

#[test]
fn test_mul_complex_rational() {
    let a = Number::Complex { real: Real::from(7), imag: Real::from(42) };
    let b = Number::Real(Real{exact: true, value: RealValue::Rational{ positive: true, num: 7, den: 42 }});

    let c = a * b;

    assert_eq!(c, Number::Complex {
        real: Real {
            exact: true,
            value: RealValue::Rational {
                positive: true,
                num: 7,
                den: 6
            },
        },
        imag: Real::from(7),
    });
}

#[test]
fn test_mul_integer_complex() {
    let a = Number::from(1);
    let b = Number::Complex { real: Real::from(7), imag: Real::from(42) };

    let c = a * b;

    assert_eq!(c, Number::Complex { real: Real::from(7), imag: Real::from(42) });
}

#[test]
fn test_mul_integer_integer() {
    let a = Number::from(1);
    let b = Number::from(42);

    let c = a * b;

    assert_eq!(c, Number::from(42));
}

#[test]
fn test_mul_integer_irrational() {
    let a = Number::from(1);
    let b = Number::Real(Real::from(1.0));

    let c = a * b;

    assert!(matches!(c, Number::Real(Real{exact: false, ..})));
}

#[test]
fn test_mul_integer_rational() {
    let a = Number::from(1);
    let b = Number::Real(Real { exact: true, value: RealValue::Rational { positive: true, num: 7, den: 42 }});

    let c = a * b;

    assert_eq!(c, Number::Real(Real{exact: true, value: RealValue::Rational { positive: true, num: 1, den: 6 }}));
}

#[test]
fn test_mul_irrational_complex() {
    let a = Number::Real(Real{exact: true, value: RealValue::from(1.0)});
    let b = Number::Complex { real: Real::from(7), imag: Real::from(42) };

    let c = a * b;

    assert!(matches!(c, Number::Complex {real: Real{..}, imag: Real{..}}));
}

#[test]
fn test_mul_irrational_integer() {
    let a = Number::Real(Real{exact: true, value: RealValue::from(1.0)});
    let b = Number::from(1);

    let c = a * b;

    assert!(matches!(c, Number::Real(Real{exact: false, ..})));
}

#[test]
fn test_mul_irrational_irrational() {
    let a = Number::Real(Real::from(1.0));
    let b = Number::Real(Real::from(1.0));

    let c = a * b;

    assert!(matches!(c, Number::Real(Real{exact: false, ..})));
}

#[test]
fn test_mul_irrational_rational() {
    let a = Number::Real(Real{exact: false, value: RealValue::from(1.0)});
    let b = Number::Real(Real{exact: true, value: RealValue::Rational{ positive: true, num: 7, den: 42 }});

    let c = a * b;

    assert!(matches!(c, Number::Real(Real{exact: false, ..})));
}

#[test]
fn test_mul_rational_complex() {
    let a = Number::Real(Real{exact: true, value: RealValue::Rational{ positive: true, num: 7, den: 42 }});
    let b = Number::Complex { real: Real::from(7), imag: Real::from(42) };

    let c = a * b;

    assert_eq!(c, Number::Complex {
        real: Real {
            exact: true,
            value: RealValue::Rational{ positive: true, num: 7, den: 6 },
        },
        imag: Real::from(7)
    });
}

#[test]
fn test_mul_rational_integer() {
    let a = Number::Real(Real{exact: true, value: RealValue::Rational { positive: true, num: 7, den: 42 }});
    let b = Number::from(1);

    let c = a * b;

    assert_eq!(c, Number::Real(Real{exact: true, value: RealValue::Rational { positive: true, num: 1, den: 6 }}));
}

#[test]
fn test_mul_rational_irrational() {
    let a = Number::Real(Real{exact: true, value: RealValue::Rational{ positive: true, num: 7, den: 42 }});
    let b = Number::Real(Real{exact: true, value: RealValue::from(1.0)});

    let c = a * b;

    assert!(matches!(c, Number::Real(Real{exact: false, ..})));
}

#[test]
fn test_mul_rational_rational() {
    let a = Number::Real(Real{exact: true, value: RealValue::Rational{ positive: true, num: 7, den: 42 }});
    let b = Number::Real(Real{exact: true, value: RealValue::Rational{ positive: true, num: 7, den: 42 }});

    let c = a * b;

    assert_eq!(c, Number::Real(Real{exact: true, value: RealValue::Rational{ positive: true, num: 1, den: 36 }}));
}

#[test]
fn test_sub_complex_complex() {
    let a = Number::Complex { real: Real::from(7), imag: Real::from(42) };
    let b = Number::Complex { real: Real::from(7), imag: Real::from(42) };

    let c = a - b;

    assert_eq!(c, Number::Complex { real: Real::from(0), imag: Real::from(0) });
}

#[test]
fn test_sub_complex_integer() {
    let a = Number::Complex { real: Real::from(7), imag: Real::from(42) };
    let b = Number::from(1);

    let c = a - b;

    assert_eq!(c, Number::Complex { real: Real::from(6), imag: Real::from(42) });
}

#[test]
fn test_sub_complex_irrational() {
    let a = Number::Complex { real: Real::from(7), imag: Real::from(42) };
    let b = Number::Real(Real{exact: true, value: RealValue::from(1.0)});

    let c = a - b;

    assert!(matches!(c, Number::Complex {real: Real{..}, imag: Real{..}}));
}

#[test]
fn test_sub_complex_rational() {
    let a = Number::Complex { real: Real::from(7), imag: Real::from(42) };
    let b = Number::Real(Real{exact: true, value: RealValue::Rational { positive: true, num: 7, den: 42 }});

    let c = a - b;

    assert_eq!(c, Number::Complex {
        real: Real {
            exact: true,
            value: RealValue::Rational { positive: true, num: 41, den: 6 }
        },
        imag: Real::from(42) });
}

#[test]
fn test_sub_integer_complex() {
    let a = Number::from(1);
    let b = Number::Complex { real: Real::from(7), imag: Real::from(42) };

    let c = a - b;

    assert_eq!(c, Number::Complex {
        real: Real{exact: true, value: RealValue::Integer { positive: false, value: 6 }},
        imag: Real{exact: true, value: RealValue::Integer { positive: false, value: 42 }},
    });
}

#[test]
fn test_sub_integer_integer() {
    let a = Number::from(1);
    let b = Number::from(1);

    let c = a - b;
    assert_eq!(c, Number::from(0));
}

#[test]
fn test_sub_integer_irrational() {
    let a = Number::from(1);
    let b = Number::Real(Real{exact: true, value: RealValue::from(1.0)});

    let c = a - b;

    assert!(matches!(c, Number::Real(Real{exact: false, ..})));
}

#[test]
fn test_sub_integer_rational() {
    let a = Number::from(1);
    let b = Number::Real(Real{exact: true, value: RealValue::Rational { positive: true, num: 1, den: 4 }});

    let c = a - b;

    assert_eq!(c, Number::Real(Real{exact: true, value: RealValue::Rational { positive: true, num: 3, den: 4 }}));
}

#[test]
fn test_sub_irrational_complex() {
    let a = Number::Real(Real{exact: false, value: RealValue::from(1.0)});
    let b = Number::Complex { real: Real::from(7), imag: Real::from(42) };

    let c = a - b;

    assert!(matches!(c, Number::Complex {real: Real{..}, imag: Real{..}}));
}

#[test]
fn test_sub_irrational_integer() {
    let a = Number::Real(Real{exact: false, value: RealValue::from(1.0)});
    let b = Number::from(1);

    let c = a - b;

    assert!(matches!(c, Number::Real(Real{exact: false, ..})));
}

#[test]
fn test_sub_irrational_irrational() {
    let a = Number::Real(Real{exact: true, value: RealValue::from(1.0)});
    let b = Number::Real(Real{exact: true, value: RealValue::from(1.0)});

    let c = a - b;

    assert!(matches!(c, Number::Real(Real{exact: false, ..})));
}

#[test]
fn test_sub_irrational_rational() {
    let a = Number::Real(Real{exact: false, value: RealValue::from(1.0)});
    let b = Number::Real(Real{exact: true, value: RealValue::Rational { positive: true, num: 7, den: 42 }});

    let c = a - b;

    assert!(matches!(c, Number::Real(Real{exact: false, ..})));
}

#[test]
fn test_sub_rational_complex() {
    let a = Number::Real(Real{exact: true, value: RealValue::Rational { positive: true, num: 7, den: 35 }});
    let b = Number::Complex {
        real: Real {
            exact: true,
            value: RealValue::Rational { positive: false, num: 34, den: 5 }
        },
        imag: Real::from(42)
    };

    let c = a - b;

    assert_eq!(c, Number::Complex { 
        real: Real { 
            exact: true,
            value: RealValue::Integer{positive: true, value: 7},
        },
        imag: Real { 
            exact: true,
            value: RealValue::Integer{positive: false, value: 42},
        }
    });
}

#[test]
fn test_sub_rational_integer() {
    let a = Number::Real(Real{exact: true, value: RealValue::Rational { positive: true, num: 7, den: 42 }});
    let b = Number::from(1);

    let c = a - b;

    assert_eq!(c, Number::Real(Real{
        exact: true,
        value: RealValue::Rational { positive: false, num: 5, den: 6 }
    }));
}

#[test]
fn test_sub_rational_irrational() {
    let a = Number::Real(Real{exact: true, value: RealValue::Rational {positive: true, num: 7, den: 35 }});
    let b = Number::from(2.0);

    let c = a - b;

    assert!(matches!(c, Number::Real(Real{exact: false, ..})));
}

#[test]
fn test_sub_rational_rational() {
    let a = Number::Real(Real{exact: true, value: RealValue::Rational { positive: true, num: 7, den: 42 }});
    let b = Number::Real(Real{exact: true, value: RealValue::Rational { positive: true, num: 7, den: 42 }});

    let c = a - b;

    assert_eq!(c, Number::Real(Real{exact: true, value: RealValue::from(0)}));
}

#[test]
fn test_eq_complex_integer() {
    let a = Number::Complex { real: Real::from(1), imag: Real::from(0) };
    let b = Number::from(1);

    assert_eq!(a, b);
}

#[test]
fn test_eq_complex_rational() {
    let a = Number::Complex { real: Real::from(1), imag: Real::from(0) };
    let b = Number::Real(Real{exact: true, value: RealValue::Rational { positive: true, num: 1, den: 1 }});

    assert_eq!(a, b);
}

#[test]
fn test_eq_integer_complex() {
    let a = Number::from(1);
    let b = Number::Complex { real: Real::from(1), imag: Real::from(0) };

    assert_eq!(a, b);    
}

#[test]
fn test_eq_integer_rational() {
    let a = Number::from(1);
    let b = Number::Real(Real{exact: true, value: RealValue::Rational { positive: true, num: 1, den: 1 }});

    assert_eq!(a, b);
}

#[test]
fn test_eq_rational_complex() {
    let a = Number::Real(Real{exact: true, value: RealValue::Rational { positive: true, num: 1, den: 1 }});
    let b = Number::Complex { real: Real::from(1), imag: Real::from(0) };

    assert_eq!(a, b);
}

#[test]
fn test_eq_rational_integer() {
    let a = Number::Real(Real{exact: true, value: RealValue::Rational { positive: true, num: 1, den: 1 }});
    let b = Number::from(1);

    assert_eq!(a, b);
}

