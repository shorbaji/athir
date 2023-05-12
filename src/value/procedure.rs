use crate::alloc::R;

pub enum Procedure {
    Closure { formals: R, body: R, env: R },

    Continuation {
        f: fn(&R, &R, &R) -> (R, R), // continuation function (ternary)
        r: R, // captured environment
        k: R  // captured continuation
    },

    ContinuationPlus {
        f: fn(&R, &R, &R, &R) -> (R, R), // continuation function (quadrenary)
        o: R, // captured variable
        r: R, // captured environment
        k: R  // captured continuation
    },

    ContinuationNull,
    PrimitiveUnary(fn(&R)->R, String),
    PrimitiveBinary(fn(&R, &R)->R, String),
    PrimitiveVariadic(fn(&R)->R, String),
    PrimitiveERK(fn(&R, &R, &R)->(R, R), String),
    PrimitiveOptionalUnary(fn(Option<&R>)->R, String),
}

impl std::fmt::Debug for Procedure {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Procedure::Closure { formals, body, env } => write!(f, "Closure({:?}, {:?}, {:?})", formals, body, env),
            Procedure::Continuation { f:_, r:_, k:_ } => write!(f, "Continuation"),
            Procedure::ContinuationPlus { f:_, o:_, r:_, k:_ } => write!(f, "ContinuationPlus"),
            Procedure::ContinuationNull => write!(f, "ContinuationNull"),
            Procedure::PrimitiveUnary(_, s) => write!(f, "PrimitiveUnary({})", s),
            Procedure::PrimitiveBinary(_, s) => write!(f, "PrimitiveBinary({})", s),
            Procedure::PrimitiveVariadic(_, s) => write!(f, "PrimitiveVariadic({})", s),
            Procedure::PrimitiveERK(_, s) => write!(f, "Primitive({})", s),
            Procedure::PrimitiveOptionalUnary(_, s) => write!(f, "PrimitiveOptionalUnary({})", s),
        }
    }
}

