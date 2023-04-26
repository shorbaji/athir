use crate::error::Error;
use crate::object::Object;
use std::cell::RefCell;
use std::rc::Rc;

#[doc(hidden)]
pub type EvalResult = Result<Rc<RefCell<Object>>, Error>;

#[doc(hidden)]
pub type VecEvalResult = Result<Vec<Rc<RefCell<Object>>>, Error>;
