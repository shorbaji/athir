use crate::object::Object;
use crate::error::Error;

#[doc(hidden)]
pub type AthirResult = Result<Box<Object>, Error>;

#[doc(hidden)]
pub type VecResult = Result<Vec<Box<Object>>, Error>;
