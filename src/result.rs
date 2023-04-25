use crate::object::Object;
use crate::error::Error;

#[doc(hidden)]
pub type AthirResult = std::result::Result<Box<Object>, Error>;

#[doc(hidden)]
pub type AthirVecResult = std::result::Result<Vec<Box<Object>>, Error>;
