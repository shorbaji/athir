/// Tokens base on R7RS
/// Uses [Logos](https://docs.rs/logos/0.11.0/logos/) to generate a lexer 
///

pub mod nondecimal;
pub mod decimal;

pub use nondecimal::{parse_number, NonDecimalNumberToken};
