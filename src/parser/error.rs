use std::error;
use std::fmt;
use std::result;

use crate::parser::Token;

/// Types of error the parser can produce
#[derive(Debug)]
pub enum Error {
    ParsingFailed(String),
    PeekMismatch(Token, Token),
    NoPrefixDefined(Token),
    AsIntFailed(String),
}

/// Result type used in the parser
pub type Result<T> = result::Result<T, Error>;

impl error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Error::*;
        let error_msg = match self {
            ParsingFailed(full_msg) => full_msg.clone(),
            PeekMismatch(expected, found) => {
                format!(
                    "Expected next token to be {:?}, got {:?} instead.",
                    expected, found
                )
            }
            NoPrefixDefined(token) => format!("No prefix parser is defined for {:?}", token),
            AsIntFailed(input) => format!("Could not parse '{}' as an integer", input),
        };
        write!(f, "{}", error_msg)
    }
}
