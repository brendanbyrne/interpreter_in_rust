use std::error;
use std::fmt;
use std::result;

use crate::evaluator::environment::Object;
use crate::parser::ast;

/// Types of error the evaluator can produce
#[derive(Debug, PartialEq)]
pub enum Error {
    UnexpectedReturn(Object),
    UnsupportedNegate(Object),
    InfixTypeMismatch(ast::InfixOperator, Object, Object),
    IdNotFound(String),
    WrongNumberArgs(usize, usize),
}

/// Result type used in the parser
pub type Result<T> = result::Result<T, Error>;

impl error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Error::*;
        let error_msg = match self {
            UnexpectedReturn(obj) => format!("Unexpected {}", obj),
            UnsupportedNegate(rhs) => format!("Negate doesn't support type {:?}", rhs),
            InfixTypeMismatch(op, lhs, rhs) => format!("{:?} {} {:?}", lhs, op, rhs),
            IdNotFound(id) => format!("Identifier not found {}", id),
            WrongNumberArgs(expected, received) => format!(
                "Expected function arguments: {}, Received: {}",
                expected, received
            ),
        };
        write!(f, "{}", error_msg)
    }
}
