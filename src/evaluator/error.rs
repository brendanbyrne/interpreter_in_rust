use std::error;
use std::fmt;
use std::result;

use crate::evaluator::object::Object;
use crate::parser::ast;

/// Types of error the evaluator can produce
#[derive(Debug)]
pub enum Error {
    UnhandledStatement(ast::Statement),
    UnhandledExpression(ast::Expression),
    UnexpectedReturn(Object),
    UnsupportedNegate(Object),
    InfixTypeMismatch(ast::InfixOperator, Object, Object),
}

/// Result type used in the parser
pub type Result<T> = result::Result<T, Error>;

impl error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Error::*;
        let error_msg = match self {
            UnhandledStatement(statement) => {
                format!("Unhandled Statement:\n```\n{}\n```", statement)
            }
            UnhandledExpression(expression) => {
                format!("Unhandled Expression:\n```\n{}\n```", expression)
            }
            UnexpectedReturn(obj) => format!("Unexpected {}", obj),
            UnsupportedNegate(rhs) => format!("Negate doesn't support type {:?}", rhs),
            InfixTypeMismatch(op, lhs, rhs) => format!("{:?} {} {:?}", lhs, op, rhs),
        };
        write!(f, "{}", error_msg)
    }
}
