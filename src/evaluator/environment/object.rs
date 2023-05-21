//! Objects to be used in the object system

use std::fmt;

/// These are the types of objects that can be represented in the object system
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Object {
    Noop, // Special intruction to the interpreter to do nothing
    Null,
    Int(i128),
    Bool(bool),
    Return(Box<Object>),
}

// QUESTION: Does this actually do what I think it does?
// Special case preallocations for only possible combinations of these objects
pub const NOOP: Object = Object::Noop;
pub const NULL: Object = Object::Null;
pub const TRUE: Object = Object::Bool(true);
pub const FALSE: Object = Object::Bool(false);

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Object::*;
        let obj = match self {
            Noop => "".to_owned(),
            Null => "null".to_owned(),
            Int(value) => format!("{}", value),
            Bool(value) => format!("{}", value),
            Return(value) => format!("return {}", *value),
        };
        write!(f, "{}", obj)
    }
}

/// Retuns a pair of ints if the extraction succeeded
///
/// Attempt to extract the integers out of two objects
pub fn get_infix_ints(lhs: Object, rhs: Object) -> Option<(i128, i128)> {
    if let Object::Int(lhs_value) = lhs {
        if let Object::Int(rhs_value) = rhs {
            Some((lhs_value, rhs_value))
        } else {
            None
        }
    } else {
        None
    }
}

/// An approximate truth evaluator
///
/// false, NULL, and 0 are false
pub fn is_truthy(obj: &Object) -> bool {
    match obj {
        &NULL => false,
        Object::Int(value) => value != &0,
        Object::Bool(value) => *value,
        Object::Return(_) => panic!("The parser should enforce that this can't be reached."),
        &NOOP => panic!("Nothing should have the value of NOOP"),
    }
}
