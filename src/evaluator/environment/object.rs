//! Objects to be used in the object system

use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::evaluator::environment::Env;
use crate::parser::ast;

pub trait Truth {
    fn truth(&self) -> bool;
}

/// These are the types of objects that can be represented in the object system
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Object {
    // Instructions to the interpreter
    Noop,
    Return(Box<Object>),

    // Values in the system
    Int(i128),
    Bool(bool),
    String_(String),
    Function(Vec<String>, ast::Statement, Rc<RefCell<Env>>),
}

// Special case preallocations for only possible combinations of these objects
pub const NOOP: Object = Object::Noop;
pub const TRUE: Object = Object::Bool(true);
pub const FALSE: Object = Object::Bool(false);

impl From<bool> for Object {
    fn from(value: bool) -> Self {
        Object::Bool(value)
    }
}

impl From<i128> for Object {
    fn from(value: i128) -> Self {
        Object::Int(value)
    }
}

impl From<String> for Object {
    fn from(value: String) -> Self {
        Object::String_(value)
    }
}

impl Truth for Object {
    fn truth(&self) -> bool {
        use Object::*;
        match self {
            Int(value) => value != &0,
            Bool(value) => *value,
            Return(_) => panic!("The parser should enforce that this can't be reached."),
            &NOOP => panic!("Nothing should have the value of NOOP"),
            Function(_, _, _) => panic!("This should never be allowed."),
            _ => false,
        }
    }
}

// QUESTION: Does this actually do what I think it does?
impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Object::*;
        let obj = match self {
            Noop => "".to_owned(),
            Int(value) => format!("{}", value),
            Bool(value) => format!("{}", value),
            String_(value) => value.clone(),
            Return(value) => format!("return {}", *value),
            Function(params, body, _) => format!("fn ({}) {}", params.join(", "), body),
        };
        write!(f, "{}", obj)
    }
}
