//! Objects to be used in the object system

/// These are the types of objects that can be represented in the object system
#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    Null,
    Int(i128),
    Bool(bool),
    Return(Box<Object>),
}

// QUESTION: Does this actually do what I think it does?
// Special case preallocations for only possible combinations of these objects
pub const NULL: Object = Object::Null;
pub const TRUE: Object = Object::Bool(true);
pub const FALSE: Object = Object::Bool(false);

impl ToString for Object {
    fn to_string(&self) -> String {
        use Object::*;
        // QUESTION: Is there a way to do like....
        //     Any(value) => format!("{}", value)
        // for any type except Null?
        match self {
            Null => "null".to_owned(),
            Int(value) => format!("{}", value),
            Bool(value) => format!("{}", value),
            Return(value) => format!("return {}", (*value).to_string()),
        }
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

pub fn is_truthy(obj: &Object) -> bool {
    match obj {
        &NULL => false,
        Object::Int(value) => value != &0,
        Object::Bool(value) => *value,
        Object::Return(_) => panic!("The parser should enforce that this can't be reached."),
    }
}
