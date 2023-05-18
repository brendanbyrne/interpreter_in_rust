//! The Object System used to evaluate Monkey

/// These are the types of objects that can be represented in the object system
#[derive(Debug)]
pub enum Object {
    Null,
    Int(i128),
    Bool(bool),
}
