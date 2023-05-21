//! Manages the values of different identifiers

use std::collections::HashMap;

pub mod object;
pub use object::{Object, FALSE, NULL, TRUE};

pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
        }
    }
}
