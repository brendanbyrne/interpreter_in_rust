//! Manages the values of different identifiers

use std::collections::HashMap;

pub mod object;
pub use object::{Object, FALSE, NOOP, NULL, TRUE};

use crate::evaluator::{Error, Result};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
        }
    }

    // QUESTION: Is there a way to make the getter and setter work with both
    //           String and &str?
    pub fn get(&self, id: String) -> Result<Object> {
        if let Some(obj) = self.store.get(&id) {
            // QUESTION: Is there a better type to store so I'm not cloning?
            //           Box?
            Ok(obj.clone())
        } else {
            Err(Error::IdNotFound(id))
        }
    }

    pub fn set(&mut self, id: String, obj: Object) {
        self.store.insert(id, obj);
    }
}
