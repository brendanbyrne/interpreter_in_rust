//! Manages the values of different identifiers

pub mod object;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::evaluator::error::{Error, Result};
pub use object::{Object, Truth, FALSE, NOOP, TRUE};

#[cfg(test)]
mod tests;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Env {
    store: HashMap<String, Object>,
    maybe_parent: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new_with_parent(parent: Rc<RefCell<Env>>) -> Self {
        Self {
            store: HashMap::new(),
            maybe_parent: Some(parent),
        }
    }

    pub fn get(&self, id: &String) -> Result<Object> {
        match self.store.get(id) {
            Some(value) => Ok(value.clone()),
            None => match &self.maybe_parent {
                Some(parent) => parent.borrow().get(id),
                None => Err(Error::IdNotFound(id.clone())),
            },
        }
    }

    pub fn set(&mut self, id: String, value: Object) {
        // Return what was set? -> value.clone()
        self.store.insert(id, value);
    }
}
