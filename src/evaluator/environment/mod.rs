//! Manages the values of different identifiers

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub mod object;
pub use object::{Object, FALSE, NOOP, TRUE};

use crate::evaluator::error::{Error, Result};

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
            Some(value) => return Ok(value.clone()),
            None => match &self.maybe_parent {
                Some(parent) => return parent.borrow().get(id),
                None => return Err(Error::IdNotFound(id.clone())),
            },
        }
    }

    pub fn set(&mut self, id: String, value: Object) {
        // Return what was set? -> value.clone()
        self.store.insert(id, value);
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn env() {
        let root = Rc::new(RefCell::new(Env::default()));
        let child = Env::new_with_parent(Rc::clone(&root));

        let id = String::from("x");
        let value = Object::Int(1);

        root.borrow_mut().set(id.clone(), value.clone());

        assert_eq!(child.get(&id).unwrap(), value);
    }
}
