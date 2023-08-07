//! Manages the values of different identifiers

pub mod object;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::iter::Iterator;
use std::rc::Rc;

use crate::evaluator::error::{Error, Result};
pub use object::{Object, FALSE, NOOP, TRUE};

#[cfg(test)]
mod tests;

type Store = HashMap<String, Object>;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Env {
    store: Store,
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

    // pub fn iter(&self) -> EnvIter {
    //     EnvIter::new(self)
    // }

    fn traverse(&self, f: impl FnMut(usize, &Store)) {
        self.traverse_with_depth(0, *&f);
    }

    fn traverse_with_depth(&self, depth: usize, f: impl FnMut(usize, &Store)) {
        f(depth, &self.store);

        if let Some(parent) = self.maybe_parent {
            parent.borrow().traverse_with_depth(depth + 1, *&f);
        }
    }
}

fn store_to_string(store: &Store) -> String {
    format!(
        "{{{}}}",
        store
            .iter()
            .map(|(k, v)| format!("{}: {}", k, v))
            .collect::<Vec<String>>()
            .join(", ")
    )
}

//////////////////
// Experimenting with iterators
//////////////////
// struct EnvIter<'a> {
//     env: &'a Env,
// }

// impl<'a> EnvIter<'a> {
//     fn new(env: &'a Env) -> Self {
//         EnvIter { env }
//     }
// }

// impl<'a> Iterator for EnvIter<'a> {
//     type Item = &'a Store;

//     fn next(&mut self) -> Option<Self::Item> {
//         let store = &self.env.store;

//         if let Some(parent) = &self.env.maybe_parent {
//             self.env = &parent.borrow();
//         }

//         Some(store)
//     }
// }

// impl fmt::Display for Env {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         let full_scope = self
//             .iter()
//             .enumerate()
//             .map(|(i, store)| format!("{}{{{}}}", "  ".repeat(i), store_to_string(store)))
//             .collect::<Vec<String>>()
//             .join("\n");

//         write!(f, "{}", full_scope)
//     }
// }

// #[cfg(test)]
// mod test {
//     use super::*;

//     #[test]
//     fn display() {
//         let mut env = Env::default();
//         env.set("hello".to_owned(), Object::Int(1));
//         assert_eq!(format!("{}", env), "{hello: 1}");
//     }
// }
