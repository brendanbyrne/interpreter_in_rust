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
}

struct Tree {
    root: Rc<RefCell<Env>>,
}
impl Tree{
    fn iter(&self) -> TreeIter {
	TreeIter::new(Rc::clone(root)
    }
}


#[derive(Default)]
struct TreeIter{
    stack: Vec<&Env>,
}

impl EnvIter<'_> {
    fn new(root: &'a Env) -> Self {
        return EnvIter { stack: vec![root] };
    }
}

// impl fmt::Display for Env {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         let scope = scope_to_string(&self.store);

//         // scope.push(&self.maybe_parent;

//         // loop {

//         // }
//         write!(f, "{}", scope)
//     }
// }

// fn env_to_string(store: &Store) -> String {
//     format!(
//         "{{{}}}",
//         store
//             .iter()
//             .map(|(k, v)| format!("{}: {}", k, v))
//             .collect::<Vec<String>>()
//             .join(", ")
//     )
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
