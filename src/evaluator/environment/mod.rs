//! Manages the values of different identifiers

use std::collections::HashMap;
use std::rc::Rc;

pub mod object;
pub use object::{Object, FALSE, NOOP, TRUE};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Env {
    scopes: Vec<HashMap<String, Rc<Object>>>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn get(&self, id: String) -> Option<&Rc<Object>> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(&id) {
                return Some(value);
            }
        }
        None
    }

    pub fn set(&mut self, id: String, obj: Rc<Object>) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(id, obj);
        }
    }

    pub fn nest(&self) -> Self {
        let mut nested = self.clone();
        nested.scopes.push(HashMap::new());
        nested
    }

    pub fn clear_top_scope(&mut self) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.clear();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ops::Deref;

    #[test]
    fn clone_consistency() {
        let mut env = Env::new();

        let obj = Rc::new(Object::Int(0));

        env.set("x".to_owned(), Rc::clone(&obj));
        assert_eq!(env.get("x".to_owned()).unwrap().deref(), obj.deref());

        let env2 = env.clone();
        assert_eq!(env.get("x".to_owned()).unwrap().deref(), obj.deref());

        env.set("y".to_owned(), Rc::clone(&obj));
        assert_eq!(env2.get("y".to_owned()), None);
    }

    #[test]
    fn nest_consistency() {
        let mut env = Env::new();

        let obj = Rc::new(Object::Int(0));

        env.set("x".to_owned(), Rc::clone(&obj));

        let mut nested = env.nest();
        nested.set("x".to_owned(), Rc::clone(&obj));

        assert_eq!(env.scopes.len(), 1);
        assert_eq!(nested.scopes.len(), 2);
        assert_eq!(nested.scopes.first().unwrap().len(), 1);
        assert_eq!(nested.scopes.last().unwrap().len(), 1);

        nested.clear_top_scope();
        assert!(nested.scopes.last().unwrap().is_empty());
    }
}
