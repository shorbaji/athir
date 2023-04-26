use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use crate::object::Object;

#[derive(Debug, Clone)]
pub struct Env {
    hashmap: HashMap<String, Rc<RefCell<Object>>>,
    parent: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            hashmap: HashMap::new(),
            parent: None,
        }
    }

    pub fn new_with_parent(parent: Rc<RefCell<Env>>) -> Env {
        Env {
            hashmap: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn lookup(&self, key: &str) -> Option<Rc<RefCell<Object>>> {
        match self.hashmap.get(key) {
            Some(value) => Some(value.clone()),
            None => match self.parent {
                Some(ref parent) => {
                    parent.borrow().lookup(key)
                },
                None => None,
            },
        }
    }

    pub fn insert(&mut self, key: String, value: Rc<RefCell<Object>>) {
        self.hashmap.insert(key, value);
    }

}

