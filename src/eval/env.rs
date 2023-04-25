use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

pub type ObjectPtr = usize;

#[derive(Debug, Clone)]
pub struct Env {
    hashmap: HashMap<String, ObjectPtr>,
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

    pub fn lookup(&self, key: &str) -> Option<ObjectPtr> {
        match self.hashmap.get(key) {
            Some(value) => Some(*value),
            None => match self.parent {
                Some(ref parent) => {
                    parent.borrow().lookup(key).map(|value| value.clone())
                },
                None => None,
            },
        }
    }

    pub fn insert(&mut self, key: String, value: ObjectPtr) {
        self.hashmap.insert(key, value);
    }

}

