use std::collections::HashMap;

type ObjectPtr = i32;

#[derive(Debug, Clone)]
pub struct Env {
    hashmap: HashMap<String, ObjectPtr>,
    parent: Option<Box<Env>>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            hashmap: HashMap::new(),
            parent: None,
        }
    }

    fn new_with_parent(parent: Env) -> Env {
        Env {
            hashmap: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }

    pub fn get(&self, key: &str) -> Option<&ObjectPtr> {
        match self.hashmap.get(key) {
            Some(value) => Some(value),
            None => match self.parent {
                Some(ref parent) => parent.get(key),
                None => None,
            },
        }
    }

    pub fn set(&mut self, key: String, value: ObjectPtr) {

        self.hashmap.insert(key, value);
    }
}
