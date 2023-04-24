use std::collections::HashMap;
use crate::object::Object;


use crate::read::{Identifier, Expr, Keyword};
use crate::error::Error;
use crate::eval::Heap;

use std::ops::Deref;

type ObjectPtr = usize;

#[derive(Debug)]
pub struct Env<'a> {
    hashmap: HashMap<String, ObjectPtr>,
    parent: Option<Box<Env<'a>>>,
    heap: &'a mut Heap,
}

impl<'a> Env<'a> {
    pub fn new(heap: &mut Heap) -> Env {
        Env {
            hashmap: HashMap::new(),
            parent: None,
            heap: heap,
        }
    }

    // fn new_with_parent(parent: Env) -> Env {
    //     Env {
    //         hashmap: HashMap::new(),
    //         parent: Some(Box::new(parent)),
    //     }
    // }

}
