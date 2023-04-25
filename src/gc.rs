use crate::object::Object;

pub type ObjectPtr = usize;

pub trait GC {
    fn new() -> Self;
    fn alloc(&mut self, object: Box<Object>) -> ObjectPtr;
    fn write(&mut self, object_ptr: ObjectPtr, object: Box<Object>);
    fn read(&self, object_ptr: ObjectPtr) -> &Object;
}

#[derive(Clone, Debug)]
pub struct NoGC {
    heap: Vec<Object>,
}

impl GC for NoGC {
    fn new() -> NoGC {
        NoGC {
            heap: Vec::new(),
        }
    }

    fn alloc(&mut self, object: Box<Object>) -> ObjectPtr {
        self.heap.push(*object.clone()); // currently no garbage collection
        self.heap.len() - 1
    }

    fn write(&mut self, object_ptr: ObjectPtr, object: Box<Object>) {
        self.heap[object_ptr] = *object.clone();
    }

    fn read(&self, object_ptr: ObjectPtr) -> &Object {
        &self.heap[object_ptr]
    }

}



