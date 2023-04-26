use crate::object::Object;
use std::borrow::BorrowMut;
use std::rc::Rc;
use std::cell::RefCell;

pub struct GC {
}

impl GC {
    pub fn new() -> Self {
        GC {}
    }

    pub fn alloc(&mut self, object: &Object) -> Rc<RefCell<Object>> {
        Rc::new(RefCell::new(object.clone()))
    }

    pub fn write(&mut self, mut ptr: Rc<RefCell<Object>>, object: &Object) {
        *ptr.borrow_mut() = Rc::new(RefCell::new(object.clone()));
    }
    
    pub fn read(&self, ptr: Rc<RefCell<Object>>) -> Object {
        ptr.borrow().clone()
    }

}



