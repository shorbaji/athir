use crate::object::{ Object, Boolean, Value, AthirError};
use std::rc::Rc;
use std::cell::RefCell;

pub trait Port {
    fn new() -> Object;
    fn as_port(&self) -> Result<Object, Object>;
    fn is_port(&self) -> Result<Object, Object>;

    // fn is_input_port(&self) -> Result<Object, Object>;
    // fn is_output_port(&self) -> Result<Object, Object>;
    // fn is_textual_port(&self) -> Result<Object, Object>;
    // fn is_binary_port(&self) -> Result<Object, Object>;
    // fn is_port(&self) -> Result<Object, Object>;
    // fn is_input_port_open(&self) -> Result<Object, Object>;
    // fn is_output_port_open(&self) -> Result<Object, Object>;
    // fn open_input_file() -> Result<Object, Object>;
    // fn open_binary_input_file() -> Result<Object, Object>;
    // fn open_output_file() -> Result<Object, Object>;
    // fn open_binary_output_file() -> Result<Object, Object>;
    // fn close_port() -> Result<Object, Object>;
    // fn close_input_port() -> Result<Object, Object>;
    // fn close_output_port() -> Result<Object, Object>;
    // fn open_input_string() -> Result<Object, Object>;
    // fn open_output_string() -> Result<Object, Object>;
    // fn get_output_string() -> Result<Object, Object>;
    // fn open_input_bytevector() -> Result<Object, Object>;
    // fn open_output_bytevector() -> Result<Object, Object>;
    // fn get_output_bytevector() -> Result<Object, Object>;
    // fn read() -> Result<Object, Object>;
    // fn read_char() -> Result<Object, Object>;
    // fn peek_char() -> Result<Object, Object>;
    // fn read_line() -> Result<Object, Object>;
    // fn is_eof_object() -> Result<Object, Object>;
    // fn eof_object() -> Result<Object, Object>;
    // fn is_char_ready() -> Result<Object, Object>;
    // fn read_string(k: Object, port: Object) -> Result<Object, Object>;
    // fn read_u8(port: Object) -> Result<Object, Object>;
    // fn peek_u8(port: Object) -> Result<Object, Object>;
    // fn is_u8_ready(port: Object) -> Result<Object, Object>;
    // fn read_bytevector(k: Object, port: Object) -> Result<Object, Object>;
    // fn read_bytevector_mut(k: Object, port: Object, start: Object, end: Object) -> Result<Object, Object>;
    // fn write(object: Object, port: Object) -> Result<Object, Object>;
    // fn write_shared(object: Object, port: Object) -> Result<Object, Object>;
    // fn write_simple(object: Object, port: Object) -> Result<Object, Object>;
    // fn display(object: Object, port: Object) -> Result<Object, Object>;
    // fn newline(port: Object) -> Result<Object, Object>;
    // fn write_char(object: Object, port: Object) -> Result<Object, Object>;
    // fn write_string(object: Object, port: Object, start: Object, end: Object) -> Result<Object, Object>;
    // fn write_u8(object: Object, port: Object) -> Result<Object, Object>;
    // fn write_bytevector(object: Object, port: Object, start: Object, end: Object) -> Result<Object, Object>;
    // fn flush_output_port(port: Object) -> Result<Object, Object>;
}

impl Port for Object {
    fn new() -> Object {
        Object {
            value: Rc::new(RefCell::new(Value::Port)),
        }
    }

    fn is_port(&self) -> Result<Object, Object> {
        Ok(<Object as Boolean>::new(self.as_port().is_ok()))
    }

    fn as_port(&self) -> Result<Object, Object> {
        match *self.borrow() {
            Value::Port => Ok(self.clone()),
            _ => Err(<Object as AthirError>::new("not a port".to_string())),
        }
    }
}