/// eval.rs
///
/// The evaluator module implements the evaluator for Athir
/// [Currently a work in progress]
/// 

pub mod env; // Environment module - pub so lambda objects can use it

use std::ops::Deref;
use std::rc::Rc;
use std::cell::RefCell;

use crate::error::Error;
use crate::object::{Object, Expr, Procedure};
use crate::read::{Identifier, Keyword};
use crate::AthirResult;
use crate::eval::env::{Env, ObjectPtr};

#[derive(Debug)]
pub struct Eval {
    heap: Vec<Object>,
    global_env: Rc<RefCell<Env>>,
}

impl Eval{
    pub fn new() -> Eval {
        Eval {
            heap: Vec::new(),
            global_env: Rc::new(RefCell::new(Env::new())),
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

    pub fn eval_in_global_env(&mut self, expr: &Box<Expr>) -> AthirResult {
        self.eval(expr, self.global_env.clone())
    }

    pub fn eval(&mut self, expr: &Box<Expr>, env: Rc<RefCell<Env>>) -> AthirResult {
        match &**expr {
            Expr::Identifier(identifier) => self.eval_identifier(identifier, env),
            Expr::Boolean(_) 
            | Expr::Bytevector(_)
            | Expr::Character(_)
            | Expr::Number(_)
            | Expr::String(_)
            | Expr::Vector(_) => Ok(expr.clone()),
            Expr::Pair(car, cdr) => {
                match &**car {
                    Expr::Identifier(Identifier::Keyword(keyword)) => match keyword {
                        Keyword::Define => self.eval_define(cdr, env),
                        Keyword::Set => self.eval_assignment(cdr, env),
                        Keyword::Begin => self.eval_begin(cdr, env),
                        Keyword::Quote => self.eval_quotation(cdr, env),
                        Keyword::If => self.eval_if(cdr, env),
                        Keyword::Lambda => self.eval_lambda(cdr, env),
                        _ => Err(Error::EvalError("not implemented".to_string())),
                    }
                    _ =>self. eval_procedure_call(car, cdr, env), }
            }
            _ => Ok(Box::new(Object::Null)),
        }
    }
    
    fn eval_if(&mut self, expr: &Box<Expr>, env: Rc<RefCell<Env>>) -> AthirResult {
        let test = expr.car()?;

        // check if test is true
        // if true, evaluate consequent
        // if false check if there is an alternative and evaluate it, otherwise return unspecified
        
        match self.eval(test, Rc::clone(&env)) {
            Ok(expr) if expr.is_true() => self.eval(expr.cadr()?, env),
            _ => expr.caddr()
                    .and_then(|expr| self.eval(expr, env))
                    .or_else(|_| Ok(Box::new(Object::Unspecified)))    
        }
    }

    fn eval_lambda(&mut self, expr: &Box<Expr>, env: Rc<RefCell<Env>>) -> AthirResult {
        let formals = expr.car()?;
        let body = expr.cadr()?;

        Ok(Box::new(Object::Procedure(Procedure::Lambda(env, formals.clone(), body.clone()))))
    }

    fn eval_procedure_call(&mut self, _operator: &Box<Expr>, _operands: &Box<Expr>, _env: Rc<RefCell<Env>>) -> AthirResult {
        // let children = node.children().unwrap();
        // let operator = eval(&children[0])?;
        // let operands = &children[1..].into_iter().map(eval);
    
        // println!("operator: {:?}", operator);
        // println!("operands: {:?}", operands);
    
        // apply(operator, operands)
        Err(Error::EvalError("eval error".to_string()))
    }
    
    // fn apply(operator: Expr, operands: impl Iterator<Item = Result<Expr, Error>>) -> Result<Expr, Error> {
    //     println!("apply");
    //     Ok(Box::new(Object::Null))
    // }
    
    fn eval_define(&mut self, expr: &Box<Expr>, env: Rc<RefCell<Env>>) -> AthirResult {
        let symbol = match expr.car()?.deref() {
            Object::Pair(_, _) => Err(Error::EvalError("not implemented".to_string())),
            Object::Identifier(Identifier::Variable(symbol)) => Ok(symbol),
            _ => Err(Error::EvalError("unknown error".to_string())),
        }?;

        let value = self.eval(expr.cadr()?, env.clone())?;

        let ptr = self.alloc(value);

        env.clone().borrow_mut().insert(symbol.clone(), ptr);

        Ok(Box::new(Object::Unspecified))
    }
    
    fn eval_assignment(&mut self, expr: &Box<Expr>, env: Rc<RefCell<Env>>) -> AthirResult {
        let symbol = match expr.car()?.deref() {
            Object::Identifier(Identifier::Variable(symbol)) => Ok(symbol),
            _ => Err(Error::EvalError("unknown error".to_string())),
        }?;

        let ptr = env.borrow().lookup(symbol).and_then(|ptr| Some(ptr)).ok_or(Error::EvalError("symbol not found".to_string()))?;

        let value = self.eval(expr.cadr()?, env)?;

        self.write(ptr, value);

        Ok(Box::new(Object::Null))
    }
    
    fn eval_begin(&mut self, _expr: &Box<Expr>, _env: Rc<RefCell<Env>>) -> AthirResult {
        Ok(Box::new(Object::Null))
    }
    
    fn eval_quotation(&mut self, _expr: &Box<Expr>, _env: Rc<RefCell<Env>>) -> AthirResult {
        Ok(Box::new(Object::Null))
    }
    
    fn eval_identifier(&mut self, id: &Identifier, env: Rc<RefCell<Env>>) -> AthirResult {
        if let Identifier::Variable(s) = id {
            if let Some(ptr) = env.borrow().lookup(s) {
                return Ok(Box::new(self.read(ptr).clone()));
            } else {
                return Err(Error::EvalError("unknown".to_string()));
            }
        } else {
            return Err(Error::EvalError("unexpected keyword".to_string()));
        }
    }
}
