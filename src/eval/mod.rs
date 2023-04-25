/// eval.rs
///
/// The evaluator module implements the evaluator for Athir
/// [Currently a work in progress]
/// 

pub mod env; // Environment module - pub so lambda objects can use it

use std::rc::Rc;
use std::cell::RefCell;

use crate::error::Error;
use crate::object::{Object, Procedure};
use crate::read::{Identifier, Keyword};
use crate::result::AthirResult;
use crate::eval::env::Env;
use crate::gc::GC;


#[derive(Debug)]
pub struct Eval<T: GC> {
    heap: T,
    global_env: Rc<RefCell<Env>>,
}

impl<T> Eval<T> where T: GC {
    pub fn new() -> Eval<T> {
        Eval {
            heap: T::new(),
            global_env: Rc::new(RefCell::new(Env::new())),
        }
    }

    pub fn eval_in_global_env(&mut self, expr: &Box<Object>) -> AthirResult {
        self.eval(expr, self.global_env.clone())
    }

    pub fn eval(&mut self, expr: &Box<Object>, env: Rc<RefCell<Env>>) -> AthirResult {
        match &**expr {
            Object::Identifier(identifier) => self.eval_identifier(identifier, env),
            Object::Boolean(_) 
            | Object::Bytevector(_)
            | Object::Character(_)
            | Object::Number(_)
            | Object::String(_)
            | Object::Vector(_) => Ok(expr.clone()),
            Object::Pair(car, cdr) => {
                match &**car {
                    Object::Identifier(Identifier::Keyword(keyword)) => match keyword {
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
    
    fn eval_if(&mut self, expr: &Box<Object>, env: Rc<RefCell<Env>>) -> AthirResult {
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

    fn eval_lambda(&mut self, expr: &Box<Object>, env: Rc<RefCell<Env>>) -> AthirResult {
        let formals = expr.car()?;
        let body = expr.cadr()?;

        Ok(Box::new(Object::Procedure(Procedure::Lambda(env, formals.clone(), body.clone()))))
    }

    fn evlis(&mut self, operands: &Box<Object>, _env: Rc<RefCell<Env>>) -> AthirResult {
        Ok(operands.clone())
    }

    fn eval_procedure_call(&mut self, operator: &Box<Object>, operands: &Box<Object>, env: Rc<RefCell<Env>>) -> AthirResult {
        let operator = self.eval(operator, Rc::clone(&env))?;
        let operands = self.evlis(operands, Rc::clone(&env))?;

        match *operator {
            Object::Procedure(procedure) => match procedure {
                Procedure::Lambda(env, formals, body) => {
                    let mut new_env = Env::new_with_parent(Rc::clone(&env));
                    let mut formals = formals;
                    let mut operands = operands;

                    while !formals.is_null() {
                        let formal = match &**formals.car()? {
                            Object::Identifier(Identifier::Variable(formal)) => formal,
                            _ => return Err(Error::EvalError("not implemented".to_string())),
                        };

                        let operand = operands.car()?;
                        let ptr = self.heap.alloc(operand.clone());
                        new_env.insert(formal.clone(), ptr);
                        formals = formals.cdr()?.clone();
                        operands = operands.cdr()?.clone();
                    }

                    self.eval(body.car()?, Rc::new(RefCell::new(new_env)))
                }
                _ => Err(Error::EvalError("not implemented".to_string())),
            }
            _ => Err(Error::EvalError("not implemented".to_string())),
        }

    }
    
    fn eval_define(&mut self, expr: &Box<Object>, env: Rc<RefCell<Env>>) -> AthirResult {
        let symbol = match &**expr.car()? {
            Object::Pair(_, _) => Err(Error::EvalError("not implemented".to_string())),
            Object::Identifier(Identifier::Variable(symbol)) => Ok(symbol),
            _ => Err(Error::EvalError("unknown error".to_string())),
        }?;

        let value = self.eval(expr.cadr()?, env.clone())?;

        let ptr = self.heap.alloc(value);

        env.clone().borrow_mut().insert(symbol.clone(), ptr);

        Ok(Box::new(Object::Unspecified))
    }
    
    fn eval_assignment(&mut self, expr: &Box<Object>, env: Rc<RefCell<Env>>) -> AthirResult {
        let symbol = match &**expr.car()? {
            Object::Identifier(Identifier::Variable(symbol)) => Ok(symbol),
            _ => Err(Error::EvalError("unknown error".to_string())),
        }?;

        let ptr = env.borrow().lookup(symbol).and_then(|ptr| Some(ptr)).ok_or(Error::EvalError("symbol not found".to_string()))?;

        let value = self.eval(expr.cadr()?, env)?;

        self.heap.write(ptr, value);

        Ok(Box::new(Object::Null))
    }
    
    fn eval_begin(&mut self, _expr: &Box<Object>, _env: Rc<RefCell<Env>>) -> AthirResult {
        Ok(Box::new(Object::Null))
    }
    
    fn eval_quotation(&mut self, _expr: &Box<Object>, _env: Rc<RefCell<Env>>) -> AthirResult {
        Ok(Box::new(Object::Null))
    }
    
    fn eval_identifier(&mut self, id: &Identifier, env: Rc<RefCell<Env>>) -> AthirResult {
        if let Identifier::Variable(s) = id {
            if let Some(ptr) = env.borrow().lookup(s) {
                return Ok(Box::new(self.heap.read(ptr).clone()));
            } else {
                return Err(Error::EvalError("unknown".to_string()));
            }
        } else {
            return Err(Error::EvalError("unexpected keyword".to_string()));
        }
    }
}
