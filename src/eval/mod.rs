/// eval.rs
///
/// The evaluator module implements the evaluator for Athir
/// [Currently a work in progress]
/// 

// mod builtins;
pub mod env; // Environment module - pub so lambda objects can use it
use std::rc::Rc;
use std::cell::RefCell;

use lazy_static::__Deref;

use crate::error::Error;
use crate::object::*;
use crate::read::{Identifier, Keyword};
use crate::result::EvalResult;
use crate::eval::env::Env;
use crate::gc::GC;

pub struct Eval {
    global_env: Rc<RefCell<Env>>,
    heap: GC,
}

impl Eval {
    pub fn new() -> Self {

        // start with an empty heap & environment
        let heap = GC::new();
        let env = Rc::new(RefCell::new(Env::new()));

        // initialize global environment with builtins

        // for (name, min_args, max_args, func) in builtins::builtins() {
        //     let object = Object::Procedure(Procedure::Builtin{name, min_args, max_args, func});
        //     Eval::::insert(&mut heap, env.clone(), name, object);
        // }

        Eval {
            heap: heap,
            global_env: env
        }

    }

    pub fn eval_global(&mut self, expr: Rc<RefCell<Object>>) -> EvalResult {
        // evaluate an expression in the global environment
        self.eval(expr, self.global_env.clone())
    }

    pub fn eval(&mut self, expr: Rc<RefCell<Object>>, env: Rc<RefCell<Env>>) -> EvalResult {        
        match expr.clone().borrow().deref() {
            Object::Boolean(_) 
            | Object::Bytevector(_)
            | Object::Character(_)
            | Object::Number(_)
            | Object::String(_)
            | Object::Vector(_) 
            | Object::Quotation(_) => Ok(expr),
            Object::Identifier(identifier) => self.eval_identifier(identifier, env),
            Object::Pair(car, cdr) => self.eval_pair(car.clone(), cdr.clone(), env),
            _ => Err(Error::EvalError("not implemented".to_string())),
        }
    }
    
    fn eval_pair(&mut self, car: Rc<RefCell<Object>>, cdr: Rc<RefCell<Object>>, env: Rc<RefCell<Env>>) -> EvalResult {
        match car.clone().borrow().deref() {
            Object::Identifier(Identifier::Keyword(keyword)) => match keyword {
                Keyword::Define => self.eval_define(cdr, env),
                Keyword::Set => self.eval_assignment(cdr, env),
                Keyword::Begin => self.eval_begin(cdr, env),
                Keyword::If => self.eval_if(cdr, env),
                Keyword::Lambda => self.eval_lambda(cdr, env),
                _ => Err(Error::EvalError("not implemented".to_string())),
            }
            _ =>self. eval_procedure_call(car.clone(), cdr.clone(), env), }
    }

    fn eval_if(&mut self, expr: Rc<RefCell<Object>>, env: Rc<RefCell<Env>>) -> EvalResult {
        let test = car(expr.clone())?;

        // check if test is true
        // if true, evaluate consequent
        // if false check if there is an alternative and evaluate it, otherwise return unspecified

        match self.eval(test, env.clone()) {
            Ok(e) if e.clone().borrow().deref().is_true() => {
                self.eval(cadr(expr)?, env)
                // Err(Error::EvalError("not implemented".to_string()))
            }
            _ => {
                match caddr(expr) {
                    Ok(expr) => self.eval(expr, env),
                    _ => Ok(self.heap.alloc(&Object::Unspecified))
                }
            }
        }
    }

    fn eval_lambda(&mut self, expr: Rc<RefCell<Object>>, env: Rc<RefCell<Env>>) -> EvalResult {
        let formals = car(expr.clone())?;
        let body = cadr(expr)?;

        Ok(self.heap.alloc(&Object::Procedure(Procedure::Lambda{env, formals, body})))
    }

    fn evlis(&mut self, operands: Rc<RefCell<Object>>, env: Rc<RefCell<Env>>) -> crate::result::VecEvalResult {
        self.heap.read(operands)
        .as_list()?
        .into_iter()
        .map(|operand| self.eval(operand, env.clone()))
        .collect::<crate::result::VecEvalResult>()
    }

    fn eval_procedure_call(&mut self, operator: Rc<RefCell<Object>>, operands: Rc<RefCell<Object>>, env: Rc<RefCell<Env>>) -> EvalResult {
        // println!("eval_procedure_call \n {:?} \n {:?}", operator, operands);
        let operator = self.eval(operator, env.clone())?;
        let operands = self.evlis(operands, env.clone())?;

        self.apply(operator, &operands)
    }
    
    fn apply(&mut self, operator: Rc<RefCell<Object>>, operands: &Vec<Rc<RefCell<Object>>>) -> EvalResult {
        match operator.clone().borrow().deref() {
            Object::Procedure(procedure) => match procedure {
                Procedure::Lambda{env, formals, body} => self.apply_lambda(formals.clone(), body.clone(), operands, env.clone()),
                Procedure::Builtin{name: _, min_args, max_args, func} => self.apply_builtin(min_args, max_args, func, operands),
            }
            _ => Err(Error::EvalError("not implemented".to_string())),
        }
    }

    fn apply_builtin(&mut self, min_args: &Option<usize>, max_args: &Option<usize>, func: &fn(&[Rc<RefCell<Object>>]) -> EvalResult, operands: &Vec<Rc<RefCell<Object>>>) -> EvalResult {

        if let Some(min_args) = min_args {
            if operands.len() < *min_args {
                return Err(Error::EvalError("not enough arguments".to_string()));
            }
        }

        if let Some(max_args) = max_args {
            if operands.len() > *max_args {
                return Err(Error::EvalError("too many arguments".to_string()));
            }
        }

        (func)(&operands[..])
    }

    fn apply_lambda(&mut self, formals: Rc<RefCell<Object>>, body: Rc<RefCell<Object>>, operands: &Vec<Rc<RefCell<Object>>>, env: Rc<RefCell<Env>>) -> EvalResult {

        // we check that formals and operands have the same length
        // otherwise we return an error
        let formals = formals.borrow().as_list()?;

        if formals.len() != operands.len() {
            return Err(Error::EvalError("arity does not match".to_string()));
        }

        // we create a new environment with the current environment as parent
        // we then bind the formals to the operands in the new environment
        let new_env = Rc::new(RefCell::new(Env::new_with_parent(env.clone())));

        for (formal, operand) in formals.into_iter().zip(operands) {
            let formal = match formal.borrow().clone() {
                Object::Identifier(Identifier::Variable(formal)) => formal,
                _ => return Err(Error::EvalError("not implemented".to_string())),
            };


            new_env.borrow_mut().insert(formal.clone(), operand.clone());
        }

        // we evaluate the body in the new environment
        // we evaluate all but the last expression in the body
        // we return the result of the last expression
        let body = self.heap.read(body);
        let body = body.as_list()?;
        let iter = body.iter();
       
        for expr in iter.take(body.len() - 1) {
            self.eval(expr.clone(), Rc::clone(&new_env))?;
        }

        self.eval(body.last().unwrap().clone(), new_env)
    }

    fn eval_define(&mut self, expr: Rc<RefCell<Object>>, env: Rc<RefCell<Env>>) -> EvalResult {
        let cadr = cadr(expr.clone())?;

        let x = match cadr.borrow().deref() {
            Object::Pair(_,_) => self.eval_define_function(expr, env),
            _ => self.eval_define_variable(expr, env),
        };

        x
    }

    fn eval_define_variable(&mut self, expr: Rc<RefCell<Object>>, env: Rc<RefCell<Env>>) -> EvalResult {
        let car = car(expr.clone())?;
        let car = car.borrow();
    
        let symbol = match car.deref() {
            Object::Identifier(Identifier::Variable(symbol)) => symbol,
            _ => return Err(Error::EvalError("unknown error".to_string())),
        };

        let value = self.eval(cadr(expr)?, env.clone())?;

        env.borrow_mut().insert(symbol.clone(), value);

        Ok(self.heap.alloc(&Object::Unspecified))
    }
    
    fn eval_define_function(&mut self, expr: Rc<RefCell<Object>>, env: Rc<RefCell<Env>>) -> EvalResult {
        let car = car(expr.clone())?;
        let car = car.borrow();
    
        let symbol = match car.deref() {
            Object::Identifier(Identifier::Variable(symbol)) => symbol,
            _ => return Err(Error::EvalError("unknown error".to_string())),
        };

        let formals = cadr(expr.clone())?;
        let body = cdr(expr)?;

        let lambda = self.eval_lambda(cons(formals, body)?, env.clone())?;

        env.borrow_mut().insert(symbol.clone(), lambda);

        Ok(self.heap.alloc(&Object::Unspecified))
    }

    fn eval_assignment(&mut self, expr: Rc<RefCell<Object>>, env: Rc<RefCell<Env>>) -> EvalResult {
        let car = car(expr.clone())?;
        let car = car.borrow();
    
        let symbol = match car.deref() {
            Object::Identifier(Identifier::Variable(symbol)) => Ok(symbol),
            _ => Err(Error::EvalError("unknown error".to_string())),
        }?;

        let ptr = env
                .borrow()
                .lookup(symbol.as_str())
                .and_then(|ptr| Some(ptr)).ok_or(Error::EvalError("symbol not found".to_string())
                )?;

        let value = self.eval(cadr(expr)?, env)?;

        self.heap.write(ptr, value.borrow().deref());

        Ok(self.heap.alloc(&Object::Null))
    }
    
    fn eval_begin(&mut self, _expr: Rc<RefCell<Object>>, _env: Rc<RefCell<Env>>) -> EvalResult {
        Ok(self.heap.alloc(&Object::Null))
    }
        
    fn eval_identifier(&mut self, id: &Identifier, env: Rc<RefCell<Env>>) -> EvalResult {
        if let Identifier::Variable(s) = id {
            if let Some(ptr) = env.borrow().lookup(s.as_str()) {
                return Ok(ptr);
            } else {
                return Err(Error::EvalError("unknown".to_string()));
            }
        } else {
            return Err(Error::EvalError("unexpected keyword".to_string()));
        }
    }

}
