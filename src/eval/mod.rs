/// eval.rs
///
/// The evaluator module implements the evaluator for Athir
/// [Currently a work in progress]
/// 

mod builtins;
pub mod env;
// Environment module - pub so lambda objects can use it
use std::rc::Rc;
use std::cell::RefCell;
use std::ops::Deref;

use crate::error::Error;
use crate::object::*;
use crate::read::{Identifier, Keyword};
use crate::result::EvalResult;
use crate::eval::env::Env;

pub struct Eval {
    global_env: Rc<RefCell<Env>>,
}

impl Eval {
    pub fn new() -> Self {

        // start with an empty heap & environment
        let env = Rc::new(RefCell::new(Env::new()));

        // initialize global environment with builtins

        for (name, min_args, max_args, func) in builtins::builtins() {
            let object = Rc::new(RefCell::new(Object::Procedure(Procedure::Builtin{name, min_args, max_args, func})));
            env.borrow_mut().insert(name.to_string(), object.clone());
        }

        Eval {
            global_env: env
        }

    }

    pub fn eval_global(&mut self, expr: Rc<RefCell<Object>>) -> EvalResult {
        // evaluate an expression in the global environment
        self.eval(expr, self.global_env.clone())
    }

    pub fn eval(&mut self, expr: Rc<RefCell<Object>>, env: Rc<RefCell<Env>>) -> EvalResult {       
        match expr.clone().deref().borrow().deref() {
            Object::Boolean(_) 
            | Object::Bytevector(_)
            | Object::Character(_)
            | Object::Number(_)
            | Object::String(_)
            | Object::Vector(_) => Ok(expr),
            | Object::Quotation(expr) => Ok(expr.clone()),
            Object::Identifier(identifier) => self.eval_identifier(identifier, env),
            Object::Pair(car, cdr) => self.eval_pair(car.clone(), cdr.clone(), env),
            _ => Err(Error::EvalError("not implemented".to_string())),
        }
    }
    
    fn eval_pair(&mut self, car: Rc<RefCell<Object>>, cdr: Rc<RefCell<Object>>, env: Rc<RefCell<Env>>) -> EvalResult {
        match car.deref().borrow().deref() {
            Object::Identifier(Identifier::Keyword(keyword)) => match keyword {
                Keyword::Define => self.eval_define(cdr, env),
                Keyword::Set => self.eval_assignment(cdr, env),
                Keyword::Begin => self.eval_begin(cdr, env),
                Keyword::If => self.eval_if(cdr, env),
                Keyword::Lambda => self.eval_lambda(cdr, env),
                _ => Err(Error::EvalError("not implemented".to_string())),
            }
            _ =>self. eval_procedure_call(car.clone(), (crate::object::car(cdr)?).clone(), env), }
    }

    fn eval_if(&mut self, expr: Rc<RefCell<Object>>, env: Rc<RefCell<Env>>) -> EvalResult {
        let test = car(expr.clone())?;

        // check if test is true
        // if true, evaluate consequent
        // if false check if there is an alternative and evaluate it, otherwise return unspecified

        let result = self.eval(test, env.clone())?;

        let bool = matches!(is_true(result)?.deref().borrow().deref(), Object::Boolean(true));

        if bool {
            self.eval(cadr(expr)?, env)
        } else {
            match caddr(expr) {
                Ok(expr) => self.eval(expr, env),
                _ => Ok(Rc::new(RefCell::new(Object::Unspecified)))
            }
        }
    }

    fn eval_lambda(&mut self, expr: Rc<RefCell<Object>>, env: Rc<RefCell<Env>>) -> EvalResult {
        let formals = car(expr.clone())?;
        let body = cadr(expr)?;

        Ok(Rc::new(RefCell::new(Object::Procedure(Procedure::Lambda{env, formals, body}))))
    }

    fn evlis(&mut self, operands: Rc<RefCell<Object>>, env: Rc<RefCell<Env>>) -> crate::result::EvalResult {

        let tmp = match operands.deref().borrow().deref() {
            Object::Null => Ok(Rc::new(RefCell::new(Object::Null))),
            _ => {
                Ok(
                    Rc::new(RefCell::new(Object::Pair(
                        self.eval(car(operands.clone())?, env.clone())?,
                        self.evlis(cdr(operands.clone())?, env.clone())?,
                    )))    
                )
            }
        };

        tmp
    }

    fn eval_procedure_call(&mut self, operator: Rc<RefCell<Object>>, operands: Rc<RefCell<Object>>, env: Rc<RefCell<Env>>) -> EvalResult {
        let operator = self.eval(operator, env.clone())?;
        let operands = self.evlis(operands, env.clone())?;

        self.apply(operator, operands)
    }
    
    fn apply(&mut self, operator: Rc<RefCell<Object>>, operands: Rc<RefCell<Object>>) -> EvalResult {
        match operator.clone().deref().borrow().deref() {
            Object::Procedure(procedure) => match procedure {
                Procedure::Lambda{env, formals, body} => self.apply_lambda(formals.clone(), body.clone(), operands, env.clone()),
                Procedure::Builtin{name: _, min_args, max_args, func} => self.apply_builtin(min_args, max_args, func, operands.clone()),
            }
            _ => Err(Error::EvalError("not implemented".to_string())),
        }
    }

    fn apply_builtin(&mut self, min_args: &Option<usize>, max_args: &Option<usize>, func: &fn(Rc<RefCell<Object>>) -> EvalResult, operands: Rc<RefCell<Object>>) -> EvalResult {

        let ls = operands.clone().deref().borrow().as_list()?;

        if let Some(min_args) = min_args {
            if ls.len() < *min_args {
                return Err(Error::EvalError("not enough arguments".to_string()));
            }
        }

        if let Some(max_args) = max_args {
            if ls.len() > *max_args {
                return Err(Error::EvalError("too many arguments".to_string()));
            }
        }

        (func)(operands)
    }

    fn apply_lambda(&mut self, formals: Rc<RefCell<Object>>, body: Rc<RefCell<Object>>, operands: Rc<RefCell<Object>>, env: Rc<RefCell<Env>>) -> EvalResult {

        // we check that formals and operands have the same length
        // otherwise we return an error
        let formals = formals.deref().borrow().as_list()?;
        let operands = operands.deref().borrow().as_list()?;

        if formals.len() != operands.len() {
            return Err(Error::EvalError("arity does not match".to_string()));
        }

        // we create a new environment with the current environment as parent
        // we then bind the formals to the operands in the new environment
        let new_env = Rc::new(RefCell::new(Env::new_with_parent(env.clone())));

        for (formal, operand) in formals.into_iter().zip(operands) {
            let formal = match formal.deref().borrow().clone() {
                Object::Identifier(Identifier::Variable(formal)) => formal,
                _ => return Err(Error::EvalError("not implemented".to_string())),
            };


            new_env.borrow_mut().insert(formal.clone(), operand.clone());
        }

        // we evaluate the body in the new environment
        // we evaluate all but the last expression in the body
        // we return the result of the last expression

        let body = body.deref().borrow().as_list()?;
        let iter = body.iter();
       
        for expr in iter.take(body.len() - 1) {
            self.eval(expr.clone(), Rc::clone(&new_env))?;
        }

        self.eval(body.last().unwrap().clone(), new_env)
    }

    fn eval_define(&mut self, expr: Rc<RefCell<Object>>, env: Rc<RefCell<Env>>) -> EvalResult {
        let cadr = cadr(expr.clone())?;

        let x = match cadr.deref().borrow().deref() {
            Object::Pair(_,_) => self.eval_define_function(expr, env),
            _ => self.eval_define_variable(expr, env),
        };

        x
    }

    fn eval_define_variable(&mut self, expr: Rc<RefCell<Object>>, env: Rc<RefCell<Env>>) -> EvalResult {
        let car = car(expr.clone())?;
        let car = car.deref().borrow();
    
        let symbol = match car.deref() {
            Object::Identifier(Identifier::Variable(symbol)) => symbol,
            _ => return Err(Error::EvalError("unknown error".to_string())),
        };

        let value = self.eval(cadr(expr)?, env.clone())?;

        env.borrow_mut().insert(symbol.clone(), value);

        Ok(Rc::new(RefCell::new(Object::Unspecified)))
    }
    
    fn eval_define_function(&mut self, expr: Rc<RefCell<Object>>, env: Rc<RefCell<Env>>) -> EvalResult {
        let car = car(expr.clone())?;
        let car = car.deref().borrow();
    
        let symbol = match car.deref() {
            Object::Identifier(Identifier::Variable(symbol)) => symbol,
            _ => return Err(Error::EvalError("unknown error".to_string())),
        };

        let formals = cadr(expr.clone())?;
        let body = cdr(expr)?;

        let lambda = self.eval_lambda(
            Rc::new(RefCell::new(Object::Pair(formals.clone(), body.clone()))),
            env.clone())?;

        env.borrow_mut().insert(symbol.clone(), lambda);

        Ok(Rc::new(RefCell::new(Object::Unspecified)))
    }

    fn eval_assignment(&mut self, expr: Rc<RefCell<Object>>, env: Rc<RefCell<Env>>) -> EvalResult {
        let car = car(expr.clone())?;
        let car = car.deref().borrow();
    
        let symbol = match car.deref() {
            Object::Identifier(Identifier::Variable(symbol)) => Ok(symbol),
            _ => Err(Error::EvalError("unknown error".to_string())),
        }?;

        let _ = env
                .clone()
                .deref()
                .borrow()
                .lookup(symbol.as_str())
                .and_then(|ptr| Some(ptr)).ok_or(Error::EvalError("symbol not found".to_string())
                )?;

        let value = self.eval(cadr(expr)?, env.clone())?;

        env.borrow_mut().insert(symbol.clone(), value);

        Ok(Rc::new(RefCell::new(Object::Null)))
    }
    
    fn eval_begin(&mut self, _expr: Rc<RefCell<Object>>, _env: Rc<RefCell<Env>>) -> EvalResult {
        Ok(Rc::new(RefCell::new(Object::Null)))
    }
        
    fn eval_identifier(&mut self, id: &Identifier, env: Rc<RefCell<Env>>) -> EvalResult {
        if let Identifier::Variable(s) = id {
            if let Some(ptr) = env.deref().borrow().lookup(s.as_str()) {
                return Ok(ptr);
            } else {
                return Err(Error::EvalError("unknown".to_string()));
            }
        } else {
            return Err(Error::EvalError("unexpected keyword".to_string()));
        }
    }

}
