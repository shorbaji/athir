/// eval.rs
///
/// The evaluator module implements the evaluator for Athir
/// [Currently a work in progress]
/// 

mod builtins;
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

        // start with an empty heap
        let mut heap = T::new();

        // initialize global environment with builtins
        let env = Rc::new(RefCell::new(Env::new()));
        for (name, min_args, max_args, func) in builtins::builtins() {
            let object = Object::Procedure(Procedure::Builtin{name, min_args, max_args, func});
            Eval::<T>::insert(&mut heap, env.clone(), name, object);
        }

        Eval {
            heap: heap,
            global_env: env
        }

    }

    pub fn eval_global(&mut self, expr: &Box<Object>) -> AthirResult {
        // evaluate an expression in the global environment
        self.eval(expr, self.global_env.clone())
    }

    pub fn eval(&mut self, expr: &Box<Object>, env: Rc<RefCell<Env>>) -> AthirResult {
        match &**expr {
            Object::Boolean(_) 
            | Object::Bytevector(_)
            | Object::Character(_)
            | Object::Number(_)
            | Object::String(_)
            | Object::Vector(_) => Ok(expr.clone()),
            Object::Identifier(identifier) => self.eval_identifier(identifier, env),
            Object::Pair(car, cdr) => self.eval_pair(car, cdr, env),
            Object::Quotation(expr) => Ok(expr.clone()),
            _ => Ok(Box::new(Object::Null)),
        }
    }
    
    fn eval_pair(&mut self, car: &Box<Object>, cdr: &Box<Object>, env: Rc<RefCell<Env>>) -> AthirResult {
        match &**car {
            Object::Identifier(Identifier::Keyword(keyword)) => match keyword {
                Keyword::Define => self.eval_define(cdr, env),
                Keyword::Set => self.eval_assignment(cdr, env),
                Keyword::Begin => self.eval_begin(cdr, env),
                Keyword::If => self.eval_if(cdr, env),
                Keyword::Lambda => self.eval_lambda(cdr, env),
                _ => Err(Error::EvalError("not implemented".to_string())),
            }
            _ =>self. eval_procedure_call(car, cdr.car()?, env), }
    }

    fn eval_if(&mut self, expr: &Box<Object>, env: Rc<RefCell<Env>>) -> AthirResult {
        let test = expr.car()?;

        // check if test is true
        // if true, evaluate consequent
        // if false check if there is an alternative and evaluate it, otherwise return unspecified

        match self.eval(test, env.clone()) {
            Ok(e) if e.is_true() => {
                self.eval(expr.cadr()?, env)
                // Err(Error::EvalError("not implemented".to_string()))
            }
            _ => {
                match expr.caddr() {
                    Ok(expr) => self.eval(expr, env),
                    _ => Ok(Box::new(Object::Unspecified)),
                }
            }
        }
    }

    fn eval_lambda(&mut self, expr: &Box<Object>, env: Rc<RefCell<Env>>) -> AthirResult {
        let formals = expr.car()?.clone();
        let body = expr.cadr()?.clone();

        Ok(Box::new(Object::Procedure(Procedure::Lambda{env, formals, body})))
    }

    fn evlis(&mut self, operands: &Box<Object>, env: Rc<RefCell<Env>>) -> crate::result::VecResult {
        operands
        .as_list()?
        .iter()
        .map(|operand| self.eval(operand, env.clone()))
        .collect::<crate::result::VecResult>()
    }

    fn eval_procedure_call(&mut self, operator: &Box<Object>, operands: &Box<Object>, env: Rc<RefCell<Env>>) -> AthirResult {
        // println!("eval_procedure_call \n {:?} \n {:?}", operator, operands);
        let operator = self.eval(operator, env.clone())?;
        let operands = self.evlis(operands, env.clone())?;

        self.apply(&operator, &operands)
    }
    
    fn apply(&mut self, operator: &Box<Object>, operands: &Vec<Box<Object>>) -> AthirResult {
        match &**operator {
            Object::Procedure(procedure) => match procedure {
                Procedure::Lambda{env, formals, body} => self.apply_lambda(formals, body, operands, env.clone()),
                Procedure::Builtin{name: _, min_args, max_args, func} => self.apply_builtin(min_args, max_args, func, operands),
            }
            _ => Err(Error::EvalError("not implemented".to_string())),
        }
    }

    fn apply_builtin(&mut self, min_args: &Option<usize>, max_args: &Option<usize>, func: &fn(&[Box<Object>]) -> AthirResult, operands: &Vec<Box<Object>>) -> AthirResult {

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

    fn apply_lambda(&mut self, formals: &Box<Object>, body: &Box<Object>, operands: &Vec<Box<Object>>, env: Rc<RefCell<Env>>) -> AthirResult {

        // we check that formals and operands have the same length
        // otherwise we return an error
        let formals = formals.as_list()?;

        if formals.len() != operands.len() {
            return Err(Error::EvalError("arity does not match".to_string()));
        }

        // we create a new environment with the current environment as parent
        // we then bind the formals to the operands in the new environment
        let new_env = Rc::new(RefCell::new(Env::new_with_parent(env.clone())));

        for (formal, operand) in formals.iter().zip(operands) {
            let formal = match &**formal {
                Object::Identifier(Identifier::Variable(formal)) => formal,
                _ => return Err(Error::EvalError("not implemented".to_string())),
            };

            Eval::<T>::insert(
                &mut self.heap,
                Rc::clone(&new_env),
                &formal,
                *operand.clone(),
            );

        }

        // we evaluate the body in the new environment
        // we evaluate all but the last expression in the body
        // we return the result of the last expression
        let body = body.as_list()?;
        let iter = body.iter();
       
        for expr in iter.take(body.len() - 1) {
            self.eval(expr, Rc::clone(&new_env))?;
        }

        self.eval(body.last().unwrap(), new_env)
    }

    fn eval_define(&mut self, expr: &Box<Object>, env: Rc<RefCell<Env>>) -> AthirResult {
        // println!("evaluating define {:?}", expr.cadr()?);
        match &**expr.cadr()? {
            Object::Pair(_,_) => self.eval_define_function(expr, env),
            _ => self.eval_define_variable(expr, env),
        }
    }

    fn eval_define_variable(&mut self, expr: &Box<Object>, env: Rc<RefCell<Env>>) -> AthirResult {
        let symbol = match &**expr.car()? {
            Object::Identifier(Identifier::Variable(symbol)) => symbol,
            _ => return Err(Error::EvalError("unknown error".to_string())),
        };

        let value = self.eval(expr.cadr()?, env.clone())?;

        Eval::<T>::insert(&mut self.heap, env.clone(), &symbol, *value);

        Ok(Box::new(Object::Unspecified))
    }
    
    fn eval_define_function(&mut self, expr: &Box<Object>, env: Rc<RefCell<Env>>) -> AthirResult {
        let symbol = match &**expr.car()? {
            Object::Identifier(Identifier::Variable(symbol)) => symbol,
            _ => return Err(Error::EvalError("unknown error".to_string())),
        };

        let formals = expr.cadr()?;
        let body = expr.cddr()?;
        let lambda = self.eval_lambda(&formals.cons(*body.clone())?, env.clone())?;
        Eval::<T>::insert(&mut self.heap, env.clone(), &symbol, *lambda);

        Ok(Box::new(Object::Unspecified))
    }

    fn eval_assignment(&mut self, expr: &Box<Object>, env: Rc<RefCell<Env>>) -> AthirResult {
        let symbol = match &**expr.car()? {
            Object::Identifier(Identifier::Variable(symbol)) => Ok(symbol),
            _ => Err(Error::EvalError("unknown error".to_string())),
        }?;

        let ptr = env
                .borrow()
                .lookup(symbol)
                .and_then(|ptr| Some(ptr)).ok_or(Error::EvalError("symbol not found".to_string())
                )?;

        let value = self.eval(expr.cadr()?, env)?;

        self.heap.write(ptr, value);

        Ok(Box::new(Object::Null))
    }
    
    fn eval_begin(&mut self, _expr: &Box<Object>, _env: Rc<RefCell<Env>>) -> AthirResult {
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

    fn insert(heap: &mut T, env: Rc<RefCell<Env>>, name: &str, object: Object) {
        let ptr = heap.alloc(Box::new(object));
        env.borrow_mut().insert(name.to_string(), ptr);
    }

}
