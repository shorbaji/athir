/// eval.rs
///
/// The evaluator module implements the evaluator for Athir
/// [Currently a work in progress]
/// 

pub mod env; // Environment module - pub so lambda objects can use it

use std::rc::Rc;
use std::cell::RefCell;

use crate::error::Error;
use crate::object::{Object, Procedure, Builtin};
use crate::read::{Identifier, Keyword};
use crate::result::AthirResult;
use crate::eval::env::Env;
use crate::gc::GC;

use crate::builtins;

#[derive(Debug)]
pub struct Eval<T: GC> {
    heap: T,
    global_env: Rc<RefCell<Env>>,
}

impl<T> Eval<T> where T: GC {
    pub fn new() -> Eval<T> {
        let env = Env::new();
        let mut vm = Eval {
            heap: T::new(),
            global_env: Rc::new(RefCell::new(env))
        };

        vm.init();

        println!("env: {:?}", vm.global_env);
        vm
    }

    fn init(&mut self) {

        for builtin in builtins::builtins() {
            let name = builtin.name.to_string();
            let object = Object::Procedure(Procedure::Builtin(builtin));
            let ptr = self.heap.alloc(Box::new(object));

            self.global_env.borrow_mut().insert(name, ptr );
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
                    _ =>self. eval_procedure_call(car, cdr.car()?, env), }
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

    fn evlis(&mut self, operands: &Box<Object>, _env: Rc<RefCell<Env>>) -> crate::result::VecResult {
        operands
        .as_list()?
        .iter()
        .map(|operand| self.eval(operand, Rc::clone(&_env)))
        .collect::<crate::result::VecResult>()
    }

    fn eval_procedure_call(&mut self, operator: &Box<Object>, operands: &Box<Object>, env: Rc<RefCell<Env>>) -> AthirResult {
        let operator = self.eval(operator, Rc::clone(&env))?;
        let operands = self.evlis(operands, Rc::clone(&env))?;

        self.apply(&operator, &operands)
    }
    
    fn apply(&mut self, operator: &Box<Object>, operands: &Vec<Box<Object>>) -> AthirResult {
        match &**operator {
            Object::Procedure(procedure) => match procedure {
                Procedure::Lambda(env, formals, body) => self.apply_lambda(formals, body, operands, env.clone()),
                Procedure::Builtin(proc) => self.apply_builtin(proc, operands),
            }
            _ => Err(Error::EvalError("not implemented".to_string())),
        }
    }

    fn apply_builtin(&mut self, proc: &Builtin, operands: &Vec<Box<Object>>) -> AthirResult {
        if let Some(min_args) = proc.min_args {
            if operands.len() < min_args {
                return Err(Error::EvalError("not enough arguments".to_string()));
            }
        }

        if let Some(max_args) = proc.max_args {
            if operands.len() > max_args {
                return Err(Error::EvalError("too many arguments".to_string()));
            }
        }

        (proc.func)(&operands[..])
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
        let mut new_env = Env::new_with_parent(Rc::clone(&env));
        for (formal, operand) in formals.iter().zip(operands) {
            let formal = match &**formal {
                Object::Identifier(Identifier::Variable(formal)) => formal,
                _ => return Err(Error::EvalError("not implemented".to_string())),
            };

            let ptr = self.heap.alloc(operand.clone());
            new_env.insert(formal.clone(), ptr);
        }

        // we evaluate the body in the new environment
        // we evaluate all but the last expression in the body
        // we return the result of the last expression
        let body = body.as_list()?;
        let iter = body.iter();
       
        for expr in iter.take(body.len() - 1) {
            self.eval(expr, Rc::new(RefCell::new(new_env.clone())))?;
        }

        self.eval(body.last().unwrap(), Rc::new(RefCell::new(new_env)))
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
