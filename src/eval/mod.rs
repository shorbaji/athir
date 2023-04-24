/// eval.rs
///
/// The evaluator module implements the evaluator for Athir
/// [Currently a work in progress]
/// 

// pub mod env;

use crate::object::{Object, Expr, UNSPECIFIED, NULL};
use std::collections::HashMap;
use crate::read::{Identifier, Keyword};
use crate::error::Error;
use crate::AthirResult;
use std::ops::Deref;
type ObjectPtr = usize;

#[derive(Debug, Clone)]
pub struct Env {
    hashmap: HashMap<String, ObjectPtr>,
    parent: Option<Box<Env>>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            hashmap: HashMap::new(),
            parent: None,
        }
    }

    pub fn lookup(&self, key: &str) -> Option<&ObjectPtr> {
        match self.hashmap.get(key) {
            Some(value) => Some(value),
            None => match self.parent {
                Some(ref parent) => parent.lookup(key),
                None => None,
            },
        }
    }

    pub fn set(&mut self, key: String, value: ObjectPtr) {
        self.hashmap.insert(key, value);
    }

}

#[derive(Debug)]
pub struct Eval {
    heap: Vec<Object>,
}

impl Eval{
    pub fn new() -> Eval {
        Eval {
            heap: Vec::new(),
        }
    }

    fn alloc(&mut self, object: Box<Object>) -> ObjectPtr {
        self.heap.push(object.deref().clone());
        self.heap.len() - 1
    }

    pub fn eval(&mut self, expr: &Box<Expr>, env: &mut Env) -> AthirResult {
        match expr.deref() {
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
                        _ => Err(Error::EvalError("not implemented".to_string())),
                    }
                    _ =>self. eval_procedure_call(car, cdr, env), }
            }
            _ => Ok(Box::new(Expr::Null)),
        }
    }
    
    fn eval_if(&mut self, expr: &Box<Expr>, env: &mut Env) -> AthirResult {
        if self.eval(expr.car()?, env)?.is_true() {
            self.eval(expr.cadr()?, env)
        } else {
            expr.caddr()
            .and_then(|expr| self.eval(expr, env))
            .or_else(|_| Ok(UNSPECIFIED.clone()))
        }        
    }

    fn eval_procedure_call(&mut self, _operator: &Box<Expr>, _operands: &Box<Expr>, env: &mut Env) -> AthirResult {
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
    //     Ok(Box::new(Expr::Null))
    // }
    
    fn eval_define(&mut self, expr: &Box<Expr>, env: &mut Env) -> AthirResult {
        let symbol = match expr.car()?.deref() {
            Object::Pair(_, _) => Err(Error::EvalError("not implemented".to_string())),
            Object::Identifier(Identifier::Variable(symbol)) => Ok(symbol),
            _ => Err(Error::EvalError("unknown error".to_string())),
        }?;

        let value = self.eval(expr.cadr()?, env)?;

        let ptr = self.alloc(value);

        env.set(symbol.clone(), ptr);

        Ok(Box::new(Expr::Null))
    }
    
    fn eval_assignment(&mut self, expr: &Box<Expr>, env: &mut Env) -> AthirResult {
        Ok(Box::new(Expr::Null))
    }
    
    fn eval_begin(&mut self, expr: &Box<Expr>, env: &mut Env) -> AthirResult {
        Ok(Box::new(Expr::Null))
    }
    
    fn eval_quotation(&mut self, expr: &Box<Expr>, env: &mut Env) -> AthirResult {
        Ok(Box::new(Expr::Null))
    }
    
    fn eval_identifier(&mut self, id: &Identifier, env: &mut Env) -> AthirResult {
        if let Identifier::Variable(s) = id {
            if let Some(ptr) = env.lookup(s) {
                return Ok(Box::new(self.heap[*ptr].clone()));
            } else {
                return Err(Error::EvalError("unknown".to_string()));
            }
        } else {
            return Err(Error::EvalError("unexpected keyword".to_string()));
        }
    }
}
