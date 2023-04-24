/// eval.rs
///
/// The evaluator module implements the evaluator for Athir
/// [Currently a work in progress]
/// 

pub mod env;

use crate::read::{Identifier, Expr, Keyword};
use crate::eval::env::Env;
use crate::object::Object;
use crate::error::Error;

use std::ops::Deref;

pub struct Eval <T: Iterator<Item=Result<String, std::io::Error>>> {
    reader: crate::read::Reader<T>,
    heap: Vec<Object>,
    global: Env,
}

impl<T> Eval<T> where T: Iterator<Item=Result<String, std::io::Error>> {
    pub fn new(reader: crate::read::Reader<T>) -> Self {
        Eval {
            reader,
            heap: Vec::new(),
            global: Env::new(),
        }
    }
}

impl<T> Iterator for Eval<T> where T: Iterator <Item=Result<String, std::io::Error>> {
    type Item = Result<Box<Object>, crate::error::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.reader.next()
    }
}

pub fn eval(expr: &Box<Expr>, env: &mut Env) -> Result<Box<Expr>, crate::error::Error> {
    match expr.deref() {
        Expr::Identifier(identifier) => eval_identifier(identifier, env),
        Expr::Boolean(_) 
        | Expr::Bytevector(_)
        | Expr::Character(_)
        | Expr::Number(_)
        | Expr::String(_)
        | Expr::Vector(_) => Ok(expr.clone()),
        Expr::Pair(car, cdr) => {
            match &**car {
                Expr::Identifier(Identifier::Keyword(keyword)) => match keyword {
                    Keyword::Define => eval_define(cdr, env),
                    Keyword::Set => eval_assignment(cdr, env),
                    Keyword::Begin => eval_begin(cdr, env),
                    Keyword::Quote => eval_quotation(cdr, env),
                    _ => Err(Error::EvalError("not implemented".to_string())),
                }
                _ => eval_procedure_call(car, cdr, env), }
        }
        _ => Ok(Box::new(Expr::Null)),
    }
}

fn eval_procedure_call(_operator: &Box<Expr>, _operands: &Box<Expr>, env: &mut Env) -> Result<Box<Expr>, crate::error::Error> {
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

fn eval_define(_node: &Box<Expr>, env: &mut Env) -> Result<Box<Expr>, crate::error::Error> {
    Ok(Box::new(Expr::Null))
}

fn eval_assignment(_node: &Box<Expr>, env: &mut Env) -> Result<Box<Expr>, crate::error::Error> {
    Ok(Box::new(Expr::Null))
}

fn eval_begin(_node: &Box<Expr>, env: &mut Env) -> Result<Box<Expr>, crate::error::Error> {
    Ok(Box::new(Expr::Null))
}

fn eval_quotation(_node: &Box<Expr>, env: &mut Env) -> Result<Box<Expr>, crate::error::Error> {
    Ok(Box::new(Expr::Null))
}

fn eval_identifier(id: &Identifier, env: &mut Env) -> Result<Box<Expr>, crate::error::Error> {
    match id {
        Identifier::Variable(_) => Err(Error::EvalError("".to_string())),
        Identifier::Keyword(_) => Err(Error::EvalError("".to_string())),
    }
}

