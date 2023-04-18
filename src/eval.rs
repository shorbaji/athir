/// eval.rs
///
/// The evaluator module implements the evaluator for Athir
/// [Currently a work in progress]
/// 
use std::ops::Deref;

use crate::read::{Identifier, Literal, Expr};
use crate::error::{Error, ErrorKind::*};

#[derive(Debug)]
pub enum Object {
    Boolean(bool),
    Character(char),
    String(String),
    Number(String),
    Null,
}

pub fn eval(node: &Box<Expr>) -> Result<Object, Error> {
    println!("eval: {:?}", node);
    match node.deref() {
        Expr::Identifier(identifier) => eval_identifier(identifier),
        Expr::Literal(literal) => eval_literal(literal),
        _ => Ok(Object::Null),
    }
}

fn eval_literal(literal: &Literal) -> Result<Object, Error> {
    match literal.deref() {
        Literal::Boolean(b) => Ok(Object::Boolean(*b)),
        Literal::Character(c) => Ok(Object::Character(*c)),
        Literal::String(s) => Ok(Object::String(s.clone())),
        Literal::Number(n) => Ok(Object::Number(n.clone())),
        Literal::Vector(_) => Ok(Object::Null),
        Literal::Bytevector(_) => Ok(Object::Null),
        _ => Err(Error::new(EvalError)),
    }
}

fn eval_procedure_call(_node: &Box<Expr>) -> Result<Object, Error> {
    // let children = node.children().unwrap();
    // let operator = eval(&children[0])?;
    // let operands = &children[1..].into_iter().map(eval);

    // println!("operator: {:?}", operator);
    // println!("operands: {:?}", operands);

    // apply(operator, operands)
    Err(Error::new(EvalError))
}

// fn apply(operator: Object, operands: impl Iterator<Item = Result<Object, Error>>) -> Result<Object, Error> {
//     println!("apply");
//     Ok(Object::Null)
// }

fn eval_definition(_node: &Box<Expr>) -> Result<Object, Error> {
    Ok(Object::Null)
}

fn eval_assignment(_node: &Box<Expr>) -> Result<Object, Error> {
    Ok(Object::Null)
}

fn eval_begin(_node: &Box<Expr>) -> Result<Object, Error> {
    Ok(Object::Null)
}

fn eval_begin_def(_node: &Box<Expr>) -> Result<Object, Error> {
    Ok(Object::Null)
}

fn eval_quotation(_node: &Box<Expr>) -> Result<Object, Error> {
    Ok(Object::Null)
}

fn eval_identifier(id: &Identifier) -> Result<Object, Error> {
    match id {
        Identifier::Variable(_) => Err(Error::new(EvalError)),
        Identifier::Keyword(_) => Err(Error::new(EvalError)),
    }
}

