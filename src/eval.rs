/// eval.rs
///
/// The evaluator module implements the evaluator for Athir
/// [Currently a work in progress]
/// 
use std::ops::Deref;

use crate::read::{Identifier, Literal, Keyword, Expr};

#[derive(Debug)]
pub enum Object {
    Boolean(bool),
    Character(char),
    String(String),
    Number(String),
    Null,
}

pub fn eval(expr: &Box<Expr>) -> Result<Object, &'static str> {
    match expr.deref() {
        Expr::Identifier(identifier) => eval_identifier(identifier),
        Expr::Literal(literal) => eval_literal(literal),
        Expr::Pair(car, cdr) => {
            match &**car {
                Expr::Identifier(Identifier::Keyword(keyword)) => match keyword {
                    Keyword::Define => eval_define(cdr),
                    Keyword::Set => eval_assignment(cdr),
                    Keyword::Begin => eval_begin(cdr),
                    Keyword::Quote => eval_quotation(cdr),
                    _ => Err("eval error: not implemented"),
                }
                _ => eval_procedure_call(car, cdr), }
        }
        _ => Ok(Object::Null),
    }
}

fn eval_literal(literal: &Literal) -> Result<Object, &'static str> {
    match literal.deref() {
        Literal::Boolean(b) => Ok(Object::Boolean(*b)),
        Literal::Character(c) => Ok(Object::Character(*c)),
        Literal::String(s) => Ok(Object::String(s.clone())),
        Literal::Number(n) => Ok(Object::Number(n.clone())),
        Literal::Vector(_) => Ok(Object::Null),
        Literal::Bytevector(_) => Ok(Object::Null),
        _ => Err("eval error"),
    }
}

fn eval_procedure_call(_operator: &Box<Expr>, _operands: &Box<Expr>) -> Result<Object, &'static str> {
    // let children = node.children().unwrap();
    // let operator = eval(&children[0])?;
    // let operands = &children[1..].into_iter().map(eval);

    // println!("operator: {:?}", operator);
    // println!("operands: {:?}", operands);

    // apply(operator, operands)
    Err("eval error")
}

// fn apply(operator: Object, operands: impl Iterator<Item = Result<Object, Error>>) -> Result<Object, Error> {
//     println!("apply");
//     Ok(Object::Null)
// }

fn eval_define(_node: &Box<Expr>) -> Result<Object, &'static str> {
    Ok(Object::Null)
}

fn eval_assignment(_node: &Box<Expr>) -> Result<Object, &'static str> {
    Ok(Object::Null)
}

fn eval_begin(_node: &Box<Expr>) -> Result<Object, &'static str> {
    Ok(Object::Null)
}

fn eval_quotation(_node: &Box<Expr>) -> Result<Object, &'static str> {
    Ok(Object::Null)
}

fn eval_identifier(id: &Identifier) -> Result<Object, &'static str> {
    match id {
        Identifier::Variable(_) => Err("eval error"),
        Identifier::Keyword(_) => Err("eval error"),
    }
}

