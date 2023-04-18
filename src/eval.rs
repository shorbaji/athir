/// eval.rs
///
/// The evaluator module implements the evaluator for Athir
/// [Currently a work in progress]
/// 
use std::ops::Deref;

use crate::read::{Node, NodeKind::*};
use crate::read::Token;
use crate::error::{Error, ErrorKind::*};

#[derive(Debug)]
pub enum Object {
    Boolean(bool),
    Character(char),
    String(String),
    Number(String),
    Null,
}

pub fn eval(node: &Box<Node>) -> Result<Object, Error> {
    match node.deref().kind() {
        Assignment => eval_assignment(node),
        Begin(false) => eval_begin(node),
        Begin(true) => eval_begin_def(node),
        Identifier => eval_identifier(node),
        Literal => eval_literal(node),
        ProcedureCall => eval_procedure_call(node),
        Quotation => eval_quotation(node),
        Define | DefineSyntax => eval_definition(node),
        _ => Ok(Object::Null),
    }
}

fn eval_literal(node: &Box<Node>) -> Result<Object, Error> {
    match node.deref() {
        Node::Leaf(Literal, token) => 
            match token {
                Token::Boolean(b) => Ok(Object::Boolean(*b)),
                Token::Character(c) => Ok(Object::Character(*c)),
                Token::String(s) => Ok(Object::String(s.clone())),
                Token::Number(n) => Ok(Object::Number(n.clone())),
                _ => Err(Error::new(EvalError)),
            },
        _ => Err(Error::new(EvalError)),
    }
}

fn eval_procedure_call(node: &Box<Node>) -> Result<Object, Error> {
    let children = node.children().unwrap();
    let operator = eval(&children[0])?;
    let operands = &children[1..].into_iter().map(eval);

    println!("operator: {:?}", operator);
    println!("operands: {:?}", operands);

    // apply(operator, operands)
    Err(Error::new(EvalError))
}

// fn apply(operator: Object, operands: impl Iterator<Item = Result<Object, Error>>) -> Result<Object, Error> {
//     println!("apply");
//     Ok(Object::Null)
// }

fn eval_definition(_node: &Box<Node>) -> Result<Object, Error> {
    Ok(Object::Null)
}

fn eval_assignment(_node: &Box<Node>) -> Result<Object, Error> {
    Ok(Object::Null)
}

fn eval_begin(_node: &Box<Node>) -> Result<Object, Error> {
    Ok(Object::Null)
}

fn eval_begin_def(_node: &Box<Node>) -> Result<Object, Error> {
    Ok(Object::Null)
}

fn eval_quotation(_node: &Box<Node>) -> Result<Object, Error> {
    Ok(Object::Null)
}

fn eval_identifier(_node: &Box<Node>) -> Result<Object, Error> {
    Ok(Object::Null)
}

