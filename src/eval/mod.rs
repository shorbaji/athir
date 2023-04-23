/// eval.rs
///
/// The evaluator module implements the evaluator for Athir
/// [Currently a work in progress]
/// 
use std::ops::Deref;

use crate::read::{Identifier, Expr, Keyword};

pub fn eval(expr: &Box<Expr>) -> Result<Box<Expr>, &'static str> {
    match expr.deref() {
        Expr::Identifier(identifier) => eval_identifier(identifier),
        Expr::Boolean(_) 
        | Expr::Bytevector(_)
        | Expr::Character(_)
        | Expr::Number(_)
        | Expr::String(_)
        | Expr::Vector(_) => Ok(expr.clone()),
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
        _ => Ok(Box::new(Expr::Null)),
    }
}

fn eval_procedure_call(_operator: &Box<Expr>, _operands: &Box<Expr>) -> Result<Box<Expr>, &'static str> {
    // let children = node.children().unwrap();
    // let operator = eval(&children[0])?;
    // let operands = &children[1..].into_iter().map(eval);

    // println!("operator: {:?}", operator);
    // println!("operands: {:?}", operands);

    // apply(operator, operands)
    Err("eval error")
}

// fn apply(operator: Expr, operands: impl Iterator<Item = Result<Expr, Error>>) -> Result<Expr, Error> {
//     println!("apply");
//     Ok(Box::new(Expr::Null))
// }

fn eval_define(_node: &Box<Expr>) -> Result<Box<Expr>, &'static str> {
    Ok(Box::new(Expr::Null))
}

fn eval_assignment(_node: &Box<Expr>) -> Result<Box<Expr>, &'static str> {
    Ok(Box::new(Expr::Null))
}

fn eval_begin(_node: &Box<Expr>) -> Result<Box<Expr>, &'static str> {
    Ok(Box::new(Expr::Null))
}

fn eval_quotation(_node: &Box<Expr>) -> Result<Box<Expr>, &'static str> {
    Ok(Box::new(Expr::Null))
}

fn eval_identifier(id: &Identifier) -> Result<Box<Expr>, &'static str> {
    match id {
        Identifier::Variable(_) => Err("eval error"),
        Identifier::Keyword(_) => Err("eval error"),
    }
}

