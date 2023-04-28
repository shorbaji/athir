use std::rc::Rc;
use std::cell::RefCell;
use std::ops::{Deref, DerefMut};

use crate::object::{Value, Object, Procedure, Keyword, ObjectExt, env::Env};
use crate::stdlib::base::*;

pub fn eval(expr: Object, env: Object) -> Result<Object, Object> {
    match expr.deref().borrow().deref() {
        Value::Boolean(_)
        | Value::Bytevector(_) 
        | Value::Character(_) 
        | Value::Number(_) 
        | Value::String(_)
        | Value::Vector(_) => Ok(expr.clone()),
        | Value::Variable(var) => Object::lookup(var.clone(), env),
        | Value::Pair(car, cdr) => {
            match car.deref().borrow().deref() {
                Value::Keyword(Keyword::If) => iff(cdr.clone(), env),
                Value::Keyword(Keyword::Lambda) => lambda(cdr.clone(), env),
                Value::Keyword(Keyword::Define) => define(cdr.clone(), env),
                Value::Keyword(Keyword::Set) => assignment(cdr.clone(), env),
                Value::Keyword(Keyword::Quote) => quote(cdr.clone()),
                _ => apply(eval(car.clone(), env.clone())?, evlis(cdr.clone(), env.clone())?),
            }
        },
        | Value::Quotation(quoted) => Ok(quoted.clone()),
        _ => Err(Object::new_error(format!("Malformed expression"))),
    }
}

fn apply(proc: Object, args: Object) -> Result<Object, Object> {
    match proc.deref().borrow().deref() {
        Value::Procedure(func) => match func {
            Procedure::Nullary(f) => f(),
            Procedure::Unary(f) => f(car(args.clone())?),
            Procedure::Binary(f) => f(car(args.clone())?, cadr(args.clone())?),
            Procedure::Ternanry(f) => f(car(args.clone())?, cadr(args.clone())?, caddr(args)?),
            Procedure::Variadic(f) => f(args),
            Procedure::Lambda(formals, body, parent) => apply_lambda(formals.clone(), body.clone(), parent.clone(), args),
        }
        _ => Err(Object::new_error(format!("not a procedure"))),
    }
}

fn apply_lambda(formals: Object, body: Object, parent: Object, args: Object) -> Result<Object, Object> {

    if len(args.clone())? == len(formals.clone())? {
        let new_env = <Object as Env>::new_env_with_parent(parent.clone());

        let mut args = args;
        let mut formal : Object;
        let mut formals = formals;

        while !formals.is_null() {
            formal = car(formals.clone())?;
            let arg = car(args.clone())?;

            new_env.insert(formal.as_variable_string()?, arg)?;

            formals = cdr(formals)?;

            args = cdr(args)?;

        };

        let mut body = body;
        let mut expr: Object;
        let mut result = Object::unspecified();

        while !body.is_null() {
            expr = car(formals.clone())?;
            result = eval(expr.clone(), new_env.clone())?;
            body = cdr(body)?;
        };

        Ok(result)
    } else {
        Err(Object::new_error(format!("Wrong number of arguments")))
    }

}

fn evlis(args: Object, env: Object) -> Result<Object, Object> {
    match (*args).borrow().deref() {
        Value::Null => Ok(Rc::new(RefCell::new(Value::Null))),
        Value::Pair(ref car, cdr) => {
            let eval_car = eval(car.clone(), env.clone())?;
            let evlis_cdr = evlis(cdr.clone(), env)?;
            Ok(cons(eval_car, evlis_cdr)?)
        },
        _ => Err(Object::new_error("Malformed args".to_string())),
    }
}

fn iff(expr: Object, env: Object) -> Result<Object, Object> {
    let test = car(expr.clone())?;

    match *(eval(test, env.clone())?).borrow().deref() {
        Value::Boolean(false) => {
            let antecedent = cddr(expr)?;
            eval(antecedent, env)
        },
        _ => {
            let consequent = cadr(expr)?;
            eval(consequent, env)
        }
    }
}

fn lambda(expr: Object, env: Object) -> Result<Object, Object> {
    let formals = car(expr.clone())?;
    let body = cadr(expr)?;

    Ok(Object::new_procedure(Procedure::Lambda(formals, body, env)))
}

fn define(expr: Object, env: Object) -> Result<Object, Object> {
    let var = car(expr.clone())?;
    let var = var.deref().borrow();

    let val = cadr(expr)?;
    let val_eval = eval(val, env.clone())?;

    match var.deref() {
        Value::Variable(var) => {
            match env.deref().borrow().deref() {
                Value::Env(_, hm) => {
                    match hm.clone().borrow_mut().deref_mut() {
                        Value::Map(hm) => {
                            hm.insert(var.clone(), val_eval.clone());
                            Ok(Object::unspecified())
                        },
                        _ => Err(Object::new_error(format!("not a map"))),
                    }
                },
                _ => Err(Object::new_error(format!("not an env"))),
            }
        },
    _ => Err(Object::new_error(format!("not a variable"))),
    }
}

fn assignment(expr: Object, env: Object) -> Result<Object, Object> {
    Object::lookup(car(expr.clone())?.as_variable_string()?, env.clone())?;
    define(expr, env)
}

fn quote(expr: Object) -> Result<Object, Object> {
    Ok(cadr(expr)?)
}

pub fn list(objects: Vec<Object>) -> Result<Object, Object> {
    let mut list = Object::null();
    for object in objects.into_iter().rev() {
        list = cons(object, list)?;
    }
    Ok(list)
}

pub fn list_not_null_terminated(objects: Vec<Object>, object: Object) -> Result<Object, Object> {
    let mut list = object;

    for object in objects.into_iter().rev() {
        list = cons(object, list)?;
    }

    Ok(list)
}

pub fn is_definition_expr(expr: Object) -> bool {
    let is_definition_keyword = match car(expr.clone()) {
        Ok(node) => is_definition_keyword(node),
        Err(_) => false,
    };
    
    is_definition_keyword || is_begin_definition_expr(expr)
}

fn is_definition_keyword(expr: Object) -> bool {
    matches!(expr.deref().borrow().deref(), 
        Value::Keyword(Keyword::Define)
        | Value::Keyword(Keyword::DefineValues)
        | Value::Keyword(Keyword::DefineRecordType)
        | Value::Keyword(Keyword::DefineSyntax))
}

fn is_begin_keyword(expr: Object) -> bool {
    matches!(*expr.borrow(), Value::Keyword(Keyword::Begin))
}

fn is_begin_expr(expr: Object) -> bool {
    match car(expr) {
        Ok(node) => is_begin_keyword(node),
        Err(_) => false,
    }
}

fn is_begin_definition_expr(expr: Object) -> bool {    
    is_begin_expr(expr.clone()) && 
    match cdadr(expr) {
        Ok(object) => matches!(*object.borrow(), Value::Boolean(true)),
        Err(_) => false,
    }
}
