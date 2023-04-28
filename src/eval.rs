
use crate::object::{Value, Object, Procedure, Keyword, ObjectExt, env::Env};
use crate::stdlib::base::*;

pub fn eval(expr: &Object, env: &Object) -> Result<Object, Object> {
    match *expr.borrow() {
        Value::Boolean(_)
        | Value::Bytevector(_) 
        | Value::Character(_) 
        | Value::Number(_) 
        | Value::String(_)
        | Value::Vector(_) => Ok(expr.clone()),
        | Value::Variable(ref var) => Object::lookup(var.clone(), env),
        | Value::Pair(ref car, ref cdr) => {
            match *car.borrow() {
                Value::Keyword(Keyword::If) => iff(&cdr, env),
                Value::Keyword(Keyword::Lambda) => lambda(&cdr, env),
                Value::Keyword(Keyword::Define) => define(&cdr, env),
                Value::Keyword(Keyword::Set) => assignment(&cdr, env),
                Value::Keyword(Keyword::Quote) => quote(&cdr),
                _ => {
                    let operator = eval(&car, env)?;
                    let operands = evlis(&cdr, env)?;
                    apply(&operator, &operands)
                }
            }
        },
        | Value::Quotation(ref quoted) => Ok(quoted.clone()),
        _ => Err(Object::new_error(format!("Malformed expression"))),
    }
}

fn apply(proc: &Object, args: &Object) -> Result<Object, Object> {
    match *proc.borrow() {
        Value::Procedure(ref func) => match func {
            Procedure::Nullary(f) => f(),
            Procedure::Unary(f) => f(&car(args)?),
            Procedure::Binary(f) => f(&car(args)?, &cadr(args)?),
            Procedure::Ternanry(f) => f(&car(args)?, &cadr(args)?, &caddr(args)?),
            Procedure::Variadic(f) => f(args),
            Procedure::Lambda(formals, body, parent) => apply_lambda(&formals, &body, &parent, args),
        }
        _ => Err(Object::new_error(format!("not a procedure"))),
    }
}

fn apply_lambda(formals: &Object, body: &Object, parent: &Object, args: &Object) -> Result<Object, Object> {

    if len(args)? == len(formals)? {
        let new_env = <Object as Env>::new_env_with_parent(parent);

        let mut args = args.clone();
        let mut formal:Object;
        let mut formals = formals.clone();

        while !formals.is_null() {
            formal = car(&formals)?;
            let arg = car(&args)?;

            new_env.insert(formal.as_variable_string()?, &arg)?;

            formals = cdr(&formals)?;

            args = cdr(&args)?;

        };

        let mut body = body.clone();
        let mut expr: Object;
        let mut result = Object::unspecified();

        while !body.is_null() {
            expr = car(&formals)?;
            result = eval(&expr, &new_env)?;
            body = cdr(&body)?;
        };

        Ok(result)
    } else {
        Err(Object::new_error(format!("Wrong number of arguments")))
    }

}

fn evlis(args: &Object, env: &Object) -> Result<Object, Object> {
    match *args.borrow() {
        Value::Null => Ok(Object::null()),
        Value::Pair(ref car, ref cdr) => cons(&eval(car, env)?, &evlis(cdr, env)?),
        _ => Err(Object::new_error("Malformed args".to_string())),
    }
}

fn iff(expr: &Object, env: &Object) -> Result<Object, Object> {
    let test = car(expr)?;
    let borrow = test.borrow();
    match *borrow {
        Value::Boolean(_) => eval(&cddr(expr)?, env),
        _ => eval(&cadr(expr)?, env),
    }
}

fn lambda(expr: &Object, env: &Object) -> Result<Object, Object> {
    let formals = car(expr)?;
    let body = cadr(expr)?;

    Ok(Object::new_procedure(Procedure::Lambda(formals, body, env.clone())))
}

fn define(expr: &Object, env: &Object) -> Result<Object, Object> {
    let var = car(expr)?;
    let val = eval(&cadr(expr)?, env)?;

    let borrow = var.borrow();

    match *borrow {
        Value::Variable(ref key) => {
            env.insert(key.clone(), &val)?;
            Ok(Object::unspecified())
        },
    _ => Err(Object::new_error(format!("not a variable"))),
    }
}

fn assignment(expr: &Object, env: &Object) -> Result<Object, Object> {
    Object::lookup(car(expr)?.as_variable_string()?, env)?;
    define(expr, env)
}

fn quote(expr: &Object) -> Result<Object, Object> {
    Ok(cadr(expr)?)
}

pub fn list(objects: Vec<Object>) -> Result<Object, Object> {
    let mut list = Object::null();
    for object in objects.into_iter().rev() {
        list = cons(&object, &list)?;
    }
    Ok(list)
}

pub fn list_not_null_terminated(objects: Vec<Object>, object: &Object) -> Result<Object, Object> {
    let mut list = object.clone();

    for object in objects.into_iter().rev() {
        list = cons(&object, &list)?;
    }

    Ok(list)
}

pub fn is_definition_expr(expr: &Object) -> bool {
    let is_definition_keyword = match car(expr) {
        Ok(node) => is_definition_keyword(&node),
        Err(_) => false,
    };
    
    is_definition_keyword || is_begin_definition_expr(expr)
}

fn is_definition_keyword(expr: &Object) -> bool {
    matches!(*expr.borrow(), 
        Value::Keyword(Keyword::Define)
        | Value::Keyword(Keyword::DefineValues)
        | Value::Keyword(Keyword::DefineRecordType)
        | Value::Keyword(Keyword::DefineSyntax))
}

fn is_begin_keyword(expr: &Object) -> bool {
    matches!(*expr.borrow(), Value::Keyword(Keyword::Begin))
}

fn is_begin_expr(expr: &Object) -> bool {
    match car(expr) {
        Ok(node) => is_begin_keyword(&node),
        Err(_) => false,
    }
}

fn is_begin_definition_expr(expr: &Object) -> bool {    
    is_begin_expr(expr) && 
    match cdadr(expr) {
        Ok(object) => matches!(*object.borrow(), Value::Boolean(true)),
        Err(_) => false,
    }
}
