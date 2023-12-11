//! Athir parser/reader module.

//! Deviations from R7RS:
//! - string inline hex escapes not implemented
//! - no support for # label in datum.
//! - derived expressions not implemented
//!

pub mod lexer;
#[cfg(test)]
mod tests;

use std::iter::{from_fn, once};
use std::ops::Deref;

use crate::alloc::{A, R};
use crate::stdlib::{
    base::{car, cons},
    cxr::cdadr,
};

use crate::read::lexer::Token;
use crate::value::{Error, Keyword, V};

/// Reader trait
///
/// Implements the reader/parser for the Athir interpreter
/// Requires a get_next_token() function that returns the next token from the input stream
/// Requires a peek_next_token() function that returns the next token from the input stream without consuming it
/// Provides a read() function that reads an expression from the input stream
/// Read function is implemented as a recursive descent parser
pub trait Reader {
    // required functions
    fn get_next_token(&mut self) -> Option<Token>;
    fn peek_next_token(&mut self) -> Option<Token>;

    // provided function

    /// read()
    /// the starting point for the recursive descent parser is the expr() function
    ///
    /// The parser implements an error recovery strategy that 1) keeps track of the depth of the parsing,
    /// i.e. the number of left parentheses encountered minus the number of right parentheses encountered
    /// and 2) skips to the next right parenthesis when an error is encountered in a compound expression
    /// To implement this strategy, the parser maintains the depth value. The depth value is incremented
    /// when a left parenthesis is encountered and decremented when a right parenthesis is encountered.
    /// We start with a depth of zero which is passed on to the entry point of the parser, the expr() function.
    ///
    /// Recovery is implemented by the recover() function

    fn read(&mut self) -> R {
        match self.expr(0) {
            Ok(e) => e,
            Err(e) => e,
        }
    }

    /// the error recovery function
    fn recover(&mut self, error: &R) {
        if let V::Error(Error::Syntax { ref depth, .. }) = error.deref().borrow().deref() {
            if *depth == 0 {
                // unexpected token at top level
                // skip to next line
                self.get_next_token();
            } else {
                // unexpected token in compound expression
                // skip to next right parenthesis

                let mut count = *depth;
                loop {
                    match self.get_next_token() {
                        Some(Token::ParenRight) => {
                            count -= 1;
                            if count == 0 {
                                break;
                            }
                        }
                        Some(Token::ParenLeft) => {
                            count += 1;
                        }
                        Some(_) => {}
                        None => break,
                    }
                }
            }
        }
    }

    //
    // Expressions
    //
    // This function implements the following rules from R7RS 7.1.3
    //
    // <expression> ::= <identifier>
    // | <literal>
    // | <procedure call>
    // | <lambda expression>
    // | <conditional>
    // | <assignment>
    // | <derived expression>
    // | <macro use>
    // | <macro block>
    // | <includer>
    //
    // <literal> ::= <quotation> | <self-evaluating>
    //
    // <self-evaluating> ::= <boolean> | <number> | <vector>
    // | <character> | <string> | <bytevector>
    //
    // <quotation> ::= ’<datum> | ( quote <datum> )
    //
    //
    // Known issues:
    // - derived expression is not implemented
    // - does not check bytevector elements are bytes (i.e. 0-255)
    // - no error recovery

    fn expr(&mut self, rdepth: usize) -> Result<R, R> {
        // a left parenthesis indicates a compound expression
        // checks for an atom

        match self.peek_or_eof()? {
            Token::ParenLeft => self.compound(rdepth),
            Token::SharpU8Open => self.bytevector(rdepth),
            Token::Identifier(_) => self.identifier(rdepth),
            Token::Boolean(_) => self.literal(rdepth),
            Token::Character(_) => self.literal(rdepth),
            Token::Number(_) => self.literal(rdepth),
            Token::String(_) => self.literal(rdepth),
            Token::Quote => self.quotation_short(rdepth),
            Token::Quasiquote => self.quasiquotation_short(rdepth, 1),
            Token::SharpOpen => self.vector(rdepth),
            _ => Err(A::syntax_error(rdepth, "unexpected token")),
        }
    }

    fn compound(&mut self, rdepth: usize) -> Result<R, R> {
        // <compound expression> ::= <special form> | <procedure call>
        // we start with a left parenthesis
        // we then call special_form_handler to select a handler based the keyword
        // if no keyword is found we default to a procedure call
        // we call the handler and look for a right parenthesis before we return the result

        self.paren_left(rdepth)?;

        let expr = match self.peek_or_eof()? {
            Token::Identifier(id) => match id.as_str() {
                "begin" => self.begin(rdepth + 1),
                "define" => self.define(rdepth + 1),
                "define-values" => self.define_values(rdepth + 1),
                "define-record-type" => self.define_record_type(rdepth + 1),
                "define-syntax" => self.define_syntax(rdepth + 1),
                "define-library" => self.define_library(rdepth + 1),
                "if" => self.iff(rdepth + 1),
                "include" => self.include(rdepth + 1),
                "include-ci" => self.include(rdepth + 1),
                "lambda" => self.lambda(rdepth + 1),
                "let-syntax" | "letrec-syntax" => self.macro_block(rdepth + 1),
                "quasiquote" => self.quasiquotation(rdepth + 1, 1),
                "quote" => self.quotation(rdepth + 1),
                "set!" => self.assignment(rdepth + 1),
                _ => self.procedure_call(rdepth + 1),
            },
            _ => self.procedure_call(rdepth + 1),
        }?;

        self.paren_right(rdepth)?;
        Ok(expr)
    }

    ///
    /// Procedure calls
    ///
    ///
    ///

    fn procedure_call(&mut self, rdepth: usize) -> Result<R, R> {
        let operator = self.expr(rdepth)?;
        let operands = self.operands(rdepth)?;

        Ok(cons(&operator, &operands))
        // operator.cons(&operands)
    }

    fn operands(&mut self, rdepth: usize) -> Result<R, R> {
        let operands = self.zero_or_more(Reader::expr, rdepth)?;
        Self::list(operands)
    }

    ///
    /// Lambda
    ///
    ///
    ///

    fn lambda(&mut self, rdepth: usize) -> Result<R, R> {
        let keyword = self.keyword("lambda", rdepth)?;
        let formals = self.formals(rdepth)?;

        let body = self.body(rdepth)?;
        Ok(cons(&keyword, &cons(&formals, &body)))
    }

    fn formals(&mut self, rdepth: usize) -> Result<R, R> {
        match self.peek_or_eof()? {
            Token::Identifier(_) => self.identifier(rdepth),
            Token::ParenLeft => {
                self.paren_left(rdepth)?;
                let ids = self.identifier_list_possible_dot(rdepth + 1)?;
                self.paren_right(rdepth + 1)?;
                Ok(ids)
            }
            _ => Err(A::syntax_error(
                rdepth,
                "expected identifier or open parenthesis",
            )),
            // token @ _ => Err(unexpected(rdepth, token.to_string(), "identifier or open parenthesis".to_string())),
        }
    }

    /// body should not be empty
    /// body can have defintions and expressions
    /// body cannot have definitions after any expression

    fn body(&mut self, rdepth: usize) -> Result<R, R> {
        let exprs = self.one_or_more(Reader::expr, rdepth)?;
        let mut defs = true;

        for expr in exprs.clone().into_iter() {
            if Self::is_definition_expr(&expr) {
                if !defs {
                    return Err(A::syntax_error(
                        rdepth,
                        "definitions must precede expressions",
                    ));
                }
            } else {
                defs = false;
            }
        }
        Self::list(exprs)
    }

    //
    // Includer
    //
    //
    //

    fn include(&mut self, rdepth: usize) -> Result<R, R> {
        let keyword = self.keyword_box_leaf_from_next(rdepth)?;
        let paths = self.strings(rdepth)?;
        Self::list(vec![keyword, paths])
    }

    ///
    /// Begin
    ///
    ///
    ///

    fn begin(&mut self, rdepth: usize) -> Result<R, R> {
        // we look for the keyword begin
        // then we look for a list of expressions

        let begin = self.keyword("begin", rdepth)?;

        // we look for a list of expressions after begin
        // we also create a boolean tag  is_all_defs to indicate if all expressions are definitions
        // we return a list (exprs is_all_defs)
        // this is used by the evaluator to determine if the begin expression is itself a definition

        let exprs = self.zero_or_more(Reader::expr, rdepth)?;

        let is_all_defs = exprs.iter().all(|node| Self::is_definition_expr(node));
        let is_all_defs = A::boolean(is_all_defs);

        let exprs = Self::list(exprs)?;

        let tagged = cons(&exprs, &is_all_defs);

        Self::list(vec![begin, tagged])
    }

    fn list(objects: Vec<R>) -> Result<R, R> {
        let mut list = A::null();
        for object in objects.into_iter().rev() {
            list = cons(&object, &list);
        }
        Ok(list)
    }

    fn list_not_null_terminated(objects: Vec<R>, object: &R) -> Result<R, R> {
        let mut list = object.clone();

        for object in objects.into_iter().rev() {
            list = cons(&object, &list);
        }

        Ok(list)
    }

    fn is_definition_expr(expr: &R) -> bool {
        let node = car(expr);

        let is_definition_keyword = match node.deref().borrow().deref() {
            V::Error(_) => false,
            _ => Self::is_definition_keyword(&node),
        };

        is_definition_keyword || Self::is_begin_definition_expr(expr)
    }

    fn is_definition_keyword(expr: &R) -> bool {
        matches!(
            expr.deref().borrow().deref(),
            V::Keyword(Keyword::Define)
                | V::Keyword(Keyword::DefineValues)
                | V::Keyword(Keyword::DefineRecordType)
                | V::Keyword(Keyword::DefineSyntax)
        )
    }

    fn is_begin_keyword(expr: &R) -> bool {
        matches!(expr.deref().borrow().deref(), V::Keyword(Keyword::Begin))
    }

    fn is_begin_expr(expr: &R) -> bool {
        let node = car(expr);

        let borrow = node.deref().borrow();
        match borrow.deref() {
            V::Error(_) => false,
            _ => Self::is_begin_keyword(&node),
        }
    }

    fn is_begin_definition_expr(expr: &R) -> bool {
        let object = cdadr(expr);

        Self::is_begin_expr(expr)
            && match object.deref().borrow().deref() {
                V::Error(_) => false,
                _ => matches!(object.deref().borrow().deref(), V::Boolean(true)),
            }
    }

    ///
    /// Definitions
    ///
    /// We start with the define keyword and then look for either
    /// - an identifier (i.e. a variable definition) or,
    /// - a parenthesized list of identifiers (i.e. a function definition)
    ///

    fn define(&mut self, rdepth: usize) -> Result<R, R> {
        let keyword = self.keyword("define", rdepth)?;
        match self.peek_or_eof()? {
            Token::Identifier(_) => {
                // variable definition
                let var = self.identifier(rdepth)?;
                let expr = self.expr(rdepth)?;
                Self::list(vec![keyword, var, expr])
            }
            Token::ParenLeft => {
                // function definition
                self.paren_left(rdepth)?;
                let var = self.identifier(rdepth + 1)?;
                let formals = self.def_formals(rdepth + 1)?;
                self.paren_right(rdepth + 1)?;
                let body = self.body(rdepth)?;

                Self::list(vec![keyword, var, formals, body])
            }
            _ => Err(A::syntax_error(
                rdepth,
                "expected identifier or open parenthesis",
            )),
            // token @ _ => Err(unexpected(1, token.to_string(), "identifier or open paren".to_string())),
        }
    }

    fn def_formals(&mut self, rdepth: usize) -> Result<R, R> {
        self.identifier_list_possible_dot(rdepth)
    }

    fn define_values(&mut self, rdepth: usize) -> Result<R, R> {
        let keyword = self.keyword("define-values", rdepth)?;
        let formals = self.formals(rdepth)?;
        let exprs = self.body(rdepth)?;

        Self::list(vec![keyword, formals, exprs])
    }

    fn define_record_type(&mut self, rdepth: usize) -> Result<R, R> {
        let keyword = self.keyword("define-record-type", rdepth)?;
        let id1 = self.identifier(rdepth)?;
        let constructor = self.constructor(rdepth)?;
        let id2 = self.identifier(rdepth)?;
        let field_specs = self.field_specs(rdepth)?;

        Self::list(vec![keyword, id1, constructor, id2, field_specs])
    }

    fn constructor(&mut self, rdepth: usize) -> Result<R, R> {
        self.paren_left(rdepth)?;
        let id = self.identifier(rdepth + 1)?;
        let field_names = self.field_names(rdepth + 1)?;
        self.paren_right(rdepth + 1)?;

        Self::list(vec![id, field_names])
    }

    fn field_names(&mut self, rdepth: usize) -> Result<R, R> {
        let names = self.zero_or_more(Reader::identifier, rdepth)?;
        Self::list(names)
    }

    fn field_specs(&mut self, rdepth: usize) -> Result<R, R> {
        let specs = self.zero_or_more(Reader::field_spec, rdepth)?;
        Self::list(specs)
    }

    fn field_spec(&mut self, rdepth: usize) -> Result<R, R> {
        self.paren_left(rdepth)?;

        let field_name = self.identifier(rdepth + 1)?;
        let accessor = self.identifier(rdepth + 1)?;

        let mut vec = vec![field_name, accessor];
        if let Token::Identifier(_) = self.peek_or_eof()? {
            vec.push(self.identifier(rdepth + 1)?);
        };

        self.paren_right(rdepth + 1)?;

        Self::list(vec)
    }

    fn define_syntax(&mut self, rdepth: usize) -> Result<R, R> {
        let keyword = self.keyword("define-syntax", rdepth)?;
        let id = self.identifier(rdepth)?;
        let expr = self.transformer_spec(rdepth)?;

        Self::list(vec![keyword, id, expr])
    }

    ///
    /// define-library
    ///
    ///
    ///

    fn define_library(&mut self, rdepth: usize) -> Result<R, R> {
        let keyword = self.keyword("define-library", rdepth)?;
        let name = self.library_name(rdepth)?;
        let declarations = self.library_declarations(rdepth)?;
        Self::list(vec![keyword, name, declarations])
    }

    fn library_declarations(&mut self, rdepth: usize) -> Result<R, R> {
        let declarations = self.zero_or_more(Reader::library_declaration, rdepth)?;
        Self::list(declarations)
    }

    fn library_name(&mut self, rdepth: usize) -> Result<R, R> {
        self.paren_left(rdepth)?;
        let name = self.library_name_after_open(rdepth + 1)?;
        self.paren_right(rdepth + 1)?;
        Ok(name)
    }

    fn library_name_after_open(&mut self, rdepth: usize) -> Result<R, R> {
        Self::list(self.one_or_more(Reader::library_name_part, rdepth)?)
    }

    fn library_name_part(&mut self, rdepth: usize) -> Result<R, R> {
        match self.peek_or_eof()? {
            Token::Identifier(_) => self.identifier(rdepth),
            Token::Number(_) => self.uinteger10(rdepth),
            _ => Err(A::syntax_error(rdepth, "expected identifier or uinteger10")),
            // token @ _ => Err(unexpected(rdepth, token.to_string(), "identifier or uinteger10".to_string())),
        }
    }

    fn library_declaration(&mut self, rdepth: usize) -> Result<R, R> {
        self.paren_left(rdepth)?;
        let declaration = match self.peek_or_eof()? {
            Token::Identifier(id) =>
                match id.as_str() {
                    "begin" => self.begin(rdepth + 1),
                    "export" => self.export(rdepth + 1),
                    "import" => self.import(rdepth + 1),
                    "include" => self.include(rdepth + 1),
                    "include-ci" => self.include(rdepth + 1),
                    "include-library-declarations" => self.include_library_declarations(rdepth + 1),
                    "cond-expand" => self.cond_expand(rdepth + 1),
                    _ => Err(A::syntax_error(
                            rdepth, "expected library declaration")),
                },
                _ => Err(A::syntax_error(rdepth, "expected library declaration")),
        }?;
        self.paren_right(rdepth + 1)?;
        Ok(declaration)
    }

    fn export(&mut self, rdepth: usize) -> Result<R, R> {
        let keyword = self.keyword("export", rdepth)?;
        let specs = self.export_specs(rdepth)?;

        Self::list(vec![keyword, specs])
    }

    fn export_specs(&mut self, rdepth: usize) -> Result<R, R> {
        let specs = self.zero_or_more(Reader::export_spec, rdepth)?;
        Self::list(specs)
    }

    fn export_spec(&mut self, rdepth: usize) -> Result<R, R> {
        match self.peek_or_eof()? {
            Token::Identifier(_) => self.export_spec_id(rdepth),
            Token::ParenLeft => self.export_spec_rename(rdepth),
            _ => Err(A::syntax_error(
                rdepth,
                "expected identifier or export-spec-list",
            )),
            // token @ _ => Err(unexpected(rdepth, token.to_string(), "identifier or export-spec-list".to_string())),
        }
    }

    fn export_spec_id(&mut self, rdepth: usize) -> Result<R, R> {
        self.identifier(rdepth)
    }

    fn export_spec_rename(&mut self, rdepth: usize) -> Result<R, R> {
        self.paren_left(rdepth)?;
        self.keyword("rename", rdepth + 1)?;

        let id1 = self.identifier(rdepth + 1)?;
        let id2 = self.identifier(rdepth + 1)?;
        self.paren_right(rdepth + 1)?;

        Self::list(vec![id1, id2])
    }

    fn import(&mut self, rdepth: usize) -> Result<R, R> {
        let keyword = self.keyword("import", rdepth)?;
        let sets = self.import_sets(rdepth)?;

        Self::list(vec![keyword, sets])
    }

    fn import_sets(&mut self, rdepth: usize) -> Result<R, R> {
        let sets = self.zero_or_more(Reader::import_set, rdepth)?;
        Self::list(sets)
    }

    fn import_set(&mut self, rdepth: usize) -> Result<R, R> {
        self.paren_left(rdepth)?;
        let import_set = match self.peek_or_eof()? {
            Token::Identifier(id) => match id.as_str() {
                "only" => self.only(rdepth + 1)?,
                "except" => self.except(rdepth + 1)?,
                "prefix" => self.prefix(rdepth + 1)?,
                "rename" => self.rename(rdepth + 1)?,
                _ => self.library_name_after_open(rdepth + 1)?,
            },
            _ => self.library_name_after_open(rdepth + 1)?,
        };

        self.paren_right(rdepth + 1)?;

        Ok(import_set)
    }

    fn only(&mut self, rdepth: usize) -> Result<R, R> {
        let keyword = self.keyword("only", rdepth)?;
        let set = self.import_set(rdepth)?;
        let ids = self.import_set_ids(rdepth)?;

        Self::list(vec![keyword, set, ids])
    }

    fn import_set_ids(&mut self, rdepth: usize) -> Result<R, R> {
        Self::list(self.one_or_more(Reader::identifier, rdepth)?)
    }

    fn except(&mut self, rdepth: usize) -> Result<R, R> {
        let keyword = self.keyword("except", rdepth)?;
        let set = self.import_set(rdepth)?;
        let ids = self.import_set_ids(rdepth)?;

        Self::list(vec![keyword, set, ids])
    }

    fn prefix(&mut self, rdepth: usize) -> Result<R, R> {
        let keyword = self.keyword("prefix", rdepth)?;
        let set = self.import_set(rdepth)?;
        let id = self.identifier(rdepth)?;

        Self::list(vec![keyword, set, id])
    }

    fn rename(&mut self, rdepth: usize) -> Result<R, R> {
        let keyword = self.keyword("rename", rdepth)?;
        let set = self.import_set(rdepth)?;

        let pairs = self.rename_pairs(rdepth)?;

        Self::list(vec![keyword, set, pairs])
    }

    fn rename_pairs(&mut self, rdepth: usize) -> Result<R, R> {
        Self::list(self.one_or_more(Reader::identifier_pair, rdepth)?)
    }

    fn cond_expand(&mut self, rdepth: usize) -> Result<R, R> {
        let keyword = self.keyword("cond-expand", rdepth)?;
        let clauses = self.cond_expand_clauses(rdepth)?;

        Self::list(vec![keyword, clauses])
    }

    fn cond_expand_clauses(&mut self, rdepth: usize) -> Result<R, R> {
        Self::list(self.one_or_more(Reader::cond_clause, rdepth)?)
    }

    fn cond_clause(&mut self, rdepth: usize) -> Result<R, R> {
        self.paren_left(rdepth)?;
        let requirement = self.feature_requirement(rdepth + 1)?;
        let declarations = self.library_declarations(rdepth + 1)?;
        self.paren_right(rdepth + 1)?;

        let mut vec = vec![requirement, declarations];

        if matches!(self.peek_or_eof()?, Token::ParenLeft) {
            let else_clause = self.cond_expand_else(rdepth)?;
            vec.push(else_clause);
        }

        Self::list(vec)
    }

    fn cond_expand_else(&mut self, rdepth: usize) -> Result<R, R> {
        self.paren_left(rdepth)?;
        let keyword = self.keyword("else", rdepth + 1)?;
        let declarations = self.library_declarations(rdepth + 1)?;
        self.paren_right(rdepth + 1)?;

        Self::list(vec![keyword, declarations])
    }

    fn feature_requirements(&mut self, rdepth: usize) -> Result<R, R> {
        let requirements = self.zero_or_more(Reader::feature_requirement, rdepth)?;
        Self::list(requirements)
    }

    fn feature_requirement(&mut self, rdepth: usize) -> Result<R, R> {
        match self.peek_or_eof()? {
            Token::Identifier(_) => self.identifier(rdepth),
            Token::ParenLeft => self.feature_requirement_with_paren(rdepth),
            _ => Err(A::syntax_error(
                rdepth,
                "expected identifier or open parenthesis",
            )),
            // token @ _ => Err(unexpected(rdepth, token.to_string(), "identifier or open parenthesis".to_string()))
        }
    }

    fn feature_requirement_with_paren(&mut self, rdepth: usize) -> Result<R, R> {
        self.paren_left(rdepth)?;

        let requirement = match self.peek_or_eof()? {
            Token::Identifier(id) => match id.as_str() {
                "and" => self.and(rdepth + 1),
                "or" => self.or(rdepth + 1),
                "not" => self.not(rdepth + 1),
                _ => self.library_name_after_open(rdepth + 1),
            },
            _ => self.library_name_after_open(rdepth + 1),
        };

        self.paren_right(rdepth + 1)?;

        requirement
    }

    fn and(&mut self, rdepth: usize) -> Result<R, R> {
        let keyword = self.keyword("and", rdepth)?;
        let requirements = self.feature_requirements(rdepth)?;
        Self::list(vec![keyword, requirements])
    }

    fn or(&mut self, rdepth: usize) -> Result<R, R> {
        let keyword = self.keyword("or", rdepth)?;
        let requirements = self.feature_requirements(rdepth)?;
        Self::list(vec![keyword, requirements])
    }

    fn not(&mut self, rdepth: usize) -> Result<R, R> {
        let keyword = self.keyword("not", rdepth)?;
        let requirement = self.feature_requirement(rdepth)?;

        Self::list(vec![keyword, requirement])
    }

    fn include_library_declarations(&mut self, rdepth: usize) -> Result<R, R> {
        let keyword = self.keyword("include-library-declarations", rdepth)?;
        let strings = self.strings(rdepth)?;

        Self::list(vec![keyword, strings])
    }

    ///
    /// Conditionals
    ///
    ///
    ///
    fn iff(&mut self, rdepth: usize) -> Result<R, R> {
        let iff = self.keyword("if", rdepth)?;
        let test = self.expr(rdepth)?;
        let consequent = self.expr(rdepth)?;

        let mut vec = vec![iff, test, consequent];

        if !matches!(self.peek_or_eof()?, Token::ParenRight) {
            vec.push(self.expr(rdepth)?);
        }

        Self::list(vec)
    }

    ///
    /// Rssignments
    ///
    ///
    ///

    fn assignment(&mut self, rdepth: usize) -> Result<R, R> {
        let keyword = self.keyword("set!", rdepth)?;

        let id = self.identifier(rdepth)?;
        let expr = self.expr(rdepth)?;

        Self::list(vec![keyword, id, expr])
    }

    fn literal(&mut self, rdepth: usize) -> Result<R, R> {
        // checks for a single-token literal, i.e. boolean, character, number or string
        match self.peek_or_eof()? {
            Token::Boolean(_) | Token::Character(_) | Token::Number(_) | Token::String(_) => {
                match self.get_next_token() {
                    Some(token) => match token {
                        Token::Boolean(b) => Ok(A::boolean(b)),
                        Token::Character(c) => Ok(A::char(c)),
                        Token::Number(n) => Ok(A::number(n)),
                        Token::String(s) => Ok(A::string(s)),
                        _ => Err(A::syntax_error(rdepth, "unexpected token")),
                        // _ => Err(unexpected(rdepth, token.to_string(), "literal".to_string())),
                    },
                    None => Err(A::eof_object()),
                }
            }
            _ => Err(A::syntax_error(rdepth, "unexpected token")),
            // token @ _ => Err(unexpected(rdepth, token.to_string(), "literal".to_string())),
        }
    }

    ///
    /// Macro blocks
    ///
    ///
    ///

    fn macro_block(&mut self, rdepth: usize) -> Result<R, R> {
        // we look for the keywords let-syntax or letrec-syntax
        match self.peek_or_eof()? {
            Token::Identifier(id) => match id.as_str() {
                "let-syntax" | "letrec-syntax" => {
                    let keyword = self.identifier(rdepth)?;
                    self.paren_left(rdepth)?;
                    let syntax_specs = self.syntax_specs(rdepth + 1)?;
                    self.paren_right(rdepth + 1)?;
                    let body = self.body(rdepth)?;
                    Self::list(vec![keyword, syntax_specs, body])
                }
                _ => Err(A::syntax_error(rdepth, "unexpected token")),
                //  _ => Err(unexpected(rdepth, token.to_string(), "let-syntax or letrec-syntax".to_string())),
            },
            _ => Err(A::syntax_error(rdepth, "unexpected token")),
            // token @ _ => Err(unexpected(rdepth, token.to_string(), "let-syntax or letrec-syntax".to_string())),
        }
    }

    fn syntax_specs(&mut self, rdepth: usize) -> Result<R, R> {
        let syntax_specs = self.zero_or_more(Reader::syntax_spec, rdepth)?;
        Self::list(syntax_specs)
    }

    fn syntax_spec(&mut self, rdepth: usize) -> Result<R, R> {
        // ( <keyword> <transformer spec> )
        self.paren_left(rdepth)?;
        let keyword = self.identifier(rdepth + 1)?;
        let transformer_spec = self.transformer_spec(rdepth + 1)?;
        self.paren_right(rdepth + 1)?;
        Self::list(vec![keyword, transformer_spec])
    }

    //
    // Quasiquotation
    //
    //
    //

    fn quasiquotation(&mut self, rdepth: usize, qqdepth: u32) -> Result<R, R> {
        let keyword = self.keyword("quasiquote", rdepth)?;

        let template = self.qq_template(rdepth, qqdepth)?;

        Self::list(vec![keyword, template])
    }

    fn quasiquotation_short(&mut self, rdepth: usize, qqdepth: u32) -> Result<R, R> {
        self.quasiquote(rdepth)?;
        // let keyword = Rc::new(RefCell::new(Object::Identifier(Identifier::Keyword(Keyword::from("quasiquote")))));
        let keyword = A::keyword(Keyword::Quasiquote);
        let template = self.qq_template(rdepth, qqdepth)?;
        Self::list(vec![keyword, template])
    }

    fn qq_template(&mut self, rdepth: usize, qqdepth: u32) -> Result<R, R> {
        match qqdepth {
            0 => self.expr(rdepth),
            _ => match self.peek_or_eof()? {
                Token::Boolean(_)
                | Token::Character(_)
                | Token::String(_)
                | Token::Number(_)
                | Token::Identifier(_)
                | Token::SharpU8Open => self.simple_datum(rdepth),
                Token::SharpOpen => self.qq_template_vector(rdepth, qqdepth),
                Token::Comma => self.qq_template_unquotation_short(rdepth, qqdepth),
                Token::Quote => {
                    self.quote(rdepth)?;
                    self.qq_template(rdepth, qqdepth)
                }
                Token::Quasiquote => self.quasiquotation_short(rdepth, qqdepth + 1),
                Token::ParenLeft => {
                    self.paren_left(rdepth)?;

                    match self.peek_or_eof()? {
                        Token::Identifier(id) if matches!(id.as_str(), "quasiquote") => {
                            self.quasiquotation(rdepth + 1, qqdepth + 1)
                        }
                        Token::Identifier(id) if matches!(id.as_str(), "unquote") => {
                            self.qq_template_unquotation(rdepth + 1, qqdepth)
                        }
                        _ => {
                            let list = self.qq_template_or_splice_list(rdepth + 1, qqdepth)?;
                            self.paren_right(rdepth + 1)?;
                            Self::list(list)
                        }
                    }
                }
                _ => Err(A::syntax_error(rdepth, "unexpected token")),
                // token @ _ => Err(unexpected(rdepth, token.to_string(), "identifier, literal or list")),
            },
        }
    }

    fn qq_template_or_splice(&mut self, rdepth: usize, qqdepth: u32) -> Result<R, R> {
        match self.peek_or_eof()? {
            Token::CommaAt => self.qq_splice_unquotation(rdepth, qqdepth),
            _ => self.qq_template(rdepth, qqdepth),
        }
    }

    fn qq_template_or_splice_list(&mut self, rdepth: usize, qqdepth: u32) -> Result<Vec<R>, R> {
        Ok(from_fn(|| self.qq_template_or_splice(rdepth, qqdepth).ok()).collect::<Vec<R>>())
    }

    fn qq_splice_unquotation(&mut self, rdepth: usize, qqdepth: u32) -> Result<R, R> {
        self.comma_at(rdepth)?;
        self.qq_template(rdepth, qqdepth - 1)
    }

    fn qq_template_unquotation_short(&mut self, rdepth: usize, qqdepth: u32) -> Result<R, R> {
        let keyword = self.comma(rdepth)?;
        let template = self.qq_template(rdepth, qqdepth - 1)?;

        Self::list(vec![keyword, template])
    }

    fn qq_template_unquotation(&mut self, rdepth: usize, qqdepth: u32) -> Result<R, R> {
        let keyword = self.keyword("unquote", rdepth)?;
        let template = self.qq_template(rdepth, qqdepth - 1)?;
        self.paren_right(rdepth)?;

        Self::list(vec![keyword, template])
    }

    fn qq_template_vector(&mut self, rdepth: usize, qqdepth: u32) -> Result<R, R> {
        let keyword = self.sharpopen(rdepth)?;
        self.keyword("vector", rdepth + 1)?;
        let qq_templates = self.qq_templates(rdepth + 1, qqdepth)?;
        self.paren_right(rdepth + 1)?;

        Self::list(vec![keyword, qq_templates])
    }

    fn qq_templates(&mut self, rdepth: usize, qqdepth: u32) -> Result<R, R> {
        let templates = self.qq_template_list(rdepth, qqdepth)?;
        Self::list(templates)
    }

    fn qq_template_list(&mut self, rdepth: usize, qqdepth: u32) -> Result<Vec<R>, R> {
        Ok(from_fn(|| self.qq_template(rdepth, qqdepth).ok()).collect::<Vec<R>>())
    }

    ///
    /// Quotations
    ///
    ///
    ///

    fn quotation(&mut self, rdepth: usize) -> Result<R, R> {
        let _keyword = self.keyword("quote", rdepth)?;
        let datum = self.datum(rdepth)?;

        Ok(A::quotation(&datum))
    }

    fn quotation_short(&mut self, rdepth: usize) -> Result<R, R> {
        // Implements '<datum> (i.e. single quote followed by datum)
        let _quote = self.quote(rdepth)?;
        let datum = self.datum(rdepth)?;

        Ok(A::quotation(&datum))
    }

    fn identifier_list_possible_dot(&mut self, rdepth: usize) -> Result<R, R> {
        match self.peek_or_eof()? {
            Token::ParenRight => Self::list(vec![]),
            _ => {
                let ids = self.one_or_more(Reader::identifier, rdepth)?;
                match self.peek_or_eof()? {
                    Token::Dot => {
                        self.dot(rdepth)?;
                        Self::list_not_null_terminated(ids, &self.identifier(rdepth)?)
                    }
                    _ => Self::list(ids),
                }
            }
        }
    }

    ///
    /// Transformer (R7RS section 7.1.5)
    /// [INCOMPLETE]
    ///

    fn transformer_spec(&mut self, rdepth: usize) -> Result<R, R> {
        self.paren_left(rdepth)?;
        self.keyword("syntax-rules", rdepth + 1)?;

        let id = self.identifier(rdepth + 1);

        self.paren_left(rdepth + 2)?;
        let ids = self.transformer_spec_ids(rdepth + 2)?;
        self.paren_right(rdepth + 2)?;

        let syntax_rules = self.syntax_rules(rdepth + 1)?;

        self.paren_right(rdepth + 1)?;

        let children = match id {
            Ok(id) => vec![id, ids, syntax_rules],
            Err(_) => vec![ids, syntax_rules],
        };

        Self::list(children)
    }

    fn transformer_spec_ids(&mut self, rdepth: usize) -> Result<R, R> {
        let ids = self.zero_or_more(Reader::identifier, rdepth)?;
        Self::list(ids)
    }

    fn syntax_rules(&mut self, rdepth: usize) -> Result<R, R> {
        let syntax_rules = self.zero_or_more(Reader::syntax_rule, rdepth)?;
        Self::list(syntax_rules)
    }

    fn syntax_rule(&mut self, rdepth: usize) -> Result<R, R> {
        // ( <pattern> <template> )
        self.paren_left(rdepth)?;
        let pattern = self.pattern(rdepth + 1)?;
        let template = self.template(rdepth + 1)?;
        self.paren_right(rdepth + 1)?;
        Self::list(vec![pattern, template])
    }

    fn pattern(&mut self, rdepth: usize) -> Result<R, R> {
        match self.peek_or_eof()? {
            Token::Identifier(id) => match id.as_str() {
                "_" => self.pattern_underscore(rdepth),
                _ => self.pattern_identifier(rdepth),
            },
            Token::Boolean(_) | Token::Character(_) | Token::String(_) | Token::Number(_) => {
                self.pattern_datum(rdepth)
            }
            Token::ParenLeft => self.pattern_with_paren(rdepth),
            Token::SharpOpen => self.pattern_with_sharp_paren(rdepth),
            _ => Err(A::syntax_error(rdepth, "unexpected token")),
            // token @ _ => Err(unexpected(rdepth, token.to_string(), "identifier, literal or list".to_string())),
        }
    }

    fn pattern_datum(&mut self, rdepth: usize) -> Result<R, R> {
        self.datum(rdepth)
    }

    fn pattern_identifier(&mut self, rdepth: usize) -> Result<R, R> {
        let token = self.peek_or_eof()?;

        match token {
            Token::Identifier(s) if s.as_str() == "..." => {
                Ok(A::syntax_error(rdepth, "ellipsis not allowed in pattern"))
            }
            // Err(unexpected(rdepth, "...".to_string(), "identifier".to_string())),
            _ => self.identifier(rdepth),
        }
    }

    fn pattern_underscore(&mut self, rdepth: usize) -> Result<R, R> {
        self.keyword("_", rdepth)
    }

    fn pattern_with_paren(&mut self, rdepth: usize) -> Result<R, R> {
        self.paren_left(rdepth)?;
        let patterns = self.pattern_with_paren_a(rdepth + 1)?;
        self.paren_right(rdepth + 1)?;

        Ok(patterns)
    }

    fn pattern_with_paren_a(&mut self, rdepth: usize) -> Result<R, R> {
        // | <pattern>*
        // | <pattern>+ . <pattern>
        // | <pattern>* <pattern> <ellipsis> <pattern>*
        // | <pattern>* <pattern> <ellipsis> <pattern>* . <pattern>

        match self.peek_or_eof()? {
            Token::ParenRight => Self::list(vec![]), // empty

            _ => {
                // initially we have not seen an ellipse or any patterns before or after ellipse
                let mut pre_ellipse_patterns: Vec<R>;
                let mut post_ellipse_patterns: Vec<R> = vec![];
                let mut ellipse = false;

                // we look for patterns before ellipse
                pre_ellipse_patterns = self.zero_or_more(Reader::pattern, rdepth)?;

                match self.peek_or_eof()? {
                    Token::ParenRight => (), // no dot
                    Token::Dot => {
                        self.dot(rdepth)?;
                        let pattern = self.pattern(rdepth)?;
                        pre_ellipse_patterns.push(pattern);

                        match self.peek_or_eof()? {
                            Token::ParenRight => (), // dot with no ellipse
                            Token::Identifier(id) if id.as_str() == "..." =>
                            // dot with ellipse
                            {
                                ellipse = true;
                                let _ellipsis = self.identifier(rdepth)?;
                                post_ellipse_patterns =
                                    self.zero_or_more(Reader::pattern, rdepth)?;

                                if !matches!(self.peek_or_eof()?, Token::ParenRight) {
                                    // dot with ellipse and dot
                                    self.dot(rdepth)?;
                                    let pattern = self.pattern(rdepth)?;
                                    post_ellipse_patterns.push(pattern);
                                }
                            }
                            _ => (),
                        }
                    }
                    Token::Identifier(id) if id.as_str() == "..." => {
                        ellipse = true;
                        let _ellipsis = self.identifier(rdepth)?;
                        post_ellipse_patterns = self.zero_or_more(Reader::pattern, rdepth)?;

                        if !matches!(self.peek_or_eof()?, Token::ParenRight) {
                            self.dot(rdepth)?;
                            let pattern = self.pattern(rdepth)?;
                            post_ellipse_patterns.push(pattern);
                        }
                    }
                    _ => (),
                }

                let mut vec = vec![Self::list(pre_ellipse_patterns)?];

                if ellipse {
                    vec.push(Self::list(post_ellipse_patterns)?);
                }

                Self::list(vec)
            }
        }
    }

    fn pattern_with_sharp_paren(&mut self, rdepth: usize) -> Result<R, R> {
        //  #( <pattern>* )
        // | #( <pattern>* <pattern> <ellipsis> <pattern>* )

        self.sharpopen(rdepth)?;
        let patterns = self.pattern_with_sharp_paren_a(rdepth + 1)?;
        self.paren_right(rdepth + 1)?;

        Self::list(vec![patterns])
    }

    fn pattern_with_sharp_paren_a(&mut self, rdepth: usize) -> Result<R, R> {
        // initially we have not seen an ellipse or any patterns before or after ellipse
        let mut pre_ellipse_patterns: Vec<R> = vec![];
        let mut post_ellipse_patterns: Vec<R> = vec![];
        let mut ellipse = false;

        match self.peek_or_eof()? {
            Token::ParenRight => (), // empty
            _ => {
                pre_ellipse_patterns = self.one_or_more(Reader::pattern, rdepth)?;
                match self.peek_or_eof()? {
                    Token::ParenRight => (),
                    Token::Identifier(id) if id.as_str() == "..." => {
                        let _ellipsis = self.identifier(rdepth)?;
                        ellipse = true;
                        post_ellipse_patterns = self.zero_or_more(Reader::pattern, rdepth)?;
                    }
                    _ => (),
                }
            }
        }

        let mut vec = vec![Self::list(pre_ellipse_patterns)?];

        if ellipse {
            vec.push(Self::list(post_ellipse_patterns)?);
        }

        Self::list(vec)
    }

    fn template(&mut self, rdepth: usize) -> Result<R, R> {
        match self.peek_or_eof()? {
            Token::Identifier(_) => self.template_identifier(rdepth),
            Token::ParenLeft => self.template_with_paren(rdepth),
            Token::SharpOpen => self.template_with_sharp_paren(rdepth),
            Token::Boolean(_) | Token::Character(_) | Token::String(_) | Token::Number(_) => {
                self.template_datum(rdepth)
            }
            _ => Err(A::syntax_error(rdepth, "unexpected token")),
            // token @ _ => Err(unexpected(rdepth, token.to_string(), "identifier, literal or list".to_string()))
        }
    }

    fn template_identifier(&mut self, rdepth: usize) -> Result<R, R> {
        self.pattern_identifier(rdepth)
    }

    fn template_with_paren(&mut self, rdepth: usize) -> Result<R, R> {
        self.paren_left(rdepth)?;

        let template = match self.peek_or_eof()? {
            Token::ParenRight => Ok(vec![]), // empty list
            _ => {
                let mut elements = self.one_or_more(Reader::template_element, rdepth + 1)?;
                match self.peek_or_eof()? {
                    Token::ParenRight => Ok(elements),
                    Token::Dot => {
                        self.dot(rdepth + 1)?;
                        elements.push(self.template_element(rdepth + 1)?);
                        Ok(elements)
                    }
                    _ => Err(A::syntax_error(rdepth, "unexpected token")),
                    // token @ _ => Err(unexpected(rdepth, token.to_string(), "close parenthesis or dot".to_string())),
                }
            }
        }?;

        self.paren_right(rdepth + 1)?;

        Self::list(template)
    }

    fn template_element(&mut self, rdepth: usize) -> Result<R, R> {
        let template = self.template(rdepth)?;

        match self.peek_or_eof()? {
            Token::Identifier(id) if id.as_str() == "..." => {
                let ellipsis = A::keyword(Keyword::Ellipsis);
                Self::list(vec![template, ellipsis])
            }
            _ => Self::list(vec![template]),
        }
    }

    fn template_with_sharp_paren(&mut self, rdepth: usize) -> Result<R, R> {
        self.sharpopen(rdepth)?;
        let elements = self.zero_or_more(Reader::template_element, rdepth + 1)?;
        self.paren_right(rdepth)?;

        Self::list(elements)
    }

    fn template_datum(&mut self, rdepth: usize) -> Result<R, R> {
        self.datum(rdepth)
    }

    ///
    /// Datum (R7RS section 7.1.3 - External Representation)
    ///
    ///
    ///

    fn datum(&mut self, rdepth: usize) -> Result<R, R> {
        match self.peek_or_eof()? {
            Token::Identifier(_) => self.symbol(rdepth),
            Token::ParenLeft => self.datum_list(rdepth),
            Token::SharpOpen => self.vector(rdepth),
            Token::Boolean(_) | Token::Character(_) | Token::String(_) | Token::Number(_) => {
                self.literal(rdepth)
            }
            Token::SharpU8Open => self.bytevector(rdepth),
            Token::Quote | Token::Quasiquote | Token::Comma | Token::CommaAt => {
                self.abbreviation(rdepth)
            }
            _ => Err(A::syntax_error(rdepth, "unexpected token")),
            // token @ _ => Err(unexpected(rdepth, token.to_string(), "identifier, literal or list".to_string()))
        }
    }

    fn simple_datum(&mut self, rdepth: usize) -> Result<R, R> {
        match self.peek_or_eof()? {
            Token::Identifier(_) => self.symbol(rdepth),
            Token::Boolean(_) | Token::Character(_) | Token::String(_) | Token::Number(_) => {
                self.literal(rdepth)
            }
            Token::SharpU8Open => self.bytevector(rdepth),
            _ => Err(A::syntax_error(rdepth, "unexpected token")),
            // token @ _ => Err(unexpected(rdepth, token.to_string(), "identifier, literal or list".to_string()))
        }
    }

    fn symbol(&mut self, rdepth: usize) -> Result<R, R> {
        self.identifier(rdepth)
    }

    fn abbreviation(&mut self, rdepth: usize) -> Result<R, R> {
        let prefix = match self.peek_or_eof()? {
            Token::Quote => self.quote(rdepth),
            Token::Quasiquote => self.quasiquote(rdepth),
            Token::Comma => self.comma(rdepth),
            Token::CommaAt => self.comma_at(rdepth),
            _ => Err(A::syntax_error(rdepth, "unexpected token")),
        }?;

        let expr = self.datum(rdepth)?;

        Self::list(vec![prefix, expr])
    }

    fn datum_list(&mut self, rdepth: usize) -> Result<R, R> {
        self.paren_left(rdepth)?;

        let data = match self.peek_or_eof()? {
            Token::ParenRight => Ok(A::null()),
            _ => {
                let data = self.one_or_more(Reader::datum, rdepth + 1)?;

                match self.peek_or_eof()? {
                    Token::Dot => {
                        self.dot(rdepth + 1)?;
                        Self::list_not_null_terminated(data, &self.datum(rdepth + 1)?)
                    }
                    _ => Self::list(data),
                }
            }
        };

        self.paren_right(rdepth + 1)?;

        data
    }

    fn uinteger10(&mut self, rdepth: usize) -> Result<R, R> {
        match self.peek_or_eof()? {
            Token::Number(n) if n.chars().all(|c| c.is_ascii_digit()) => {
                match self.get_next_token() {
                    Some(Token::Number(n)) => Ok(A::number(n)),
                    _ => Err(A::eof_object()),
                }
            }
            _ => Err(A::syntax_error(rdepth, "unexpected token")),
            // token @ _ => Err(unexpected(rdepth, token.to_string(), "uninteger10".to_string())),
        }
    }

    fn vector(&mut self, rdepth: usize) -> Result<R, R> {
        self.sharpopen(rdepth)?;
        let data = self.zero_or_more(Reader::expr, rdepth + 1)?;
        self.paren_right(rdepth + 1)?;
        Ok(A::vector(&Self::list(data)?))
    }

    fn bytevector(&mut self, rdepth: usize) -> Result<R, R> {
        self.sharpu8open(rdepth)?;
        let data = self.zero_or_more(Reader::expr, rdepth + 1)?;
        self.paren_right(rdepth + 1)?;

        Ok(A::bytevector(&Self::list(data)?))
    }

    fn zero_or_more(
        &mut self,
        closure: fn(&mut Self, rdepth: usize) -> Result<R, R>,
        rdepth: usize,
    ) -> Result<Vec<R>, R> {
        Ok(from_fn(|| closure(self, rdepth).ok()).collect())
    }

    fn one_or_more(
        &mut self,
        closure: fn(&mut Self, rdepth: usize) -> Result<R, R>,
        rdepth: usize,
    ) -> Result<Vec<R>, R> {
        Ok(once(closure(self, rdepth)?)
            .chain(from_fn(|| closure(self, rdepth).ok()))
            .collect())
    }

    fn keyword(&mut self, keyword: &str, rdepth: usize) -> Result<R, R> {
        match self.peek_or_eof()? {
            Token::Identifier(s) if keyword == s => self.keyword_box_leaf_from_next(rdepth),
            _ => Err(A::syntax_error(rdepth, "unexpected token")),
            // token @ _ => Err(unexpected(rdepth, token.to_string(), "a keyword".to_string()))
        }
    }

    fn keyword_box_leaf_from_next(&mut self, rdepth: usize) -> Result<R, R> {
        match self.get_next_token() {
            Some(Token::Identifier(s)) => Ok(A::keyword(Keyword::from(s))),
            None => Err(A::eof_object()),
            _ => Err(A::syntax_error(rdepth, "unexpected token")),
            // token @ _ => Err(unexpected(rdepth, token.unwrap().to_string(), "a keyword".to_string())),
        }
    }

    fn punctuation(&mut self, rdepth: usize, expected: Token, s: &'static str) -> Result<R, R> {
        match self.peek_or_eof()? {
            t if t == expected => {
                self.get_next_token();
                Ok(A::symbol(s))
            }
            _ => Err(A::syntax_error(rdepth, "unexpected token")),
            // token @ _ => Err(unexpected(rdepth, token.to_string(), s.to_string())),
        }
    }

    fn comma(&mut self, rdepth: usize) -> Result<R, R> {
        self.punctuation(rdepth, Token::Comma, ",")
    }

    fn comma_at(&mut self, rdepth: usize) -> Result<R, R> {
        self.punctuation(rdepth, Token::CommaAt, ",@")
    }

    fn dot(&mut self, rdepth: usize) -> Result<R, R> {
        self.punctuation(rdepth, Token::Dot, ".")
    }

    fn paren_right(&mut self, rdepth: usize) -> Result<R, R> {
        self.punctuation(rdepth, Token::ParenRight, ")")
    }

    fn paren_left(&mut self, rdepth: usize) -> Result<R, R> {
        self.punctuation(rdepth, Token::ParenLeft, "(")
    }

    fn quasiquote(&mut self, rdepth: usize) -> Result<R, R> {
        self.punctuation(rdepth, Token::Quasiquote, "`")
    }

    fn quote(&mut self, rdepth: usize) -> Result<R, R> {
        self.punctuation(rdepth, Token::Quote, "'")
    }

    fn sharpopen(&mut self, rdepth: usize) -> Result<R, R> {
        self.punctuation(rdepth, Token::SharpOpen, "#(")
    }

    fn sharpu8open(&mut self, rdepth: usize) -> Result<R, R> {
        self.punctuation(rdepth, Token::SharpU8Open, "#u8(")
    }

    fn strings(&mut self, rdepth: usize) -> Result<R, R> {
        Self::list(self.one_or_more(Reader::string, rdepth)?)
    }

    fn string(&mut self, rdepth: usize) -> Result<R, R> {
        match self.peek_or_eof()? {
            Token::String(_) => {
                let next = self.get_next_token();
                match next {
                    Some(token) => match token {
                        Token::String(s) => Ok(A::string(s)),
                        _ => Err(A::syntax_error(rdepth, "unexpected token")),
                        // _ => Err(unexpected(rdepth, token.to_string(), "string".to_string())),
                    },
                    None => Ok(A::eof_object()),
                }
            }
            _ => Err(A::syntax_error(rdepth, "unexpected token")),
            // token @ _ => Err(unexpected(rdepth, token.to_string(), "string".to_string())),
        }
    }

    fn identifier(&mut self, rdepth: usize) -> Result<R, R> {
        match self.peek_or_eof()? {
            Token::Identifier(_) => {
                if let Some(Token::Identifier(id)) = self.get_next_token() {
                    Ok(A::symbol(id.as_str()))
                } else {
                    Err(A::eof_object())
                }
            }
            _ => Err(A::syntax_error(rdepth, "unexpected token")),
            // token @ _ => Err(unexpected(rdepth, token.to_string(), "identifier".to_string())),
        }
    }

    fn identifier_pair(&mut self, rdepth: usize) -> Result<R, R> {
        self.paren_left(rdepth)?;
        let id1 = self.identifier(rdepth + 1)?;
        let id2 = self.identifier(rdepth + 1)?;

        self.paren_right(rdepth + 1)?;

        Self::list(vec![id1, id2])
    }

    fn peek_or_eof(&mut self) -> Result<Token, R> {
        self.peek_next_token().ok_or(A::eof_object())
    }
}
