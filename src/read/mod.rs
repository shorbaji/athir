//! Athir parser module
//! Implements recursive descent parser for R7RS Scheme
//! 
//! TODO:
//! 
//! - review for readability
//! - documentation
//! - tests
//! 
//! Example usage:
//! ```
//!     // we first create a Source using an iterator over String
//!     // in this example we use stdin
//!     // we then create a Parser using the Source
//!     // we then iterate over the Parser to get expressions
//!     
//!     let source = Source::new(std::io::stdin().lines().map(|line| line.unwrap()));
//!    
//!     let parser = Parser::new(source);
//!     for expr in parser {
//!         match expr { // expr is of type Result<Box<Node>, Error>
//!             Ok(expr) => println!("{:?}", expr),
//!             Err(err) => println!("{:?}", err),
//!         }
//!     }
//! ```
//!
//! Deviations from R7RS:
//! - string inline hex escapes not implemented
//! - no support for # label in datum.
//! - derived expressions not implemented
//!

mod error;
mod lexer; // lexical analyzer
mod expr; // abstract syntax tree node

#[cfg(test)]
mod tests;

use std::iter::{once, from_fn};
use lexer::{Lexer, Token};
use error::{SyntaxError, SyntaxErrorKind};

pub use lexer::Source;
pub use expr::{Literal, Keyword, Identifier, Expr};

pub type ParseResult = Result<Box<Expr>, SyntaxError>;
type ParseVecResult = Result<Vec<Box<Expr>>, SyntaxError>;

/// 
/// Parser 
/// 
/// Uses a peekable Lexer to get tokens and then parses them into an AST
/// (see mod crate::read::lexer)

pub struct Parser <T: Iterator<Item=String>> {
    lexer: Lexer<T>,
}

impl<T> Iterator for Parser<T> where T: Iterator<Item = String> {
    type Item = ParseResult;

    fn next(&mut self) -> Option<Self::Item> {
        match self.peek() {
            Ok(_) => Some(self.read()),
            Err(_) => None,
        }
    }
}

impl<T> Parser<T> where T: Iterator<Item = String> {
    pub fn new(source: T) -> Self {
        Self { 
            lexer: Lexer::new(source),
        }
    }
    
    pub fn read(&mut self) -> ParseResult {
        self.expr().or_else(|e| {
            self.lexer.next(); // if an error consume the token that caused it
            Err(e)
        })
    }

    /// 
    /// Expressions
    /// 
    /// This function implements the following rules from R7RS 7.1.3
    /// 
    /// <expression> ::= <identifier>
    /// | <literal>
    /// | <procedure call>
    /// | <lambda expression>
    /// | <conditional>
    /// | <assignment>
    /// | <derived expression>
    /// | <macro use>
    /// | <macro block>
    /// | <includer>
    /// 
    /// <literal> ::= <quotation> | <self-evaluating>
    /// 
    /// <self-evaluating> ::= <boolean> | <number> | <vector> 
    /// | <character> | <string> | <bytevector>
    /// 
    /// <quotation> ::= ’<datum> | ( quote <datum> )
    /// 
    /// 
    /// Known issues:
    /// - includer is not implemented
    /// - macro block is not implemented
    /// - derived expression is not implemented
    /// - macro use is not implemented
    /// - vector and bytevector are not fully implemented
    /// 
    /// 
    /// 1. look for self-evaluating literal expressions
    /// 2. handle '<datum> quotation directly and leave (quote <datum>) 
    ///    to compound_expr() since if begins with a parenthesis
    /// 3. look for identifier
    /// 4. look for a parenthesis for compount expressions
    /// 5. return an error to handle unexpected tokens like ,@ etc
    /// 
    
    fn expr(&mut self) -> ParseResult {
        let result = match self.peek()? {
            Token::ParenLeft => self.compound_expr(),
            _ => self.atom(),
        };

        match result {
            Ok(expr) => Ok(expr),
            Err(err) => Err(err),
        }
    }

    fn atom(&mut self) -> ParseResult {  
        self.boolean()
        .or_else(|_| self.character())
        .or_else(|_| self.number())
        .or_else(|_| self.string())
        .or_else(|_| self.identifier())
        .or_else(|_| self.vector())
        .or_else(|_| self.bytevector())
        .or(self.quotation_short())
        .or(self.quasiquotation_short(1))
    }

    fn boolean(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Boolean(_) => {
                let next = self.lexer.next();
                match next {
                    Some(token) => match token {
                            Token::Boolean(b) => Ok(Box::new(Expr::Literal(Literal::Boolean(b)))),
                            _ => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken { unexpected: token.to_string(), expected: "boolean" })),
                        },
                    None => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedEOF)),
                }
            },
            token @ _ => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken { unexpected: token.to_string(), expected: "boolean" })),
        }
    }

    fn character(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Character(_) => {
                let next = self.lexer.next();
                match next {
                    Some(token) => match token {
                            Token::Character(c) => Ok(Box::new(Expr::Literal(Literal::Character(c)))),
                            _ => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken { unexpected: token.to_string(), expected: "character" })),
                        },
                    None => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedEOF)),
                }
            },
            token @ _ => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken { unexpected: token.to_string(), expected: "character" })),
        }
    }

    
    fn string(&mut self) -> ParseResult {
        match self.peek()? {
            Token::String(_) => {
                let next = self.lexer.next();
                match next {
                    Some(token) => match token {
                            Token::String(s) => Ok(Box::new(Expr::Literal(Literal::String(s)))),
                            _ => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken { unexpected: token.to_string(), expected: "string" })),
                        },
                    None => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedEOF)),
                }
            },
            token @ _ => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken { unexpected: token.to_string(), expected: "string" })),
        }
    }

    fn number(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Number(_) => {
                let next = self.lexer.next();
                match next {
                    Some(token) => match token {
                            Token::Number(n) => Ok(Box::new(Expr::Literal(Literal::Number(n)))),
                            _ => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken { unexpected: token.to_string(), expected: "number" })),
                        },
                    None => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedEOF)),
                }
            },
            token @ _ => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken { unexpected: token.to_string(), expected: "number" })),
        }
    }

    /// 
    /// this function handles a non-atomic expression 
    /// starting with a parenthesis we then first look for keywords
    /// if not keyword found we fall back to a procedure call
    /// 
    
    fn compound_expr(&mut self) -> ParseResult {
        
        // <compound expression> ::= <special form> | <procedure call>
        // starts with a left parenthesis
        // we look for a keyword for a special form or else we treat as a procedure call
        // we then look for a right parenthesis

        self.paren_left()?;

        let expr = match self.peek()? {
            token @ Token::Identifier(id) => 
                match id.as_str() {
                    "begin" => self.begin(),
                    "define" => self.define(),
                    "define-values" => self.define_values(),
                    "define-record-type" => self.define_record_type(),
                    "define-syntax" => self.define_syntax(),
                    "define-library" => self.define_library(),
                    "if" => self.iff(),
                    "include" => self.include(),
                    "include-ci" => self.include(),
                    "lambda" => self.lambda(),
                    "let-syntax" | "letrec-syntax" => self.macro_block(),
                    "quasiquote" => self.quasiquotation(1),
                    "quote" => self.quotation(),
                    "set!" => self.assignment(),
                    "unquote" => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken { unexpected: token.to_string() , expected: "operator or other keyword" })),
                    _ => self.procedure_call(),
                },
            _ => self.procedure_call(),
        }?;

        self.paren_right()?;

        Ok(expr)
    }
    
    ///
    /// Begin
    /// 
    /// 
    ///

    fn begin(&mut self) -> ParseResult {
        // we look for the keyword begin 
        // we then look for definitions. if that is all then we return a BeginDef
        // but if there is an expression then we return a Begin

        let begin = self.keyword("begin")?;

        let exprs = self.begin_exprs()?;

        Expr::list(vec!(begin, exprs))
    }

    fn begin_exprs(&mut self) -> ParseResult {
        let exprs = self.zero_or_more(Parser::expr)?;

        // we add a tag to the end of the list to indicate if all expressions are definitions
        let is_def = Box::new(
                        Expr::Literal(
                            Literal::Boolean(
                                exprs
                                .iter()
                                .all(|node| node.is_definition_expr())
                            )
                        )
                    );
        
        let list = Expr::list(exprs)?;

        Expr::list(vec!(list, is_def))
    }

    ///
    /// Definitions
    /// 
    /// We start with the define keyword and then look for either 
    /// - an identifier (i.e. a variable definition) or,
    /// - a parenthesized list of identifiers (i.e. a function definition)
    /// 

    fn define(&mut self) -> ParseResult {
        let keyword = self.keyword("define")?;
        match self.peek()? {
            Token::Identifier(_) => { // variable definition
                let var = self.identifier()?;
                let expr = self.expr()?;
                Expr::list(vec!(keyword, var, expr))
            },
            Token::ParenLeft => { // function definition
                self.paren_left()?;
                let var = self.identifier()?;
                let formals = self.def_formals()?;
                self.paren_right()?;
                let body = self.body()?;
                Expr::list(vec!(keyword, var, formals, body))
            },
            token @ _ => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken{unexpected: token.to_string(), expected: "identifier or open paren"})),
        }
    }

    fn def_formals(&mut self) -> ParseResult {
        self.identifier_list_possible_dot()
    }

    fn define_values(&mut self) -> ParseResult {
        let keyword = self.keyword("define-values")?;
        let formals = self.formals()?;
        let exprs = self.body()?;

        Expr::list(vec!(keyword, formals, exprs))
    }

    fn define_record_type(&mut self) -> ParseResult {
        let keyword = self.keyword("define-record-type")?;
        let id1 = self.identifier()?;
        let constructor = self.constructor()?;
        let id2 = self.identifier()?;
        let field_specs = self.field_specs()?;

        Expr::list(vec!(keyword, id1, constructor, id2, field_specs))
    }

    fn constructor(&mut self) -> ParseResult {
        self.paren_left()?;
        let id = self.identifier()?;
        let field_names = self.field_names()?;
        self.paren_right()?;

        Expr::list(vec!(id, field_names))
    }

    fn field_names(&mut self) -> ParseResult {
        let names = self.zero_or_more(Parser::identifier)?;
        Expr::list(names)
    }

    fn field_specs(&mut self) -> ParseResult {
        let specs = self.zero_or_more(Parser::field_spec)?;
        Expr::list(specs)
    }

    fn field_spec(&mut self) -> ParseResult {
        self.paren_left()?;

        let field_name = self.identifier()?;
        let accessor = self.identifier()?;

        let mut vec = vec!(field_name, accessor);
        if let Token::Identifier(_) = self.peek()? {
            vec.push(self.identifier()?);
        };

        self.paren_right()?;

        Expr::list(vec)
    }

    fn define_syntax(&mut self) -> ParseResult {
        let keyword = self.keyword("define-syntax")?;
        let id = self.identifier()?;
        let expr = self.transformer_spec()?;

        Expr::list(vec!(keyword, id, expr))
    }

    ///
    /// Conditionals
    /// 
    /// 
    ///
    fn iff(&mut self) -> ParseResult {
        let iff = self.keyword("if")?;
        let test = self.expr()?;
        let consequent = self.expr()?;

        let mut vec = vec!(iff, test, consequent);

        if !matches!(self.peek()?, Token::ParenRight) {
            vec.push(self.expr()?);
        }

        Expr::list(vec)
        // Expr::List(vec)
    }

    ///
    /// Lambda
    /// 
    /// 
    ///

    fn lambda(&mut self) -> ParseResult {
        let keyword = self.keyword("lambda")?;
        let formals = self.formals()?;

        let body = self.body()?;

        Expr::list(vec!(keyword, formals, body))
    }

    fn formals(&mut self) -> ParseResult {   
        match self.peek()? {
            Token::Identifier(_) => self.identifier(),
            Token::ParenLeft => {
                self.paren_left()?;
                let ids = self.identifier_list_possible_dot()?;
                self.paren_right()?;
                Ok(ids)
            }
            token @ _ => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken{unexpected: token.to_string(), expected: "identifier or open parenthesis"})),
        }
    }

    /// body should not be empty
    /// body can have defintions and expressions
    /// body cannot have definitions after any expression
    
    fn body(&mut self) -> ParseResult {       
        let exprs = self.one_or_more(Parser::expr)?;
        let mut defs = true;

        for expr in exprs.iter() {
            if expr.is_definition_expr() {
                if defs == false {
                    return Err(SyntaxError::new(SyntaxErrorKind::DefinitionsBeforeExpressionsinLambda));
                }
            } else {
                defs = false;
            }
        }
        Expr::list(exprs)
    }

    ///
    /// Quotations
    /// 
    /// 
    ///

    fn quotation(&mut self) -> ParseResult {
        let _keyword = self.keyword("quote")?;
        let datum = self.datum()?;


        Ok(Box::new(Expr::Literal(Literal::Quotation(datum))))
        // Expr::list(vec!(keyword, datum))
    }

    fn quotation_short(&mut self) -> ParseResult {
        // Implements '<datum> (i.e. single quote followed by datum)
        let _quote = self.quote()?;
        let datum = self.datum()?;

        Ok(Box::new(Expr::Literal(Literal::Quotation(datum))))
    }

    ///
    /// Assignments
    /// 
    /// 
    ///

    fn assignment(&mut self) -> ParseResult {
        let keyword = self.keyword("set!")?;
        
        let id = self.identifier()?;
        let expr = self.expr()?;

        Expr::list(vec!(keyword, id, expr))
    }

    ///
    /// Procedure calls
    ///
    ///
    /// 

    fn procedure_call(&mut self) -> ParseResult {
        let operator = self.expr()?;
        let operands = self.operands()?;

        Expr::list(vec!(operator, operands))
    }

    fn operands(&mut self) -> ParseResult {
        let operands = self.zero_or_more(Parser::expr)?;
        Expr::list(operands)
    }

    ///
    /// Macro blocks
    /// 
    /// 
    ///

    fn macro_block(&mut self) -> ParseResult {
        // we look for the keywords let-syntax or letrec-syntax
        match self.peek()? {
            token @ Token::Identifier(id) => 
                match id.as_str() {
                    "let-syntax" | "letrec-syntax"=> {
                        let keyword = self.identifier()?;
                        self.paren_left()?;
                        let syntax_specs = self.syntax_specs()?;
                        self.paren_right()?;
                        let body = self.body()?;
                        Expr::list(vec!(keyword, syntax_specs, body))
                    }
                     _ => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken { unexpected: token.to_string(), expected: "let-syntax or letrec-syntax" })),
                },
            token @ _ => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken { unexpected: token.to_string(), expected: "let-syntax or letrec-syntax" })),
        }
    }

    fn syntax_specs (&mut self) -> ParseResult {
        let syntax_specs = self.zero_or_more(Parser::syntax_spec)?;
        Expr::list(syntax_specs)
    }

    fn syntax_spec(&mut self) -> ParseResult {
        // ( <keyword> <transformer spec> )
        self.paren_left()?;
        let keyword = self.identifier()?;
        let transformer_spec = self.transformer_spec()?;
        self.paren_right()?;
        Expr::list(vec!(keyword, transformer_spec))
    }

    ///
    /// Transformer (R7RS section 7.1.5) 
    /// [INCOMPLETE]
    /// 
    
    fn transformer_spec(&mut self) -> ParseResult {
        self.paren_left()?;
        self.keyword("syntax-rules")?;

        let id = self.identifier();

        self.paren_left()?;
        let ids = self.transformer_spec_ids()?;
        self.paren_right()?;

        let syntax_rules = self.syntax_rules()?;

        self.paren_right()?;

        let children = match id {
            Ok(id) => vec!(id, ids, syntax_rules),
            Err(_) => vec!(ids, syntax_rules),
        };

        Expr::list(children)
    }

    fn transformer_spec_ids(&mut self) -> ParseResult {
        let ids = self.zero_or_more(Parser::identifier)?;
        Expr::list(ids)
    }

    fn syntax_rules(&mut self) -> ParseResult {
        let syntax_rules = self.zero_or_more(Parser::syntax_rule)?;
        Expr::list(syntax_rules)
    }

    fn syntax_rule(&mut self) -> ParseResult {
        // ( <pattern> <template> )
        self.paren_left()?;
        let pattern = self.pattern()?;
        let template = self.template()?;
        self.paren_right()?;
        Expr::list(vec!(pattern, template))
    }

    fn pattern(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Identifier(id) => {
                match id.as_str() {
                    "_" => self.pattern_underscore(),
                    _ => self.pattern_identifier(),
                }
            },
            Token::Boolean(_)
            | Token::Character(_)
            | Token::String(_)
            | Token::Number(_) => self.pattern_datum(),
            Token::ParenLeft => self.pattern_with_paren(),
            Token::SharpOpen => self.pattern_with_sharp_paren(),
            token @ _ => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken { unexpected: token.to_string(), expected: "identifier, literal or list" })),
        }
    }

    fn pattern_datum(&mut self) -> ParseResult {
        self.datum()
    }

    fn pattern_identifier(&mut self) -> ParseResult {
        let token = self.peek()?;

        match token {
            Token::Identifier(s) if s.as_str() == "..." => Err(SyntaxError::new(SyntaxErrorKind::EllipsisNotValidPatternIdentifier)),
            _ => self.identifier(),
        }
    }

    fn pattern_underscore(&mut self) -> ParseResult {
        self.keyword("_")
    }


    fn pattern_with_paren(&mut self) -> ParseResult {
        self.paren_left()?;
        let patterns = self.pattern_with_paren_a()?;
        self.paren_right()?;

        Ok(patterns)
    }


    fn pattern_with_paren_a(&mut self) -> ParseResult {
        // | <pattern>*
        // | <pattern>+ . <pattern>
        // | <pattern>* <pattern> <ellipsis> <pattern>*
        // | <pattern>* <pattern> <ellipsis> <pattern>* . <pattern>

        match self.peek()? {
            
            Token::ParenRight => Expr::list(vec!()), // empty

            _ => {

                // initially we have not seen an ellipse or any patterns before or after ellipse
                let mut pre_ellipse_patterns : Vec<Box<Expr>>;
                let mut post_ellipse_patterns: Vec<Box<Expr>> = vec!();
                let mut ellipse = false;

                // we look for patterns before ellipse
                pre_ellipse_patterns = self.zero_or_more(Parser::pattern)?;

                match self.peek()? {
                    Token::ParenRight => (), // no dot
                    Token::Dot => {
                        self.dot()?;
                        let pattern = self.pattern()?;
                        pre_ellipse_patterns.push(pattern);

                        match self.peek()? {
                            Token::ParenRight => (), // dot with no ellipse
                            Token::Identifier(id) if  id.as_str() == "..." =>  // dot with ellipse
                            {
                                ellipse = true;
                                let _ellipsis = self.identifier()?;
                                post_ellipse_patterns = self.zero_or_more(Parser::pattern)?;

                                if !matches!(self.peek()?, Token::ParenRight) { // dot with ellipse and dot 
                                    self.dot()?;
                                    let pattern = self.pattern()?;
                                    post_ellipse_patterns.push(pattern);
                                }
        
                            },
                            _ => ()
                        }
                    },
                    Token::Identifier(id) if  id.as_str() == "..." =>  
                    {
                        ellipse = true;
                        let _ellipsis = self.identifier()?;
                        post_ellipse_patterns = self.zero_or_more(Parser::pattern)?;

                        if !matches!(self.peek()?, Token::ParenRight) {
                            self.dot()?;
                            let pattern = self.pattern()?;
                            post_ellipse_patterns.push(pattern);
                        }
                    },
                    _ => ()
                }

                let mut vec = vec!(Expr::list(pre_ellipse_patterns)?);

                if ellipse {
                    vec.push(Expr::list(post_ellipse_patterns)?);
                }

                Expr::list(vec)

            }
        }
    }

    fn pattern_with_sharp_paren(&mut self) -> ParseResult {
        //  #( <pattern>* )
        // | #( <pattern>* <pattern> <ellipsis> <pattern>* )

        self.sharpopen()?;
        let patterns = self.pattern_with_sharp_paren_a()?;
        self.paren_right()?;

        Expr::list(vec!(patterns))
    }

    fn pattern_with_sharp_paren_a(&mut self) -> ParseResult {
        // initially we have not seen an ellipse or any patterns before or after ellipse
        let mut pre_ellipse_patterns : Vec<Box<Expr>> = vec!();
        let mut post_ellipse_patterns: Vec<Box<Expr>> = vec!();
        let mut ellipse = false;
                
        match self.peek()? {
            Token::ParenRight => (), // empty
            _ => {
                pre_ellipse_patterns = self.one_or_more(Parser::pattern)?;
                match self.peek()? {
                    Token::ParenRight => (),
                    Token::Identifier(id) if  id.as_str() == "..." => {
                        let _ellipsis = self.identifier()?;
                        ellipse = true;
                        post_ellipse_patterns = self.zero_or_more(Parser::pattern)?;

                    },
                    _ => ()
                }
            }
        }

        let mut vec = vec!(Expr::list(pre_ellipse_patterns)?);

        if ellipse {
            vec.push(Expr::list(post_ellipse_patterns)?);
        }

        Expr::list(vec)
    }

    fn template(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Identifier(_) => self.template_identifier(),
            Token::ParenLeft => self.template_with_paren(),
            Token::SharpOpen => self.template_with_sharp_paren(),
            Token::Boolean(_)
            | Token::Character(_)
            | Token::String(_)
            | Token::Number(_) => self.template_datum(),
            token @ _ => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken { unexpected: token.to_string(), expected: "identifier, literal or list" })),
        }
    }

    fn template_identifier(&mut self) -> ParseResult {
        self.pattern_identifier()
    }

    fn template_with_paren(&mut self) -> ParseResult {
        self.paren_left()?;

        let template = match self.peek()? {
            Token::ParenRight => Ok(vec!()), // empty list
            _ => {
                let mut elements = self.one_or_more(Parser::template_element)?;
                match self.peek()? {
                    Token::ParenRight => Ok(elements),
                    Token::Dot => {
                        self.dot()?;
                        elements.push(self.template_element()?);
                        Ok(elements)
                    },
                    token @ _ => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken { unexpected: token.to_string(), expected: "close parenthesis or dot" })),
                }
            }
        }?;

        self.paren_right()?;

        Expr::list(template)        
    }

    fn template_element(&mut self) -> ParseResult {
        let template = self.template()?;
        
        match self.peek()? {
            Token::Identifier(id) if id.as_str() == "..." => { 
                let ellipsis = Box::new(Expr::Identifier(Identifier::Keyword(Keyword::Ellipsis)));
                Expr::list(vec!(template, ellipsis))
            }
            _ => Expr::list(vec!(template))
        }
    }

    fn template_with_sharp_paren(&mut self) -> ParseResult {
        self.sharpopen()?;
        let elements = self.zero_or_more(Parser::template_element)?;
        self.paren_right()?;

        Expr::list(elements)
    }

    fn template_datum(&mut self) -> ParseResult {
        self.datum()
    }

    ///
    /// Datum (R7RS section 7.1.3 - External Representation)
    /// 
    /// 
    /// 
    fn datum(&mut self) -> ParseResult {
        self.simple_datum()
        .or_else(|_| self.abbreviation())
        .or_else(|_| self.datum_list())
        .or_else(|_| self.vector())
    }

    fn simple_datum(&mut self) -> ParseResult {
        self.boolean()
        .or_else(|_| self.number())
        .or_else(|_| self.character())
        .or_else(|_| self.string())
        .or_else(|_| self.symbol())
        .or_else(|_| self.bytevector())
    }

    fn symbol(&mut self) -> ParseResult {
        self.identifier()
    }

    fn abbreviation(&mut self) -> ParseResult {
        let prefix = self.quote()
        .or_else(|_| self.quasiquote())
        .or_else(|_| self.comma())
        .or_else(|_| self.comma_at())?;

        let expr = self.datum()?;

        Expr::list(vec!(prefix, expr))
    }

    fn datum_list(&mut self) -> ParseResult {
        self.paren_left()?;
        let data = self.datum_list_possible_dot()?;
        self.paren_right()?;

        Expr::list(data)
    }

    fn datum_list_possible_dot(&mut self) -> ParseVecResult {
        match self.peek()? {
            Token::ParenRight => Ok(vec!()), // empty list
            _ => {
                let mut data = self.one_or_more(Parser::datum)?;

                match self.peek()? {
                    Token::ParenRight => Ok(data),
                    Token::Dot => {
                        self.dot()?;
                        data.push(self.datum()?);
                        Ok(data)
                    },
                    token @ _ => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken { unexpected: token.to_string(), expected: "close parenthesis or dot" })),
                }
            },
        }
    }

    //
    // Includer
    //
    //
    //

    fn include(&mut self) -> ParseResult {
        println!("include");
        let keyword = self.keyword_box_leaf_from_next()?;
        println!("keyword: {:?}", keyword);
        let paths = self.paths()?;
        println!("paths: {:?}", paths);
        Expr::list(vec!(keyword, paths))
    }

    fn paths(&mut self) -> ParseResult {
        Expr::list(self.one_or_more(Parser::string)?)
    }

    //
    // Quasiquotation
    //
    //
    //

    fn quasiquotation(&mut self, depth: u32) -> ParseResult {
        let keyword = self.keyword("quasiquote")?;

        let template = self.qq_template(depth)?;

        Expr::list(vec!(keyword, template))

    }

    fn quasiquotation_short(&mut self, depth: u32) -> ParseResult {
        self.quasiquote()?;
        let keyword = Box::new(Expr::Identifier(Identifier::Keyword(Keyword::from("quasiquote".to_string()))));
        let template = self.qq_template(depth)?;
        Expr::list(vec!(keyword, template))
    }

    fn qq_template(&mut self, depth: u32) -> ParseResult {
        match depth {
            0 => self.expr(),
            _ => match self.peek()? {
                Token::Boolean(_)
                | Token::Character(_)
                | Token::String(_)
                | Token::Number(_)
                | Token::Identifier(_)
                | Token::SharpU8Open => self.simple_datum(),
                Token::SharpOpen => self.qq_template_vector(depth),
                Token::Comma => self.qq_template_unquotation_short(depth),
                Token::Quote => {
                    self.quote()?;
                    self.qq_template(depth)
                },
                Token::Quasiquote => self.quasiquotation_short(depth + 1),
                Token::ParenLeft => {
                    self.paren_left()?;

                    match self.peek()? {
                        Token::Identifier(id) if matches!(id.as_str(), "quasiquote") => self.quasiquotation(depth + 1),
                        Token::Identifier(id) if matches!(id.as_str(), "unquote") => self.qq_template_unquotation(depth),
                        _ => {
                            let list = self.qq_template_or_splice_list(depth)?;
                            self.paren_right()?;
                            Expr::list(list)
                        },
                    }
                },
                token @ _ => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken { unexpected: token.to_string(), expected: "identifier, literal or list" })),
            }    
        }
    }

    fn qq_template_or_splice(&mut self, depth: u32) -> ParseResult {
        match self.peek()? {
            Token::CommaAt => self.qq_splice_unquotation(depth),
            _ => self.qq_template(depth),
        }
    }

    fn qq_template_or_splice_list(&mut self, depth: u32) -> ParseVecResult {
        Ok(from_fn(|| self.qq_template_or_splice(depth).ok()).collect::<Vec<Box<Expr>>>())
    }

    fn qq_splice_unquotation(&mut self, depth: u32) -> ParseResult {
        self.comma_at()?;
        self.qq_template(depth - 1)
    }

    fn qq_template_unquotation_short(&mut self, depth: u32) -> ParseResult {
        let keyword = self.comma()?;
        let template = self.qq_template(depth - 1)?;

        Expr::list(vec!(keyword, template))
    }

    fn qq_template_unquotation(&mut self, depth: u32) -> ParseResult {
        let keyword = self.keyword("unquote")?;
        let template = self.qq_template(depth - 1)?;
        self.paren_right()?;

        Expr::list(vec!(keyword, template))
    }

    fn qq_template_vector(&mut self, depth: u32) -> ParseResult {
        let keyword = self.sharpopen()?;
        self.keyword("vector")?;
        let qq_templates = self.qq_templates(depth)?;
        self.paren_right()?;

        Expr::list(vec!(keyword, qq_templates))
    }

    fn qq_templates(&mut self, depth: u32) -> ParseResult {
        let templates = self.qq_template_list(depth)?;
        Expr::list(templates)
    }

    fn qq_template_list(&mut self, depth: u32) -> ParseVecResult {
        Ok(from_fn(|| self.qq_template(depth).ok()).collect::<Vec<Box<Expr>>>())
    }

    ///
    /// define-library
    /// 
    /// 
    ///

    fn define_library(&mut self) -> ParseResult {
        println!("define-library");
        let keyword = self.keyword("define-library")?;
        println!("keyword: {:?}", keyword);
        let name = self.library_name()?;
        println!("name: {:?}", name);
        let declarations = self.library_declarations()?;
        println!("declarations: {:?}", declarations);
        Expr::list(vec!(keyword, name, declarations))
    }

    fn library_declarations(&mut self) -> ParseResult {
        let declarations = self.zero_or_more(Parser::library_declaration)?;
        Expr::list(declarations)
    }

    fn library_name(&mut self) -> ParseResult {
        self.paren_left()?;
        let name = self.library_name_after_open()?;
        self.paren_right()?;
        Ok(name)
    }

    fn library_name_after_open(&mut self) -> ParseResult {
        Expr::list(self.one_or_more(Parser::library_name_part)?)
    }

    fn library_name_part(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Identifier(_) => self.identifier(),
            Token::Number(_) => self.uinteger10(),
            token @ _ => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken{unexpected: token.to_string(), expected: "identifier or uinteger10"})),
        }
    }

    fn library_declaration(&mut self) -> ParseResult {
        println!("library_declaration {:?}", self.peek()?);
        self.paren_left()?;
        println!("library_declaration paren_open {:?}", self.peek()?);
        let declaration = match self.peek()? {
            Token::Identifier(id) => 
                match id.as_str() {
                    "begin" => self.begin(),
                    "export" => self.export(),
                    "import" => self.import(),
                    "include" => self.include(),
                    "include-ci" => self.include(),
                    "include-library-declarations" => self.include_library_declarations(),
                    "cond-expand" => self.cond_expand(),
                    token @ _ => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken{unexpected: token.to_string(), expected: "export, import, include, include-ci, cond-expand"})),
                },
            token @ _ => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken{unexpected: token.to_string(), expected: "export, import, include, include-ci, cond-expand"})),
            
        }?;
        self.paren_right()?;
        Ok(declaration)
    }

    fn export(&mut self) -> ParseResult {
        println!("export {:?}", self.peek()?);
        let keyword = self.keyword("export")?;
        println!("export keyword {:?}", keyword);
        let specs = self.export_specs()?;

        Expr::list(vec!(keyword, specs))
    }

    fn export_specs(&mut self) -> ParseResult {
        let specs = self.zero_or_more(Parser::export_spec)?;

        Expr::list(specs)
    }

    fn export_spec(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Identifier(_) => self.export_spec_id(),
            Token::ParenLeft => self.export_spec_rename(),
            token @ _ => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken{unexpected: token.to_string(), expected: "identifier or export-spec-list"})),
        }
    }

    fn export_spec_id(&mut self) -> ParseResult {
        self.identifier()
    }

    fn export_spec_rename(&mut self) -> ParseResult {
        self.paren_left()?;
        self.keyword("rename")?;

        let id1 = self.identifier()?;
        let id2 = self.identifier()?;
        self.paren_right()?;

        Expr::list(vec!(id1, id2))
    }

    fn import(&mut self) -> ParseResult {
        let keyword= self.keyword("import")?;
        let sets = self.import_sets()?;

        Expr::list(vec!(keyword, sets))
    }

    fn import_sets(&mut self) -> ParseResult {
        let sets = self.zero_or_more(Parser::import_set)?;
        Expr::list(sets)
    }

    fn import_set(&mut self) -> ParseResult {
        self.paren_left()?;
        let import_set = match self.peek()? {
            Token::Identifier(id) => 
                match id.as_str() {
                    "only" => self.only()?,
                    "except" => self.except()?,
                    "prefix" => self.prefix()?,
                    "rename" => self.rename()?,
                    _ => self.library_name_after_open()?,
                }
            _ => self.library_name_after_open()?,
        };

        self.paren_right()?;

        Ok(import_set)
    }

    fn only(&mut self) -> ParseResult {
        let keyword = self.keyword("only")?;
        let set = self.import_set()?;
        let ids = self.import_set_ids()?;

        Expr::list(vec!(keyword, set, ids))
    }

    fn import_set_ids(&mut self) -> ParseResult {
        Expr::list(self.one_or_more(Parser::identifier)?)
    }


    fn except(&mut self) -> ParseResult {
        let keyword = self.keyword("except")?;
        let set = self.import_set()?;
        let ids = self.import_set_ids()?;

        Expr::list(vec!(keyword, set, ids))
    }

    fn prefix(&mut self) -> ParseResult {
        let keyword = self.keyword("prefix")?;
        let set = self.import_set()?;
        let id = self.identifier()?;

        Expr::list(vec!(keyword, set, id))
    }

    fn rename(&mut self) -> ParseResult {
        let keyword = self.keyword("rename")?;
        let set = self.import_set()?;

        let pairs = self.rename_pairs()?;

        Expr::list(vec!(keyword, set, pairs))
    }

    fn rename_pairs(&mut self) -> ParseResult {
        Expr::list(self.one_or_more(Parser::identifier_pair)?)
    }


    fn cond_expand(&mut self) -> ParseResult {
        let keyword = self.keyword("cond-expand")?;

        let clauses = self.cond_expand_clauses()?;

        Expr::list(vec!(keyword, clauses))
    }

    fn cond_expand_clauses(&mut self) -> ParseResult {
        Expr::list(self.one_or_more(Parser::cond_clause)?)
    }

    fn cond_clause(&mut self) -> ParseResult {
        self.paren_left()?;
        let requirement = self.feature_requirement()?;
        let declarations = self.library_declarations()?;
        self.paren_right()?;

        let mut vec = vec!(requirement, declarations);

        if matches!(self.peek()?, Token::ParenLeft) {
            let else_clause = self.cond_expand_else()?;
            vec.push(else_clause);
        }

        Expr::list(vec)
    }

    fn cond_expand_else(&mut self) -> ParseResult {
        self.paren_left()?;
        let keyword = self.keyword("else")?;
        let declarations = self.library_declarations()?;
        self.paren_right()?;

        Expr::list(vec!(keyword, declarations))
    }

    fn feature_requirements(&mut self) -> ParseResult {
        let requirements = self.zero_or_more(Parser::feature_requirement)?;
        Expr::list(requirements)
    }

    fn feature_requirement(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Identifier(_) => self.identifier(),
            Token::ParenLeft => self.feature_requirement_with_paren(),
            token @ _ => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken{unexpected: token.to_string(), expected: "identifier or open parenthesis"})),
        }
    }

    fn feature_requirement_with_paren(&mut self) -> ParseResult {
        self.paren_left()?;

        let requirement = match self.peek()? {
            Token::Identifier(id) => 
                match id.as_str() {
                    "and" => self.and(),
                    "or" => self.or(),
                    "not" => self.not(),
                    _ => self.library_name_after_open(),
                }
            _ => self.library_name_after_open(),
        };

        self.paren_right()?;

        requirement
    }

    fn and(&mut self) -> ParseResult {
        let keyword = self.keyword("and")?;
        let requirements = self.feature_requirements()?; 
        Expr::list(vec!(keyword, requirements))
    }

    fn or(&mut self) -> ParseResult {
        let keyword = self.keyword("or")?;
        let requirements = self.feature_requirements()?;
        Expr::list(vec!(keyword, requirements))
    }

    fn not(&mut self) -> ParseResult {
        let keyword = self.keyword("not")?;
        let requirement = self.feature_requirement()?;

        Expr::list(vec!(keyword, requirement))
    }

    fn include_library_declarations(&mut self) -> ParseResult {
        let keyword = self.keyword("include-library-declarations")?;
        let strings = self.include_library_declaration_strings()?;

        Expr::list(vec!(keyword, strings))
    }

    fn include_library_declaration_strings(&mut self) -> ParseResult {
        Expr::list(self.one_or_more(Parser::string)?)
    }

    //
    // Helpers
    //
    //
    //

    fn peek(&mut self) -> Result<&Token, SyntaxError> {
        self.lexer.peek().ok_or(SyntaxError::new(SyntaxErrorKind::EndOfInput))
    }
    
    fn zero_or_more(&mut self, closure: fn(&mut Self) -> ParseResult) -> ParseVecResult {
        Ok(from_fn(|| closure(self).ok()).collect())
    }

    fn one_or_more(&mut self, closure: fn(&mut Self) -> ParseResult) -> ParseVecResult {
        Ok(once(closure(self)?).chain(from_fn(|| closure(self).ok())).collect())
    }

    fn keyword(&mut self, keyword: &str) -> ParseResult {
        match self.peek()? {
            Token::Identifier(s) if keyword == s => self.keyword_box_leaf_from_next(),
            token @ _ => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken { unexpected: token.to_string() , expected: "a  keyword" })),
        }
    }

    fn keyword_box_leaf_from_next(&mut self) -> ParseResult {

        match self.lexer.next() {
            Some(Token::Identifier(s)) => Ok(Box::new(Expr::Identifier(Identifier::Keyword(Keyword::from(s))))),
            token @ _ => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken { unexpected: token.unwrap().to_string() , expected: "a  keyword" })),
        }
    }
    
    fn punctuation(&mut self, expected: Token, s: &'static str) -> ParseResult {
        match self.peek()? {
            t if t == &expected => {
                self.lexer.next();
                Ok(Box::new(Expr::Identifier(Identifier::Variable(s.to_string()))))
            },
            token @ _ => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken { unexpected: token.to_string() , expected: s })),
        }   
    }

    fn comma(&mut self) -> ParseResult {
        self.punctuation(Token::Comma, ",")
    }

    fn comma_at(&mut self) -> ParseResult {
        self.punctuation(Token::CommaAt, ",@")
    }

    fn dot(&mut self) -> ParseResult {
        self.punctuation(Token::Dot, ".")
    }

    fn paren_right(&mut self) -> ParseResult {
        self.punctuation(Token::ParenRight, ")")
    }
    
    fn paren_left(&mut self) -> ParseResult {
        self.punctuation(Token::ParenLeft, "(")
    }

    fn quasiquote(&mut self) -> ParseResult {
        self.punctuation(Token::Quasiquote, "`")
    }

    fn quote(&mut self) -> ParseResult {
        self.punctuation(Token::Quote, "'")
    }

    fn sharpopen(&mut self) -> ParseResult {
        self.punctuation(Token::SharpOpen, "#(")
    }

    fn sharpu8open(&mut self) -> ParseResult {
        self.punctuation(Token::SharpU8Open, "#u8(")
    }

    fn identifier(&mut self) -> ParseResult {        
        match self.peek()? {
            Token::Identifier(_) => Ok(Box::new(Expr::Identifier(Identifier::from(&self.lexer.next().unwrap())))),
            token @ _ => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken { unexpected: token.to_string(), expected: "identifier" })),
        }        
    }
    
    fn identifier_pair(&mut self) -> ParseResult {
        self.paren_left()?;
        let id1 = self.identifier()?;
        let id2 = self.identifier()?;

        self.paren_right()?;

        Expr::list(vec!(id1, id2))
    }

    fn identifier_list_possible_dot(&mut self) -> ParseResult {
        let mut ids = vec!();
        
        if !matches!(self.peek()?, Token::ParenRight) {
            ids = self.one_or_more(Parser::identifier)?;
            if matches!(self.peek()?, Token::Dot) {
                self.dot()?;
                ids.push(self.identifier()?);
            }
        }

        Expr::list(ids)
    }
    

    fn uinteger10(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Number(n) if n.chars().all(|c| c.is_digit(10)) => self.number(),
            token @ _ => Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken { unexpected: token.to_string(), expected: "uninteger10" })),
        }
    }

    fn vector(&mut self) -> ParseResult {
        self.sharpopen()?;
        let data = self.zero_or_more(Parser::expr)?;
        self.paren_right()?;
        Ok(Box::new(Expr::Literal(expr::Literal::Vector(data))))
    }

    fn bytevector(&mut self) -> ParseResult {
        self.sharpu8open()?;
        let data = self.zero_or_more(Parser::number)?;
        self.paren_right()?;

        if data.iter().all(|e| match &**e { Expr::Literal(Literal::Number(n)) => n.parse::<u8>().is_ok(), _ => false, }) {
                Ok(Box::new(Expr::Literal(Literal::Bytevector(data))))
        } else {
            Err(SyntaxError::new(SyntaxErrorKind::UnexpectedToken { unexpected: Token::Error.to_string(), expected: "bytevector with bytes" }))
        }
    }

}
