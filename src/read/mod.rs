//! Athir parser module
//! Implements recursive descent parser for R7RS Scheme
//! 
//! TODO:
//! 
//! [P0]
//! - test define-library including begin
//! - test begin
//! 
//! [P1]
//! - Review code for readability
//! - Review code for performance
//! 
//! [P2[]
//! - clean-up: iterator instead of Vec if possible
//! - clean-up: parenthesized! macro
//! - clean-up: code for optional
//! - clean-up: error reporting
//! - Node captures source code
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
//!             Err(err) => println!("{}", err),
//!         }
//!     }
//! ```
//!
//! Deviations from R7RS:
//! - string inline hex escapes not implemented
//! - no support for # label in datum.
//! - derived expressions not implemented
//!

mod lexer;
mod node;

#[cfg(test)]
mod tests;

use std::iter::{once, from_fn};

pub use node::{NodeKind, Node};
pub use lexer::{Token, Source};
pub use crate::error::{Error, ErrorKind};

use lexer::Lexer;

type ParseResult = Result<Box<Node>, Error>;
type ParseVecResult = Result<Vec<Box<Node>>, Error>;
/// 
/// Parser 
/// 
/// Uses a peekable Lexer to get tokens and then parses them into an AST
/// (see mod crate::read::lexer)

pub struct Parser <T: Iterator<Item=String>> {
    lexer: Lexer<T>,
}

impl<T> Iterator for Parser<T> where T: Iterator<Item = String> {
    type Item = Result<Box<Node>, Error>;

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
        match self.peek()? {
            Token::ParenOpen => self.compound_expr(),
            _ => self.atom(),
        }
    }

    fn atom(&mut self) -> ParseResult {  
        match self.peek()? {
            Token::Boolean(_) => self.leaf(NodeKind::Literal),
            Token::Character(_) => self.leaf(NodeKind::Literal),
            Token::Identifier(_) => self.leaf(NodeKind::Identifier),
            Token::Number(_) => self.leaf(NodeKind::Literal),
            Token::Quasiquote => self.quasiquotation_short(1),
            Token::Quote => self.quotation_short(),
            Token::SharpOpen => self.vector(),
            Token::SharpU8Open => self.bytevector(),
            Token::String(_)=> self.leaf(NodeKind::Literal),
            t @ _ => Err(Error{ kind: ErrorKind::UnexpectedToken{unexpected: t.clone(), expected: "atom"} }) ,
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

        self.paren_open()?;

        let expr = match self.peek()? {
            token @ Token::Identifier(id) => 
                match id.as_str() {
                    "begin" => self.begin(),
                    "define" => self.define(),
                    "define-values" => self.define_values(),
                    "define-record-type" => self.define_record_type(),
                    "define-syntax" => self.define_syntax(),
                    "define-library" => self.define_library(),
                    "if" => self.conditional(),
                    "include" => self.include(),
                    "include-ci" => self.include(),
                    "lambda" => self.lambda(),
                    "let-syntax" | "letrec-syntax" => self.macro_block(),
                    "quasiquote" => self.quasiquotation(1),
                    "quote" => self.quotation(),
                    "set!" => self.assignment(),
                    "unquote" => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: token.clone() , expected: "operator or other keyword" })),
                    _ => self.procedure_call(),
                },
            _ => self.procedure_call(),
        };

        self.paren_close()?;

        expr
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

        let _begin = self.keyword("begin")?;

        let nodes = self.zero_or_more(Parser::expr)?;

        if nodes.iter().all(|node| node.is_definition_expr()) {
            return self.node(NodeKind::Begin(true), nodes)
        } else {
            self.node(NodeKind::Begin(false), nodes)
        }
    }

    ///
    /// Definitions
    /// 
    /// We start with the define keyword and then look for either 
    /// - an identifier (i.e. a variable definition) or,
    /// - a parenthesized list of identifiers (i.e. a function definition)
    /// 

    fn define(&mut self) -> ParseResult {
        self.keyword("define")?;
        match self.peek()? {
            Token::Identifier(_) => self.variable_definition(),
            Token::ParenOpen => self.function_definition(),
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken{unexpected: t.clone(), expected: "identifier or open paren"})),
        }
    }
        
    fn variable_definition(&mut self) -> ParseResult {
        let id = self.identifier()?;
        let expr = self.expr()?;
        self.node(NodeKind::Define, vec![id, expr])
    }
        
    fn function_definition(&mut self) -> ParseResult {
        self.paren_open()?;
        let id = self.identifier()?;
        let formals = self.def_formals()?;
        self.paren_close()?;
        let body = self.body()?;
        self.node(NodeKind::DefineFunction, vec![id, formals, body])
    } 

    fn def_formals(&mut self) -> ParseResult {
        self.identifier_list_possible_dot()
    }

    fn define_values(&mut self) -> ParseResult {
        let _define_values = self.keyword("define-values")?;
        let formals = self.formals()?;
        let exprs = self.body()?;
        self.node(NodeKind::DefineValues, vec![formals, exprs])
    }

    fn define_record_type(&mut self) -> ParseResult {
        let _define_record_type = self.keyword("define-record-type")?;
        let id1 = self.identifier()?;
        let constructor = self.constructor()?;
        let id2 = self.identifier()?;
        let field_specs = self.zero_or_more(Parser::field_spec)?;
        self.node(
            NodeKind::DefineRecordType,
            once(id1).chain(once(constructor)).chain(once(id2)).chain(field_specs).collect()
        )
    }

    fn constructor(&mut self) -> ParseResult {
        self.paren_open()?;
        let id = self.identifier()?;
        let field_names = self.zero_or_more(Parser::identifier)?;
        self.paren_close()?;

        self.node(NodeKind::List, once(id).chain(field_names).collect())
    }

    fn field_spec(&mut self) -> ParseResult {
        self.paren_open()?;

        let field_name = self.identifier()?;
        let accessor = self.identifier()?;
        let node = match self.peek()? {
            Token::ParenClose => {
                self.node(NodeKind::List, vec![field_name, accessor])
            },
            Token::Identifier(_) => {
                let mutator = self.identifier()?;
                self.node(NodeKind::List, vec![field_name, accessor, mutator])
            }
            _ => Err(Error::new(ErrorKind::UnexpectedToken{unexpected: self.peek()?.clone(), expected: "identifier or close parenthesis"})),
        }?;

        self.paren_close()?;

        Ok(node)

    }

    fn define_syntax(&mut self) -> ParseResult {
        let _define_syntax = self.keyword("define-syntax")?;
        let id = self.identifier()?;
        let expr = self.transformer_spec()?;

        self.node(NodeKind::DefineSyntax, vec![id, expr])        
    }

    ///
    /// Conditionals
    /// 
    /// 
    ///
    fn conditional(&mut self) -> ParseResult {
        let _if = self.keyword("if")?;
        let test = self.expr()?;
        let consequent = self.expr()?;

        match self.peek()? {
            Token::ParenClose => self.node(NodeKind::Conditional, vec![test, consequent]),   
            _ => {
                let alternative = self.expr()?;
                match self.peek()? {
                    Token::ParenClose => self.node(NodeKind::Conditional, vec![test, consequent, alternative]),
                    _ => Err(Error::new(ErrorKind::UnexpectedToken{unexpected: self.peek()?.clone(), expected: "close parenthesis"})),
                }
            },
        }
    }

    ///
    /// Lambda
    /// 
    /// 
    ///

    fn lambda(&mut self) -> ParseResult {
        let _lambda = self.keyword("lambda")?;
        let formals = self.formals()?;
        let body = self.body()?;
        self.node(NodeKind::Lambda, vec![formals, body])
    }

    fn formals(&mut self) -> ParseResult {   
        match self.peek()? {
            Token::Identifier(_) => self.identifier(),
            Token::ParenOpen => {
                self.paren_open()?;
                let ids = self.identifier_list_possible_dot()?;
                self.paren_close()?;
                Ok(ids)
            }
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken{unexpected: t.clone(), expected: "identifier or open parenthesis"})),
        }
    }

    /// body should not be empty
    /// body can have defintions and expressions
    /// body cannot have definitions after any expression
    
    fn body(&mut self) -> ParseResult {        
        match self.peek()? {
            Token::ParenClose => Err(Error::new(ErrorKind::EmptyBodyinLambda)),
            _ => {
                let exprs = self.zero_or_more(Parser::expr)?;
                let mut defs = true;

                for expr in exprs.iter() {
                    if expr.is_definition_expr() {
                        if defs == false {
                            return Err(Error::new(ErrorKind::DefinitionsBeforeExpressionsinLambda));
                        }
                    } else {
                        defs = false;
                    }
                }
                self.node(NodeKind::List, exprs)
            }
        }
    }

    ///
    /// Quotations
    /// 
    /// 
    ///

    fn quotation(&mut self) -> ParseResult {
        let _quote = self.keyword("quote")?;
        let datum = self.datum()?;
        self.node(NodeKind::Quotation, vec![datum])
    }

    fn quotation_short(&mut self) -> ParseResult {
        // Implements '<datum> (i.e. single quote followed by datum)
        let _quote = self.quote()?;
        let datum = self.datum()?;
        self.node(NodeKind::Quotation, vec![datum])
    }

    ///
    /// Assignments
    /// 
    /// 
    ///

    fn assignment(&mut self) -> ParseResult {
        self.keyword("set!")?;
        
        let id = self.identifier()?;
        let expr = self.expr()?;

        self.node(NodeKind::Assignment, vec![id, expr])
    }

    ///
    /// Procedure calls
    ///
    ///
    /// 

    fn procedure_call(&mut self) -> ParseResult {
        let operator = self.expr()?;
        let operands = self.zero_or_more(Parser::expr)?;
        self.node(NodeKind::ProcedureCall, once(operator).chain(operands).collect())
    }

    ///
    /// Macro blocks
    /// 
    /// 
    ///

    fn macro_block(&mut self) -> ParseResult {
        // we look for the keywords let-syntax or letrec-syntax
        match self.peek()? {
            t @ Token::Identifier(id) => 
                match id.as_str() {
                    "let-syntax" | "letrec-syntax"=> {
                        let keyword = self.identifier()?;
                        self.paren_open()?;
                        let syntax_specs = self.syntax_specs()?;
                        self.paren_close()?;
                        let body = self.body()?;
                        Ok(self.node(NodeKind::MacroBlock, once(keyword).chain(syntax_specs).chain(once(body)).collect()))
                    }
                     _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "let-syntax or letrec-syntax" })),
                },
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "let-syntax or letrec-syntax" })),
        }?
    }

    fn syntax_specs(&mut self) -> ParseVecResult {
        // <syntax spec>*
        Ok(from_fn(|| self.syntax_spec().ok()).collect::<Vec<Box<Node>>>())
    }

    fn syntax_spec(&mut self) -> ParseResult {
        // ( <keyword> <transformer spec> )
        self.paren_open()?;
        let keyword = self.identifier()?;
        let transformer_spec = self.transformer_spec()?;
        self.paren_close()?;
        self.node(NodeKind::List, vec![keyword, transformer_spec])
    }

    ///
    /// Transformer (R7RS section 7.1.5) 
    /// [INCOMPLETE]
    /// 
    
    fn transformer_spec(&mut self) -> ParseResult {
        self.paren_open()?;
        self.keyword("syntax-rules")?;

        let id = self.identifier();

        self.paren_open()?;

        let ids = self.zero_or_more(Parser::identifier)?;
        let ids = self.node(NodeKind::List, ids)?;

        self.paren_close()?;
        let syntax_rules = self.zero_or_more(Parser::syntax_rule)?;
        let syntax_rules = self.node(NodeKind::List, syntax_rules)?;

        self.paren_close()?;

        let children = match id {
            Ok(id) => vec!(id, ids, syntax_rules),
            Err(_) => vec!(ids, syntax_rules),
        };

        self.node(NodeKind::List, children)
    }

    fn syntax_rule(&mut self) -> ParseResult {
        // ( <pattern> <template> )
        self.paren_open()?;
        let pattern = self.pattern()?;
        let template = self.template()?;
        self.paren_close()?;
        self.node(NodeKind::List, vec![pattern, template])
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
            Token::ParenOpen => self.pattern_with_paren(),
            Token::SharpOpen => self.pattern_with_sharp_paren(),
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "identifier, literal or list" })),
        }
    }

    fn pattern_datum(&mut self) -> ParseResult {
        self.leaf(NodeKind::Datum)
    }

    fn pattern_identifier(&mut self) -> ParseResult {
        let token = self.peek()?;

        match token {
            Token::Identifier(s) if s.as_str() == "..." => Err(Error::new(ErrorKind::EllipsisNotValidPatternIdentifier)),
            _ => self.identifier(),
        }
    }

    fn pattern_underscore(&mut self) -> ParseResult {
        self.keyword("_")
    }


    fn pattern_with_paren(&mut self) -> ParseResult {
        self.paren_open()?;
        let patterns = self.pattern_with_paren_a()?;
        self.paren_close()?;

        Ok(patterns)
    }


    fn pattern_with_paren_a(&mut self) -> ParseResult {
        // | <pattern>*
        // | <pattern>+ . <pattern>
        // | <pattern>* <pattern> <ellipsis> <pattern>*
        // | <pattern>* <pattern> <ellipsis> <pattern>* . <pattern>

        match self.peek()? {
            
            Token::ParenClose => self.node(NodeKind::List, vec!()), // empty

            _ => {
                let mut pre_ellipse_patterns = self.pattern_pre_ellipse()?;

                match self.peek()? {
                    // <pattern>*            
                    Token::ParenClose => {
                        let patterns = self.node(NodeKind::List, pre_ellipse_patterns)?;
                        self.node(NodeKind::List, vec!(patterns))
                    },
                    // | <pattern>+ . <pattern>
                    Token::Dot => self.dot().and_then(|_|
                        self.pattern().and_then(|pattern| {
                            pre_ellipse_patterns.push(pattern);
                            let patterns = self.node(NodeKind::List, pre_ellipse_patterns)?;
                            self.node(NodeKind::List, vec!(patterns))        
                        })
                    ),
                    Token::Identifier(id) if  id.as_str() == "..." => 
                        self.identifier().and_then(|_ellipsis| {
                            let mut post_ellipse_patterns = self.pattern_post_ellipse()?;
                            match self.peek()? {
                                // | <pattern>* <pattern> <ellipsis> <pattern>*
                                Token::ParenClose => {
                                    let pre_patterns = self.node(NodeKind::List, pre_ellipse_patterns)?;
                                    let post_patterns = self.node(NodeKind::List, post_ellipse_patterns)?;
                                    self.node(NodeKind::List, vec!(pre_patterns, post_patterns))
                                },
                                // | <pattern>* <pattern> <ellipsis> <pattern>* . pattern
                                Token::Dot => 
                                    self.dot().and_then(|_|
                                        self.pattern().and_then(|pattern| {
                                            post_ellipse_patterns.push(pattern);
                                            let pre_patterns = self.node(NodeKind::List, pre_ellipse_patterns)?;
                                            let post_patterns = self.node(NodeKind::List, post_ellipse_patterns)?;
                                            self.node(NodeKind::List, vec!(pre_patterns, post_patterns))
                                        })
                                    ),
                                t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "close parenthesis or dot" })),
                            }
                        }),
                    t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone() , expected: "close parenthesis, dot or ellipsis" })),
                }
            }
        }
    }

    fn pattern_pre_ellipse(&mut self) -> ParseVecResult {
        // <pattern>*
        Ok(from_fn(|| self.pattern().ok()).collect())
    }

    fn pattern_post_ellipse(&mut self) -> ParseVecResult {
        // <pattern>*
        Ok(from_fn(|| self.pattern().ok()).collect())
    }
    
    fn pattern_with_sharp_paren(&mut self) -> ParseResult {
        //  #( <pattern>* )
        // | #( <pattern>* <pattern> <ellipsis> <pattern>* )

        self.sharpopen()?;
        let patterns = self.pattern_with_sharp_paren_a()?;
        self.paren_close()?;
        self.node(NodeKind::List, vec![patterns])

    }

    fn pattern_with_sharp_paren_a(&mut self) -> ParseResult {
        match self.peek()? {
            Token::ParenClose => self.node(NodeKind::List, vec!()),
            _ => {
                let pre_ellipse_patterns = self.pattern_pre_ellipse()?;
                match self.peek()? {
                    Token::ParenClose => {
                        let patterns = self.node(NodeKind::List, pre_ellipse_patterns)?;
                        self.node(NodeKind::List, vec!(patterns))
                    },
                    Token::Identifier(id) if  id.as_str() == "..." =>
                        self.identifier().and_then(|_ellipsis| {
                            let post_ellipse_patterns = self.pattern_post_ellipse()?;
                            match self.peek()? {
                                Token::ParenClose => {
                                    let pre_patterns = self.node(NodeKind::List, pre_ellipse_patterns)?;
                                    let post_patterns = self.node(NodeKind::List, post_ellipse_patterns)?;
                                    self.node(NodeKind::List, vec!(pre_patterns, post_patterns))
                                },
                                t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "close parenthesis" })),
                            }
                    }),
                    t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "close parenthesis or ellipsis" })),
                }
            }
        }
    }

    fn template(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Identifier(_) => self.template_identifier(),
            Token::ParenOpen => self.template_with_paren(),
            Token::SharpOpen => self.template_with_sharp_paren(),
            Token::Boolean(_)
            | Token::Character(_)
            | Token::String(_)
            | Token::Number(_) => self.template_datum(),
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "identifier, literal or list" })),
        }
    }

    fn template_identifier(&mut self) -> ParseResult {
        self.pattern_identifier()
    }

    fn template_with_paren(&mut self) -> ParseResult {
        self.paren_open()?;
        let template = self.template_with_paren_a()?;
        self.paren_close()?;

        self.node(NodeKind::List, template)
    }

    fn template_with_paren_a(&mut self) -> ParseVecResult {
        match self.peek()? {
            Token::ParenClose => Ok(vec!()), // empty list
            _ => self.one_or_more(Parser::template_element).and_then(|elements| // get rest of template_element sequence
                    match self.peek()? {
                        Token::ParenClose => Ok(elements),
                        Token::Dot => self.dot().and_then(|_|
                                        self.template_element().and_then(|last|
                                            Ok(elements
                                                .into_iter()
                                                .chain(once(last))
                                                .collect()))),
                        t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "close parenthesis or dot" })),
                    }
                ),
        }   
    }

    fn template_element(&mut self) -> ParseResult {
        let template = self.template()?;
        
        match self.peek()? {
            Token::Identifier(id) if id.as_str() == "..." => { 
                let ellipsis = self.leaf(NodeKind::Identifier)?;
                self.node(NodeKind::List, vec![ellipsis, template])
            }
            _ => self.node(NodeKind::List, vec![template])
        }
    }

    fn template_with_sharp_paren(&mut self) -> ParseResult {
        self.sharpopen()?;
        let elements = self.zero_or_more(Parser::template_element)?;
        self.paren_close()?;
        self.node(NodeKind::List, elements)
    }

    fn template_datum(&mut self) -> ParseResult {
        self.leaf(NodeKind::Datum)
    }

    ///
    /// Datum (R7RS section 7.1.3 - External Representation)
    /// 
    /// 
    /// 
    fn datum(&mut self) -> ParseResult {
        self.simple_datum()
        .or_else(|_| self.abbreviation())
        .or_else(|_| self.list())
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
        match self.peek()? {
            Token::Identifier(_) => self.leaf(NodeKind::Symbol),
            t @ _ => return Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "symbol" })),
        }
    }

    fn abbreviation(&mut self) -> ParseResult {
        let prefix = self.quote()
        .or_else(|_| self.quasiquote())
        .or_else(|_| self.comma())
        .or_else(|_| self.comma_at())?;

        let expr = self.datum()?;

        self.node(NodeKind::List, vec!(prefix, expr))
    }

    fn list(&mut self) -> ParseResult {
        self.paren_open()?;
        let data = self.datum_list_possible_dot()?;
        self.paren_close()?;
        self.node(NodeKind::List, data)
    }

    fn datum_list_possible_dot(&mut self) -> ParseVecResult {
        match self.peek()? {
            Token::ParenClose => Ok(vec!()), // empty list
            _ => {
                let data = self.one_or_more(Parser::datum)?;

                match self.peek()? {
                    Token::ParenClose => Ok(data),
                    Token::Dot => {
                        self.dot()?;
                        let last = self.datum()?;
                        Ok(data.into_iter().chain(once(last)).collect())
                    },
                    t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "close parenthesis or dot" })),
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
        let keyword = self.leaf(NodeKind::Identifier)?;

        let paths = self.one_or_more(Parser::string)?;

        self.node(
            NodeKind::Includer,
            once(keyword)
            .chain(paths.into_iter())
            .collect()
        )
    }

    //
    // Quasiquotation
    //
    //
    //

    fn quasiquotation(&mut self, depth: u32) -> ParseResult {
        self.keyword("quasiquote")?;

        let template = self.qq_template(depth)?;
        self.node(NodeKind::Quasiquotation(depth), vec![template])
    }

    fn quasiquotation_short(&mut self, depth: u32) -> ParseResult {
        self.quasiquote()?;
        let template = self.qq_template(depth)?;
        self.node(NodeKind::Quasiquotation(depth), vec!(template))
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
                Token::ParenOpen => {
                    self.paren_open()?;

                    match self.peek()? {
                        Token::Identifier(id) if matches!(id.as_str(), "quasiquote") => self.quasiquotation(depth + 1),
                        Token::Identifier(id) if matches!(id.as_str(), "unquote") => self.qq_template_unquotation(depth),
                        _ => {
                            let list = self.qq_template_or_splice_list(depth)?;
                            self.paren_close()?;
                            self.node(NodeKind::List, list)
                        },
                    }
                },
                t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "identifier, literal or list" })),
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
        Ok(from_fn(|| self.qq_template_or_splice(depth).ok()).collect::<Vec<Box<Node>>>())
    }

    fn qq_splice_unquotation(&mut self, depth: u32) -> ParseResult {
        self.comma_at()?;
        self.qq_template(depth - 1)
    }

    fn qq_template_unquotation_short(&mut self, depth: u32) -> ParseResult {
        self.comma()?;
        let template = self.qq_template(depth - 1)?;
        self.node(NodeKind::Unquotation(depth-1), vec!(template))
    }

    fn qq_template_unquotation(&mut self, depth: u32) -> ParseResult {
        self.keyword("unquote")?;
        let template = self.qq_template(depth - 1)?;
        self.paren_close()?;
        self.node(NodeKind::Unquotation(depth-1), vec!(template))
    }

    fn qq_template_vector(&mut self, depth: u32) -> ParseResult {
        self.sharpopen()?;
        self.keyword("vector")?;
        let list = self.qq_template_list(depth)?;
        self.paren_close()?;
        self.node(NodeKind::Vector, list)
    }

    fn qq_template_list(&mut self, depth: u32) -> ParseVecResult {
        Ok(from_fn(|| self.qq_template(depth).ok()).collect::<Vec<Box<Node>>>())
    }

    ///
    /// define-library
    /// 
    /// 
    ///

    fn define_library(&mut self) -> ParseResult {
        self.keyword("define-library")?;

        let name = self.library_name()?;
        let body = self.zero_or_more(Parser::library_declaration)?;
        self.node(NodeKind::DefineLibrary, once(name).chain(body).collect())
    }

    fn library_name(&mut self) -> ParseResult {
        self.paren_open()?;
        self.library_name_after_open()
    }

    fn library_name_after_open(&mut self) -> ParseResult {
        let parts = self.one_or_more(Parser::library_name_part)?;
        self.paren_close()?;
        self.node(NodeKind::List, parts)
    }

    fn library_name_part(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Identifier(_) => self.identifier(),
            Token::Number(_) => self.uinteger10(),
            _ => Err(Error::new(ErrorKind::UnexpectedToken{unexpected: self.peek()?.clone(), expected: "identifier or uinteger10"})),
        }
    }

    fn library_declaration(&mut self) -> ParseResult {
        self.paren_open()?;
        let declaration = self.library_declaration_inner()?;
        self.paren_close()?;
        Ok(declaration)
    }

    fn library_declaration_inner(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Identifier(id) => 
                match id.as_str() {
                    "begin" => self.begin(),
                    "export" => self.export(),
                    "import" => self.import(),
                    "include" => self.include(),
                    "include-ci" => self.include(),
                    "include-library-declarations" => self.include_library_declarations(),
                    "cond-expand" => self.cond_expand(),
                    _ => Err(Error::new(ErrorKind::UnexpectedToken{unexpected: self.peek()?.clone(), expected: "export, import, include, include-ci, cond-expand"})),
                },
            _ => Err(Error::new(ErrorKind::UnexpectedToken{unexpected: self.peek()?.clone(), expected: "export, import, include, include-ci, cond-expand"})),
            
        }
    }

    fn export(&mut self) -> ParseResult {
        let export = self.keyword("export")?;
        let specs = self.zero_or_more(Parser::export_spec)?;
        self.node(NodeKind::List, once(export).chain(specs.into_iter()).collect())
    }

    fn export_spec(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Identifier(_) => self.export_spec_id(),
            Token::ParenOpen => self.export_spec_rename(),
            _ => Err(Error::new(ErrorKind::UnexpectedToken{unexpected: self.peek()?.clone(), expected: "identifier or export-spec-list"})),
        }
    }

    fn export_spec_id(&mut self) -> ParseResult {
        let id = self.identifier()?;
        self.node(NodeKind::List, vec![id])
    }

    fn export_spec_rename(&mut self) -> ParseResult {
        self.paren_open()?;
        self.keyword("rename")?;

        let id1 = self.identifier()?;
        let id2 = self.identifier()?;
        self.paren_close()?;
        self.node(NodeKind::List, vec![id1, id2])
    }

    fn import(&mut self) -> ParseResult {
        let import = self.keyword("import")?;
        let sets = self.one_or_more(Parser::import_set)?;
        self.node(
            NodeKind::List, 
            once(import)
            .chain(sets).collect()
        )
    }

    fn import_set(&mut self) -> ParseResult {
        self.paren_open()?;
        let import_set = match self.peek()? {
            Token::Identifier(id) => 
                match id.as_str() {
                    "only" => self.only(),
                    "except" => self.except(),
                    "prefix" => self.prefix(),
                    "rename" => self.rename(),
                    _ => self.library_name_after_open(),
                }
            _ => self.library_name_after_open(),
        };

        self.paren_close()?;

        import_set
    }

    fn only(&mut self) -> ParseResult {
        let only = self.keyword("only")?;
        let set = self.import_set()?;
        let ids = self.one_or_more(Parser::identifier)?;

        self.node(
            NodeKind::List,
            once(only)
            .chain(once(set))
            .chain(ids)
            .collect()
        )
    }

    fn except(&mut self) -> ParseResult {
        let except = self.keyword("except")?;
        let set = self.import_set()?;
        let ids = self.one_or_more(Parser::identifier)?;
        self.node(
            NodeKind::List,
            once(except)
            .chain(once(set))
            .chain(ids)
            .collect()
        )
    }

    fn prefix(&mut self) -> ParseResult {
        let prefix = self.keyword("prefix")?;
        let set = self.import_set()?;
        let id = self.identifier()?;
        self.node(NodeKind::List, vec!(prefix, set, id))
    }

    fn rename(&mut self) -> ParseResult {
        let rename = self.keyword("rename")?;
        let set = self.import_set()?;
        self.paren_open()?;

        let pairs = self.one_or_more(Parser::identifier_pair)?;


        self.paren_close()?;
        self.node(
            NodeKind::List,
            once(rename)
            .chain(once(set))
            .chain(pairs)
            .collect()
        )
    }

    fn identifier_pair(&mut self) -> ParseResult {
        self.paren_open()?;
        let id1 = self.identifier()?;
        let id2 = self.identifier()?;

        let pair = self.node(NodeKind::List, vec![id1, id2]);
        self.paren_close()?;
        pair
    }

    fn cond_expand(&mut self) -> ParseResult {
        let cond_expand = self.keyword("cond-expand")?;

        let clauses = self.one_or_more(Parser::cond_clause)?;

        self.node(
            NodeKind::List,
            once(cond_expand)
            .chain(clauses)
            .collect()
        )
    }

    fn cond_clause(&mut self) -> ParseResult {
        self.paren_open()?;
        let requirement = self.feature_requirement()?;
        let declarations = self.zero_or_more(Parser::library_declaration)?;
        self.paren_close()?;

        self.node(NodeKind::List, once(requirement).chain(declarations).collect())

    }

    fn feature_requirement(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Identifier(_) => self.identifier(),
            Token::ParenOpen => self.feature_requirement_with_paren(),
            _ => Err(Error::new(ErrorKind::UnexpectedToken{unexpected: self.peek()?.clone(), expected: "identifier or open parenthesis"})),
        }
    }

    fn feature_requirement_with_paren(&mut self) -> ParseResult {
        self.paren_open()?;

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

        self.paren_close()?;

        requirement
    }

    fn and(&mut self) -> ParseResult {
        let and = self.keyword("and")?;
        let requirements = self.zero_or_more(Parser::feature_requirement)?;
        self.node(NodeKind::List, once(and).chain(requirements).collect())
    }

    fn or(&mut self) -> ParseResult {
        let or = self.keyword("or")?;
        let requirements = self.zero_or_more(Parser::feature_requirement)?;
        self.node(NodeKind::List, once(or).chain(requirements).collect())
    }

    fn not(&mut self) -> ParseResult {
        let not = self.keyword("not")?;
        let requirement = self.feature_requirement()?;
        self.node(NodeKind::List, vec!(not, requirement))
    }

    fn include_library_declarations(&mut self) -> ParseResult {
        let include_library_declarations = self.keyword("include-library-declarations")?;
        let strings = self.one_or_more(Parser::string)?;

        self.node(
            NodeKind::List,
            once(include_library_declarations)
            .chain(strings)
            .collect()
        )
    }
    //
    // Helpers
    //
    //
    //

    #[inline]
    fn peek(&mut self) -> Result<&Token, Error> {
        self.lexer.peek().ok_or(Error::new(ErrorKind::EndOfInput))
    }
    
    fn node(&mut self, kind: NodeKind, children: Vec<Box<Node>>) -> ParseResult {
        Ok(Box::new(Node::Inner(kind, children)))
    }
    
    fn leaf(&mut self, kind: NodeKind) -> ParseResult {
        Ok(Box::new(Node::Leaf(kind, self.lexer.next().unwrap())))
    }

    fn keyword(&mut self, keyword: &str) -> ParseResult {
        let token = self.peek()?;
        
        match token {
            Token::Identifier(s) if keyword == s => {
                self.identifier()
            },
            _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: token.clone() , expected: "a  keyword" })),
        }
        
    }
    
    fn dot(&mut self) -> Result<(), Error> {
        match self.peek()? {
            Token::Dot => {
                self.lexer.next(); // consume Dot
                Ok(())
            },
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "Dot" })),
        }
    }

    fn quote(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Quote => {
                self.leaf(NodeKind::Quote)
            },
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "Quote" })),
        }
    }

    fn quasiquote(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Quasiquote => {
                self.leaf(NodeKind::Quasiquote)
            },
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "quasiquote" })),
        }
    }

    fn comma(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Comma => {
                self.leaf(NodeKind::Comma)
            },
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "comma" })),
        }
    }

    fn comma_at(&mut self) -> ParseResult {
        match self.peek()? {
            Token::CommaAt => {
                self.leaf(NodeKind::CommaAt)
            },
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: ",@" })),
        }
    }

    fn paren_open(&mut self) -> Result<(), Error> {
        match self.peek()? {
            Token::ParenOpen => {
                self.lexer.next(); // consume ParenOpen
                Ok(())
            },
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "(" })),
        }
    }
    
    fn paren_close(&mut self) -> Result<(), Error> {
        match self.peek()? {
            Token::ParenClose => {
                self.lexer.next(); // consume ParenClose
                Ok(())
            },
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: ")" })),
        }
    }

    fn sharpopen(&mut self) -> Result<(), Error> {
        match self.peek()? {
            Token::SharpOpen => {
                self.lexer.next(); // consume SharpOpen
                Ok(())
            },
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "#(" })),
        }
    }

    fn sharpu8open(&mut self) -> Result<(), Error> {
        match self.peek()? {
            Token::SharpU8Open => {
                self.lexer.next(); // consume SharpU8Open
                Ok(())
            },
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "#u8(" })),
        }
    }
    
    fn identifier(&mut self) -> ParseResult {        
        match self.peek()? {
            Token::Identifier(_) => self.leaf(NodeKind::Identifier),
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "identifier" })),
        }        
    }

    fn identifier_list_possible_dot(&mut self) -> ParseResult {
        match self.peek()? {
            Token::ParenClose => self.node(NodeKind::List, vec!()), // empty list
            Token::Identifier(_) => {
                let ids = self.one_or_more(Parser::identifier)?;

                match self.peek()? {
                    Token::ParenClose => self.node(NodeKind::List, ids),
                    Token::Dot => {
                        self.dot()?;
                        let last = self.identifier()?;
                        self.node(
                            NodeKind::List, 
                            ids
                            .into_iter()
                            .chain(once(last))
                            .collect()
                        )
                    },
                    t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "close parenthesis or dot" })),
                }
            },
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "close parenthesis or dot" })),
        }   
    }
    
    fn boolean(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Boolean(_) => self.leaf(NodeKind::Literal),
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "boolean" })),
        }
    }

    fn character(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Character(_) => self.leaf(NodeKind::Literal),
           t @  _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "character" })),
        }
    }

    fn string(&mut self) -> ParseResult {
        match self.peek()? {
            Token::String(_) => self.leaf(NodeKind::Literal),
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "string" })),
        }
    }

    fn zero_or_more(&mut self, closure: fn(&mut Self) -> ParseResult) -> ParseVecResult {
        Ok(from_fn(|| closure(self).ok()).collect())
    }

    fn one_or_more(&mut self, closure: fn(&mut Self) -> ParseResult) -> ParseVecResult {
        Ok(once(closure(self)?).chain(from_fn(|| closure(self).ok())).collect())
    }

    fn number(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Number(_) => self.leaf(NodeKind::Literal),
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "number" })),
        }
    }

    fn uinteger10(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Number(n) if n.chars().all(|c| c.is_digit(10)) => self.leaf(NodeKind::Literal),
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "uninteger10" })),
        }
    }

    fn vector(&mut self) -> ParseResult {
        self.sharpopen()?;
        let data = self.datum_list_possible_dot()?;
        self.paren_close()?;
        self.node(NodeKind::Vector, data)
    }

    fn bytevector(&mut self) -> ParseResult {
        self.sharpu8open()?;
        let data = self.datum_list_possible_dot()?;
        self.paren_close()?;
        self.node(NodeKind::ByteVector, data)
    }

}
