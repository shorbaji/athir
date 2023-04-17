//! Athir parser module
//! Implements recursive descent parser for R7RS Scheme
//! 
//! TODO:
//! 
//! - quasiquotation
//! - define-library
//! - node captures source code
//!  
//! Example usage:
//! ```
//!     // we first create a Source using an iterator over String
//!     // in this example we use stdin
//!     // we then create a Parser using the Source
//!     // we then iterate over the Parser to get expressions
//!     
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


#[cfg(test)]
mod tests;
mod node;
mod lexer;

use std::iter::{once, from_fn};

pub use node::{NodeKind, Node};
pub use lexer::{Token, Source};
pub use crate::error::{Error, ErrorKind};

use lexer::Lexer;

type ParseResult = std::result::Result<Box<Node>, Error>;

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
            self.lexer.next(); // consume the token that caused the error
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
            Token::ParenOpen => self.paren_open().and(self.compound_expr()),
            _ => self.atom(),
        }
    }

    fn atom(&mut self) -> ParseResult {        
        match self.peek()? {
            Token::Boolean(_) | Token::Character(_) | Token::String(_) | Token::Number(_) => self.leaf(NodeKind::Literal),
            Token::Identifier(_) => self.leaf(NodeKind::Identifier),
            Token::SharpOpen => self.vector().and_then(|vector| self.node(NodeKind::Vector, vec![vector])),
            Token::SharpU8Open => self.bytevector().and_then(|bytevector| self.node(NodeKind::ByteVector, vec![bytevector])),
            Token::Quote => self.quotation_apostrophe(),
            t @ _ => Err(Error{kind: ErrorKind::UnexpectedToken{unexpected: t.clone(), expected: "atom"}}) ,
        }
    }
    
    /// 
    /// this function handles a non-atomic expression 
    /// starting with a parenthesis we then first look for keywords
    /// if not keyword found we fall back to a procedure call
    /// 
    
    fn compound_expr(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Identifier(id) => 
                match id.as_str() {
                    "begin" => self.begin(),
                    "define" | "define-values" | "define-record-type" | "define-syntax" => self.definition(),
                    "if" => self.conditional(),
                    "include" => self.include(),
                    "include-ci" => self.include_ci(),
                    "lambda" => self.lambda(),
                    "let-syntax" | "letrec-syntax" => self.macro_block(),
                    "quote" => self.quotation(),
                    "set!" => self.assignment(),
                    _ => self.procedure_call(),
                },
            _ => self.procedure_call(),
        }
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
        self.keyword("begin").and_then(|_| 
            self.command_or_definition_list().and_then(|nodes| 
                self.paren_close().and_then(|_| {
                    if nodes.iter().all(|node| node.is_definition_expr()) {
                        return self.node(NodeKind::BeginDef, nodes)
                    } else {
                        self.node(NodeKind::Begin, nodes)
                    }
                })
            ),
        )       
    }

    fn command_or_definition_list(&mut self) -> Result<Vec<Box<Node>>, Error> {
        Ok(from_fn(|| self.command_or_definition().ok()).collect::<Vec<Box<Node>>>())
    }

    fn command_or_definition(&mut self) -> ParseResult {
        match self.lexer.peek().ok_or(Error::new(ErrorKind::EndOfInput))? {
            Token::ParenOpen => self.paren_open().and_then(|_| {
                match self.peek()? {
                    Token::Identifier(id) if matches!(
                                                id.as_str(),
                                                "define"
                                                | "define-values"
                                                | "define-record-type"
                                                | "define-syntax"
                                            ) => self.definition(),
                    Token::Identifier(id) if matches!(
                                                id.as_str(),
                                                "begin"
                                            ) => self.begin(),
                    _ => self.compound_expr(),
                }
            }),
            _ => self.atom(),
        }
    }

    ///
    /// Definitions
    /// 
    /// We start with the define keyword and then look for either 
    /// - an identifier (i.e. a variable definition) or,
    /// - a parenthesized list of identifiers (i.e. a function definition)
    /// 

    fn definition(&mut self) -> ParseResult {
        match self.peek()? {
            t @ Token::Identifier(id) => 
                match id.as_str() {
                    "define" => self.variable_or_function_definition(),
                    "define-values" => self.values_definition(),
                    "define-record-type" => self.record_type_definition(),
                    "define-syntax" => self.syntax_definition(),
                    _ => Err(Error::new(ErrorKind::UnexpectedToken{unexpected: t.clone(), expected: "define, define-values, define-record-type, define-syntax"})),
                },
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken{unexpected: t.clone(), expected: "define, define-values, define-record-type, define-syntax"})),
        }
    }

    fn variable_or_function_definition(&mut self) -> ParseResult {
        self.keyword("define").and_then(|_| 
            match self.peek()? {
                Token::Identifier(_) => self.variable_definition(),
                Token::ParenOpen => self.function_definition(),
                t @ _ => Err(Error::new(ErrorKind::UnexpectedToken{unexpected: t.clone(), expected: "identifier or open paren"})),
            },
        )
    }
        
    fn variable_definition(&mut self) -> ParseResult {
        self.identifier().and_then(
            |id| self.expr().and_then(
                |expr| self.paren_close().and_then(
                    |_| self.node(NodeKind::VariableDefinition, vec![id, expr])
                )
            )
        )
    }
        
    fn function_definition(&mut self) -> ParseResult {
        self.paren_open().and_then(|_| 
            self.identifier().and_then(|id| 
                self.def_formals().and_then(|formals| 
                    self.paren_close().and_then(|_| 
                        self.body().and_then(|body| 
                            self.paren_close().and_then(|_| 
                                self.node(NodeKind::FunctionDefinition, vec![id, formals, body])
                            )
                        )
                    )
                )
            )
        )
    } 

    fn def_formals(&mut self) -> ParseResult {
        self.identifier_list()
    }

    fn values_definition(&mut self) -> ParseResult {
        self.keyword("define-values").and_then(|_| 
            self.formals().and_then(|formals| 
                self.body().and_then(|exprs| 
                    self.paren_close().and_then(|_| 
                        self.node(NodeKind::ValuesDefinition, vec![formals, exprs])
                    )
                )
            )
        )
    }

    fn record_type_definition(&mut self) -> ParseResult {
        self.keyword("define-record-type").and_then(|_| 
            self.identifier().and_then(|id1| 
                self.constructor().and_then(|constructor| 
                    self.identifier().and_then(|id2| 
                        self.field_specs().and_then(|field_specs| 
                            self.paren_close().and_then(|_| 
                                self.node(NodeKind::RecordTypeDefinition, vec![id1, constructor, id2, field_specs])
                            )
                        )
                    )
                )
            )
        )
    }

    fn constructor(&mut self) -> ParseResult {
        self.paren_open().and_then(|_|
            self.identifier().and_then(|id|
                self.identifier_list().and_then(|field_names|
                    self.paren_close().and_then(|_|
                        self.node(NodeKind::List, vec![id, field_names])
                    )
                )
            )
        )
    }

    fn field_specs (&mut self) -> ParseResult {
        Ok(from_fn(|| self.field_spec().ok()).collect::<Vec<Box<Node>>>()).and_then(|nodes|
            self.node(NodeKind::List, nodes))
    }

    fn field_spec(&mut self) -> ParseResult {
        self.paren_open().and_then(|_|
            self.identifier().and_then(|field_name|
                self.identifier().and_then(|accessor|
                    match self.peek()? {
                        Token::ParenClose => {
                            return self.node(NodeKind::List, vec![field_name, accessor])
                        },
                        _ => self.identifier().and_then(|mutator|
                                self.paren_close().and_then(|_|
                                    self.node(NodeKind::List, vec![field_name, accessor, mutator])
                                )
                            ),
                    }
                )
            )
        )
    }

    fn syntax_definition(&mut self) -> ParseResult {
        self.keyword("define-syntax").and_then(|_| 
            self.identifier().and_then(|id| 
                self.transformer_spec().and_then(|expr| 
                    self.paren_close().and_then(|_| 
                        self.node(NodeKind::SyntaxDefinition, vec![id, expr])
                    )
                )
            )
        )
    }

    ///
    /// Conditionals
    /// 
    /// 
    ///
    fn conditional(&mut self) -> ParseResult {
        self.keyword("if").and_then(
            |_| self.expr().and_then(
                |test| self.expr().and_then(
                    |consequent| 
                    match self.peek()? {
                        Token::ParenClose => {
                            self.lexer.next(); // consume ParenClose
                            return self.node(NodeKind::Conditional, vec![test, consequent])
                        },
                        
                        _ => self.expr().and_then(|alternative| 
                            self.paren_close()
                            .or(Err(Error::new(ErrorKind::NotImplemented)))
                            .and_then(|_| self.node(NodeKind::Conditional, vec![test, consequent, alternative])
                            )
                        ),
                    }
                )
            )
        )
    }        
        
    ///
    /// Lambda
    /// 
    /// 
    ///

    fn lambda(&mut self) -> ParseResult {
        self.keyword("lambda").and_then(|_| 
            self.formals().and_then(|formals|
                self.body().and_then(|body| 
                    self.paren_close().and_then(|_| 
                        self.node(NodeKind::Lambda, vec![formals, body])
                    )
                )
            )
        )
    }

    fn formals(&mut self) -> ParseResult {   
        match self.peek()? {
            Token::Identifier(_) => self.identifier(),
            Token::ParenOpen => self.paren_open().and_then(|_|
                                    self.identifier_list().and_then(|ids|
                                        self.paren_close().and_then(|_|
                                            Ok(ids)
                                        )
                                    )
                                ),
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken{unexpected: t.clone(), expected: "identifier or open parenthesis"})),
        }
    }

    /// body should not be empty
    /// body can have defintions and expressions
    /// body cannot have definitions after any expression
    
    fn body(&mut self) -> ParseResult {        
        match self.peek()? {
            Token::ParenClose => Err(Error::new(ErrorKind::EmptyBodyinLambda)),
            _ => self.expr_list().and_then(|exprs| {
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
            }),
        }
    }

    ///
    /// Quotations
    /// 
    /// 
    ///

    fn quotation(&mut self) -> ParseResult {
        self.keyword("quote").and_then(|_| 
            self.datum().and_then(|datum|
                self.paren_close().and_then(|_|
                    self.node(NodeKind::Quotation, vec![datum])
                )
            )
        )
    }

    ///
    /// Assignments
    /// 
    /// 
    ///

    fn assignment(&mut self) -> ParseResult {
        self.keyword("set!").and_then(|_|
            self.identifier().and_then(|id| 
                self.expr().and_then(|expr| 
                    self.paren_close().and_then(|_| 
                        self.node(NodeKind::Assignment, vec![id, expr])
                    )
                )
            )
        )        
    }

    ///
    /// Procedure calls
    ///
    ///
    /// 

    fn procedure_call(&mut self) -> ParseResult {
        self.expr().and_then(|operator| 
            self.expr_list().and_then(|operands|
                self.paren_close().and_then(|_| {
                    self.node(
                        NodeKind::ProcedureCall,
                        once(operator)
                        .chain(operands.into_iter())
                        .collect())    
                })
            )
        )
    }

    ///
    /// Macro blocks
    /// 
    /// 
    ///

    fn macro_block(&mut self) -> ParseResult {
        // we look for the keywords let-syntax or letrec-syntax
        let node = match self.peek()? {
            t @ Token::Identifier(id) => 
                match id.as_str() {
                    "let-syntax" => self.let_syntax(),
                    "letrec-syntax" => self.letrec_syntax(),
                     _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "let-syntax or letrec-syntax" })),
                },
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "let-syntax or letrec-syntax" })),
        }?;

        self.node(NodeKind::MacroBlock, vec![node])
    }

    fn let_syntax(&mut self) -> ParseResult {
        self.keyword("let-syntax").and_then(|_| 
            self.macro_block_suffix().and_then(|(transformer_specs, body)|
                self.node(NodeKind::LetSyntax, vec![transformer_specs, body])))
    }

    fn letrec_syntax(&mut self) -> ParseResult {
        self.keyword("letrec-syntax").and_then(|_| 
            self.macro_block_suffix().and_then(|(transformer_specs, body)|
                self.node(NodeKind::LetRecSyntax, vec![transformer_specs, body])))
    }

    fn macro_block_suffix(&mut self) -> Result<(Box<Node>, Box<Node>), Error> {
        // ( <syntax spec>* ) <body> )
        self.paren_open().and_then(|_|
            self.syntax_specs().and_then(|syntax_specs|
                self.paren_close().and_then(|_|
                    self.body().and_then(|body|
                        self.paren_close().and_then(|_|
                            Ok((syntax_specs, body)))))))
    }

    fn syntax_specs(&mut self) -> ParseResult {
        
        // <syntax spec>*
        Ok(from_fn(|| self.syntax_spec().ok()).collect::<Vec<Box<Node>>>())
            .and_then(|specs| self.node(NodeKind::SyntaxSpecList, specs))
    }

    fn syntax_spec(&mut self) -> ParseResult {
        // ( <keyword> <transformer spec> )
        self.paren_open().and_then(|_|
            self.identifier().and_then(|keyword| 
                self.transformer_spec().and_then(|transformer_spec|
                    self.paren_close().and_then(|_|
                        self.node(NodeKind::SyntaxSpec, vec![keyword, transformer_spec])))))
    }

    ///
    /// Transformer (R7RS section 7.1.5) 
    /// [INCOMPLETE]
    /// 
    
    fn transformer_spec(&mut self) -> ParseResult {
        
        self.paren_open().and_then(|_|
            self.keyword("syntax-rules").and_then(|_| 
                match self.peek()? {
                    Token::Identifier(_) => self.identifier().and_then(|id|
                        self.transformer_spec_suffix().and_then(|(transformer_spec_identifier_list, syntax_rule_list)|
                            self.node(NodeKind::TransformerSpec, vec![id, transformer_spec_identifier_list, syntax_rule_list]))),
                    Token::ParenOpen => self.transformer_spec_suffix().and_then(|(transformer_spec_identifier_list, syntax_rule_list)|
                        self.node(NodeKind::TransformerSpec, vec![transformer_spec_identifier_list, syntax_rule_list])),
                    t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "identifier or open parenthesis" })),
                }))
    }

    fn transformer_spec_suffix(&mut self) -> Result<(Box<Node>, Box<Node>), Error> {
        // ( <identifier>* ) <syntax rule>* )
        self.paren_open().and_then(|_|
            self.transformer_spec_identifier_list().and_then(|transformer_spec_identifier_list|
                self.paren_close().and_then(|_|
                    self.syntax_rule_list().and_then(|syntax_rule_list|
                        self.paren_close().and_then(|_| 
                            Ok((transformer_spec_identifier_list, syntax_rule_list))
                        )
                    )
                )
            )
        )
    }

    fn transformer_spec_identifier_list(&mut self) -> ParseResult {
        // <identifier>*
        Ok(from_fn(|| self.identifier().ok()).collect::<Vec<Box<Node>>>())
            .and_then(|ids| self.node(NodeKind::TransformerSpecIdentifierList, ids))
    }

    fn syntax_rule_list(&mut self) -> ParseResult {
        // <syntax rule>*
        Ok(from_fn(|| self.syntax_rule().ok()).collect::<Vec<Box<Node>>>())
            .and_then(|syntax_rules| self.node(NodeKind::SyntaxRuleList, syntax_rules))
    }

    fn syntax_rule(&mut self) -> ParseResult {
        // ( <pattern> <template> )
        self.paren_open().and_then(|_|
            self.pattern().and_then(|pattern|
                self.template().and_then(|template|
                    self.paren_close().and_then(|_|
                        self.node(NodeKind::SyntaxRule, vec![pattern, template])
                    )
                )
            )
        )
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
        }.and_then(|pattern|
            self.node(NodeKind::Pattern, vec!(pattern))
        )
    }

    fn pattern_datum(&mut self) -> ParseResult {
        self.leaf(NodeKind::PatternDatum)
    }

    fn pattern_identifier(&mut self) -> ParseResult {
        let token = self.peek()?;

        match token {
            Token::Identifier(s) if s.as_str() == "..." => Err(Error::new(ErrorKind::EllipsisNotValidPatternIdentifier)),
            _ => self.identifier().and_then(|id| self.node(NodeKind::PatternIdentifier, vec![id])),
        }
    }

    fn pattern_underscore(&mut self) -> ParseResult {
        self.keyword("_").and_then(|_| 
            self.node(NodeKind::PatternUnderscore, vec!()))
    }


    fn pattern_with_paren(&mut self) -> ParseResult {
        self.paren_open().and_then(|_|
            self.pattern_with_paren_a().and_then(|pattern_list|
                self.paren_close().and_then(|_|
                    self.node(NodeKind::PatternWithParen, vec![pattern_list])
                )
            )
        )
    }


    fn pattern_with_paren_a(&mut self) -> ParseResult {
        // | <pattern>*
        // | <pattern>+ . <pattern>
        // | <pattern>* <pattern> <ellipsis> <pattern>*
        // | <pattern>* <pattern> <ellipsis> <pattern>* . <pattern>

        match self.peek()? {
            
            Token::ParenClose => self.node(NodeKind::PatternParen, vec!()), // empty

            _ => {
                let mut pre_ellipse_patterns = self.pattern_pre_ellipse()?;

                match self.peek()? {
                    // <pattern>*            
                    Token::ParenClose => self.node(NodeKind::PatternParen, vec!(pre_ellipse_patterns)),

                    // | <pattern>+ . <pattern>
                    Token::Dot => 
                        self.dot().and_then(|_|
                            self.pattern().and_then(|pattern| {
                                pre_ellipse_patterns.add_child(pattern);
                                self.node(NodeKind::PatternParen, vec!(pre_ellipse_patterns))
                            }
                            )
                        ),
                    Token::Identifier(id) if  id.as_str() == "..." => 
                        self.identifier().and_then(|_ellipsis| {
                            let mut post_ellipse_patterns = self.pattern_post_ellipse()?;
                            match self.peek()? {
                                // | <pattern>* <pattern> <ellipsis> <pattern>*
                                Token::ParenClose => self.node(NodeKind::PatternParen, vec!(pre_ellipse_patterns, post_ellipse_patterns)),
                                // | <pattern>* <pattern> <ellipsis> <pattern>* . pattern
                                Token::Dot => 
                                    self.dot().and_then(|_|
                                        self.pattern().and_then(|pattern| {
                                            post_ellipse_patterns.add_child(pattern);
                                            self.node(NodeKind::PatternParen, vec!(pre_ellipse_patterns, post_ellipse_patterns))
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

    fn pattern_pre_ellipse(&mut self) -> ParseResult {
        // <pattern>*
        Ok(from_fn(|| self.pattern().ok()).collect()).and_then(|patterns|
            self.node(NodeKind::PatternPreEllipse, patterns))
    }

    fn pattern_post_ellipse(&mut self) -> ParseResult {
        // <pattern>*
        Ok(from_fn(|| self.pattern().ok()).collect()).and_then(|patterns|
            self.node(NodeKind::PatternPostEllipse, patterns))
    }
    
    fn pattern_with_sharp_paren(&mut self) -> ParseResult {
        //  #( <pattern>* )
        // | #( <pattern>* <pattern> <ellipsis> <pattern>* )

        self.sharpopen().and_then(|_|
            self.pattern_with_sharp_paren_a().and_then(|pattern_list|
                self.paren_close().and_then(|_|
                    self.node(NodeKind::PatternSharp, vec![pattern_list]))))

    }

    fn pattern_with_sharp_paren_a(&mut self) -> ParseResult {
        match self.peek()? {
            Token::ParenClose => self.node(NodeKind::PatternSharp, vec!()),
            _ => {
                let pre_ellipse_patterns = self.pattern_pre_ellipse()?;
                match self.peek()? {
                    Token::ParenClose => self.node(NodeKind::PatternSharp, vec!(pre_ellipse_patterns)),
                    Token::Identifier(id) if  id.as_str() == "..." =>
                        self.identifier().and_then(|_ellipsis| {
                            let post_ellipse_patterns = self.pattern_post_ellipse()?;
                            match self.peek()? {
                                Token::ParenClose => self.node(NodeKind::PatternSharp, vec!(pre_ellipse_patterns, post_ellipse_patterns)),
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
        self.pattern_identifier().and_then(|id|
            self.node(NodeKind::Template, vec![id]))
    }

    fn template_with_paren(&mut self) -> ParseResult {
        self.paren_open().and_then(|_|
            self.template_with_paren_a().and_then(|template|
                self.paren_close().and_then(|_|
                    self.node(NodeKind::TemplateWithParen, template))))
    }

    fn template_with_paren_a(&mut self) -> Result<Vec<Box<Node>>, Error> {
        match self.peek()? {
            Token::ParenClose => Ok(vec!()), // empty list
            _ => self.template_element().and_then(|first| // non-empty list so get first template_element
                    self.template_element_sequence().and_then(|template_element_sequence| // get rest of template_element sequence
                        match self.peek()? {
                            Token::ParenClose => Ok(once(first).chain(template_element_sequence).collect()),
                            Token::Dot => self.dot().and_then(|_|
                                            self.template_element().and_then(|last|
                                                Ok(once(first)
                                                    .chain(template_element_sequence)
                                                    .chain(once(last))
                                                    .collect()))),
                            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "close parenthesis or dot" })),
                        }
                    )
                ),
        }   
    }

    fn template_element(&mut self) -> ParseResult {
        let template = self.template()?;
        
        match self.peek()? {
            Token::Identifier(id) if id.as_str() == "..." => { 
                self.lexer.next();
                self.node(NodeKind::TemplateElementEllipsis, vec![template])
            }
            _ => self.node(NodeKind::TemplateElement, vec![template])
        }
    }

    fn template_with_sharp_paren(&mut self) -> ParseResult {
        self.sharpopen().and_then(|_|
            self.template_element_sequence().and_then(|template_list|
                self.paren_close().and_then(|_|
                    self.node(NodeKind::TemplateSharp, template_list)
                )
            )
        )
    }

    fn template_element_sequence(&mut self) -> Result<Vec<Box<Node>>, Error> {
        Ok(from_fn(|| self.template_element().ok()).collect())
    }

    fn template_datum(&mut self) -> ParseResult {
        self.leaf(NodeKind::TemplateDatum)
    }

    ///
    /// Datum (R7RS section 7.1.3 - External Representation)
    /// 
    /// 
    /// 
    fn datum(&mut self) -> ParseResult {
        self.boolean()
        .or_else(|_| self.character())
        .or_else(|_| self.string())
        .or_else(|_| self.number())
        .or_else(|_| self.symbol())
        .or_else(|_| self.abbreviation())
        .or_else(|_| self.list())
        .or_else(|_| self.vector())
        .and_then(|node| self.node(NodeKind::Datum, vec![node]))
    }

    fn symbol(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Identifier(_) => self.leaf(NodeKind::Symbol),
            t @ _ => return Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "symbol" })),
        }
    }

    fn abbreviation(&mut self) -> ParseResult {
        self.quote()
        .or_else(|_| self.quasiquote())
        .or_else(|_| self.comma())
        .or_else(|_| self.comma_at())
        .and_then(|prefix| 
            self.datum().and_then(|expr| 
                self.node(NodeKind::List, vec!(prefix, expr))
            )
        )
    }

    fn list(&mut self) -> ParseResult {
        self.paren_open().and_then(|_| 
            self.datum_list().and_then(|data| 
                self.paren_close().and_then(|_| 
                    self.node(NodeKind::List, data)
                )
            )
        )
    }

    fn datum_list(&mut self) -> Result<Vec<Box<Node>>, Error> {
        match self.peek()? {
            Token::ParenClose => Ok(vec!()), // empty list
            _ => self.datum()
                    .and_then(|first| // non-empty list so get first datum
                        self.datum_sequence().and_then(|datum_sequence| // get rest of datum sequence
                            match self.peek()? {
                                Token::ParenClose => Ok(once(first).chain(datum_sequence).collect()),
                                Token::Dot => self.dot().and_then(|_|
                                                self.datum().and_then(|last|
                                                    Ok(once(first)
                                                        .chain(datum_sequence)
                                                        .chain(once(last))
                                                        .collect()))),
                                t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "close parenthesis or dot" })),
                            }
                        )
                    ),
       }
    }

    fn datum_sequence(&mut self) -> Result<Vec<Box<Node>>, Error> {
        Ok(from_fn(|| self.datum().ok()).collect())
    }

    //
    // Includer
    //
    //
    //

    fn include(&mut self) -> ParseResult {
        self.keyword("include").and_then(|_| 
            self.string().and_then(|path| 
                self.string_list().and_then(|strings| 
                    self.paren_close().and_then(|_|
                        self.node(NodeKind::Includer, once(path).chain(strings.into_iter()).collect())
                    )
                )
            )
        )
    }

    fn include_ci(&mut self) -> ParseResult {
        self.keyword("include-ci").and_then(|_| 
            self.string().and_then(|path| 
                self.string_list().and_then(|strings| 
                    self.paren_close().and_then(|_|
                        self.node(NodeKind::IncluderCI, once(path).chain(strings.into_iter()).collect())
                    )
                )
            )
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

    fn keyword(&mut self, keyword: &str) -> Result<(), Error> {
        let token = self.peek()?;
        
        match token {
            Token::Identifier(s) if keyword == s => {
                self.lexer.next(); // consume keyword
                Ok(())
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

    fn expr_list(&mut self) -> Result<Vec<Box<Node>>, Error> {
        Ok(from_fn(|| self.expr().ok()).collect())
    }
    
    fn identifier(&mut self) -> ParseResult {        
        match self.peek()? {
            Token::Identifier(_) => self.leaf(NodeKind::Identifier),
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "identifier" })),
        }        
    }

    fn identifier_list(&mut self) -> ParseResult {
        match self.peek()? {
            Token::ParenClose => self.node(NodeKind::List, vec!()), // empty list
            Token::Identifier(_) => self.identifier().and_then(|first| // non-empty list so get first identifier
                self.identifier_sequence().and_then(|identifier_sequence| // get rest of identifier sequence
                    match self.peek()? {
                        Token::ParenClose => self.node(NodeKind::List, once(first).chain(identifier_sequence).collect()),
                        Token::Dot => self.dot().and_then(|_|
                                        self.identifier().and_then(|last|
                                            self.node(
                                                NodeKind::List, 
                                                once(first)
                                                .chain(identifier_sequence)
                                                .chain(once(last))
                                                .collect()
                                            )
                                        )
                                    ),
                        t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "close parenthesis or dot" })),
                    }
                )
            ),
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "close parenthesis or dot" })),
        }   
    }
    
    fn identifier_sequence(&mut self) -> Result<Vec<Box<Node>>, Error> {
        Ok(from_fn(|| self.identifier().ok()).collect())
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

    fn string_list(&mut self) -> Result<Vec<Box<Node>>, Error> {
        Ok(from_fn(|| self.string().ok()).collect())
    }

    fn number(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Number(_) => self.leaf(NodeKind::Literal),
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "number" })),
        }
    }

    fn vector(&mut self) -> ParseResult {
        self.sharpopen().and_then(|_| 
            self.datum_list().and_then(|data| 
                self.paren_close().and_then(|_| 
                    self.node(NodeKind::Vector, data))))
    }

    fn bytevector(&mut self) -> ParseResult {
        self.sharpu8open().and_then(|_| 
            self.datum_list().and_then(|data| 
                self.paren_close().and_then(|_| 
                    self.node(NodeKind::ByteVector, data)
                )
            )
        )
    }

    /// Implements '<datum> (i.e. single quote followed by datum)
    fn quotation_apostrophe(&mut self) -> ParseResult {
        self.quote().and_then(
            |_| self.datum().and_then(
                |expr| self.node(NodeKind::Quotation, vec![expr])
            )
        )
    }

}
