//! Athir parser module
//!
//! TODO:
//! [P0]
//! - quasiquotation
//! - support for multiline input
//! 
//! [P1]
//! - parsetree captures source code
//! - node captures source code
//! 
//! [P2]
//! - includer
//! - define-library
//! - Support for # label in datum.
//! 
//! Known issues:
//! - string inline hex escapes not implemented
//! - derived expressions not implemented (deferred to macro expansion)

#[cfg(test)]
mod tests;

pub mod parsetree;

use std::iter::once;
use std::iter::{from_fn, Peekable};

pub use crate::read::lexer::Lexer;
pub use crate::read::lexer::Token;
pub use crate::read::parser::parsetree::{Kind, Node};
use crate::error::{Error, ErrorKind};

/// Implements recursive descent parser and uses peekable lexer iterator 
/// to implement LL(1) lookahead
pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

type ParseResult = std::result::Result<Box<Node>, Error>;

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { 
            lexer: Lexer::new(input).peekable() 
        }
    }
    
    pub fn parse(&mut self) -> ParseResult {
        self.program()
    }

    fn program(&mut self) -> ParseResult {
        self.command_or_definition().and_then(|node| 
            Ok(from_fn(|| self.command_or_definition().ok()).collect::<Vec<Box<Node>>>()).and_then(|nodes|
                self.node(
                    Kind::Program,
                    once(node).chain(nodes.into_iter()).collect::<Vec<Box<Node>>>())
            )
        )
    }

    fn command_or_definition(&mut self) -> ParseResult {
        match self.lexer.peek().ok_or(Error::new(ErrorKind::EndOfInput))? {
            Token::ParenOpen => self.paren_open().and_then(|_| {
                match self.peek()? {
                    Token::Identifier(id) if matches!(id.as_str(), "define" | "define-values" | "define-record-type" | "define-syntax") => self.definition(),
                    Token::Identifier(id) if matches!(id.as_str(), "begin") => self.begin(),
                    _ => self.compound_expr(),
                }
            }),
            _ => self.atom(),
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
                        return self.node(Kind::BeginDef, nodes)
                    } else {
                        self.node(Kind::Begin, nodes)
                    }
                })
            ),
        )       
    }

    fn command_or_definition_list(&mut self) -> Result<Vec<Box<Node>>, Error> {
        Ok(from_fn(|| self.command_or_definition().ok()).collect::<Vec<Box<Node>>>())
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
            Token::Boolean(_)
            | Token::Character(_)
            | Token::String(_)
            | Token::Number(_) => self.leaf(Kind::Literal),
            Token::Identifier(_) => self.leaf(Kind::Identifier),
            Token::SharpOpen => self.vector().and_then(|vector| self.node(Kind::Vector, vec![vector])),
            Token::SharpU8Open => self.bytevector().and_then(|bytevector| self.node(Kind::Bytevector, vec![bytevector])),
            Token::Quote => self.quotation_apostrophe(),
            t @ _ => Err(Error{kind: ErrorKind::UnexpectedToken{unexpected: t.clone(), expected: "atom"}}),
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
                    "if" => self.conditional_if(),
                    "lambda" => self.lambda(),
                    "quote" => self.quotation(),
                    "set!" => self.assignment(),
                    "let-syntax" | "letrec-syntax" => self.macro_block(),
                    "define" | "define-values" | "define-record-type" | "define-syntax" => self.definition(),
                    "begin" => self.begin(),
                    _ => self.procedure_call(),
                },
            _ => self.procedure_call(),
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
                    _ => Err(Error::new(ErrorKind::UnexpectedToken{unexpected: t.clone().clone(), expected: "define, define-values, define-record-type, define-syntax"})),
                },
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken{unexpected: t.clone().clone(), expected: "define, define-values, define-record-type, define-syntax"})),
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
                    |_| self.node(Kind::VariableDefinition, vec![id, expr])
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
                                self.node(Kind::FunctionDefinition, vec![id, formals, body])
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
        println!("values_definition"); 
        self.keyword("define-values").and_then(|_| 
                self.formals().and_then(|formals| 
                    self.body().and_then(|exprs| 
                        self.paren_close().and_then(|_| 
                            self.node(Kind::ValuesDefinition, vec![formals, exprs])
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
                                self.node(Kind::RecordTypeDefinition, vec![id1, constructor, id2, field_specs])
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
                        self.node(Kind::List, vec![id, field_names])
                    )
                )
            )
        )
    }

    fn field_specs (&mut self) -> ParseResult {
        Ok(from_fn(|| self.field_spec().ok()).collect::<Vec<Box<Node>>>()).and_then(|nodes|
            self.node(Kind::List, nodes))
    }

    fn field_spec(&mut self) -> ParseResult {
        self.paren_open().and_then(|_|
            self.identifier().and_then(|field_name|
                self.identifier().and_then(|accessor|
                    match self.peek()? {
                        Token::ParenClose => {
                            return self.node(Kind::List, vec![field_name, accessor])
                        },
                        _ => self.identifier().and_then(|mutator|
                            self.paren_close().and_then(|_|
                                self.node(Kind::List, vec![field_name, accessor, mutator])
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
                        self.node(Kind::SyntaxDefinition, vec![id, expr])
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
    fn conditional_if(&mut self) -> ParseResult {
        self.keyword("if").and_then(
            |_| self.expr().and_then(
                |test| self.expr().and_then(
                    |consequent| 
                    match self.peek()? {
                        Token::ParenClose => {
                            self.lexer.next(); // consume ParenClose
                            return self.node(Kind::Conditional, vec![test, consequent])
                        },
                        
                        _ => self.expr().and_then(|alternative| 
                            self.paren_close()
                            .or(Err(Error::new(ErrorKind::NotImplemented)))
                            .and_then(|_| self.node(Kind::Conditional, vec![test, consequent, alternative])
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
                        self.node(Kind::Lambda, vec![formals, body])
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
                self.node(Kind::List, exprs)
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
                    self.node(Kind::Quotation, vec![datum])
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
                        self.node(Kind::Assignment, vec![id, expr])
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
                        Kind::ProcedureCall,
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

        self.node(Kind::MacroBlock, vec![node])
    }

    fn let_syntax(&mut self) -> ParseResult {
        self.keyword("let-syntax").and_then(|_| 
            self.macro_block_suffix().and_then(|(transformer_specs, body)|
                self.node(Kind::LetSyntax, vec![transformer_specs, body])))
    }

    fn letrec_syntax(&mut self) -> ParseResult {
        self.keyword("letrec-syntax").and_then(|_| 
            self.macro_block_suffix().and_then(|(transformer_specs, body)|
                self.node(Kind::LetRecSyntax, vec![transformer_specs, body])))
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
            .and_then(|specs| self.node(Kind::SyntaxSpecList, specs))
    }

    fn syntax_spec(&mut self) -> ParseResult {
        // ( <keyword> <transformer spec> )
        self.paren_open().and_then(|_|
            self.identifier().and_then(|keyword| 
                self.transformer_spec().and_then(|transformer_spec|
                    self.paren_close().and_then(|_|
                        self.node(Kind::SyntaxSpec, vec![keyword, transformer_spec])))))
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
                            self.node(Kind::TransformerSpec, vec![id, transformer_spec_identifier_list, syntax_rule_list]))),
                    Token::ParenOpen => self.transformer_spec_suffix().and_then(|(transformer_spec_identifier_list, syntax_rule_list)|
                        self.node(Kind::TransformerSpec, vec![transformer_spec_identifier_list, syntax_rule_list])),
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
                            Ok((transformer_spec_identifier_list, syntax_rule_list)))))))
    }

    fn transformer_spec_identifier_list(&mut self) -> ParseResult {
        // <identifier>*
        Ok(from_fn(|| self.identifier().ok()).collect::<Vec<Box<Node>>>())
            .and_then(|ids| self.node(Kind::TransformerSpecIdentifierList, ids))
    }

    fn syntax_rule_list(&mut self) -> ParseResult {
        // <syntax rule>*
        Ok(from_fn(|| self.syntax_rule().ok()).collect::<Vec<Box<Node>>>())
            .and_then(|syntax_rules| self.node(Kind::SyntaxRuleList, syntax_rules))
    }

    fn syntax_rule(&mut self) -> ParseResult {
        // ( <pattern> <template> )
        self.paren_open().and_then(|_|
            self.pattern().and_then(|pattern|
                self.template().and_then(|template|
                    self.paren_close().and_then(|_|
                        self.node(Kind::SyntaxRule, vec![pattern, template])))))
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
        }.and_then(|pattern| self.node(Kind::Pattern, vec!(pattern)))
    }

    fn pattern_datum(&mut self) -> ParseResult {
        self.leaf(Kind::PatternDatum)
    }

    fn pattern_identifier(&mut self) -> ParseResult {
        let token = self.peek()?;

        match token {
            Token::Identifier(s) if s.as_str() == "..." => Err(Error::new(ErrorKind::EllipsisNotValidPatternIdentifier)),
            _ => self.identifier().and_then(|id| self.node(Kind::PatternIdentifier, vec![id])),
        }
    }

    fn pattern_underscore(&mut self) -> ParseResult {
        self.keyword("_").and_then(|_| 
            self.node(Kind::PatternUnderscore, vec!()))
    }


    fn pattern_with_paren(&mut self) -> ParseResult {
        self.paren_open().and_then(|_|
            self.pattern_with_paren_a().and_then(|pattern_list|
                self.paren_close().and_then(|_|
                    self.node(Kind::PatternWithParen, vec![pattern_list]))))
    }


    fn pattern_with_paren_a(&mut self) -> ParseResult {
        // | <pattern>*
        // | <pattern>+ . <pattern>
        // | <pattern>* <pattern> <ellipsis> <pattern>*
        // | <pattern>* <pattern> <ellipsis> <pattern>* . <pattern>

        match self.peek()? {
            
            Token::ParenClose => self.node(Kind::PatternParen, vec!()), // empty

            _ => {
                let mut pre_ellipse_patterns = self.pattern_pre_ellipse()?;

                match self.peek()? {
                    // <pattern>*            
                    Token::ParenClose => self.node(Kind::PatternParen, vec!(pre_ellipse_patterns)),

                    // | <pattern>+ . <pattern>
                    Token::Dot => 
                        self.dot().and_then(|_|
                            self.pattern().and_then(|pattern| {
                                pre_ellipse_patterns.add_child(pattern);
                                self.node(Kind::PatternParen, vec!(pre_ellipse_patterns))
                            }
                            )
                        ),
                    Token::Identifier(id) if  id.as_str() == "..." => 
                        self.identifier().and_then(|_ellipsis| {
                            let mut post_ellipse_patterns = self.pattern_post_ellipse()?;
                            match self.peek()? {
                                // | <pattern>* <pattern> <ellipsis> <pattern>*
                                Token::ParenClose => self.node(Kind::PatternParen, vec!(pre_ellipse_patterns, post_ellipse_patterns)),
                                // | <pattern>* <pattern> <ellipsis> <pattern>* . pattern
                                Token::Dot => 
                                    self.dot().and_then(|_|
                                        self.pattern().and_then(|pattern| {
                                            post_ellipse_patterns.add_child(pattern);
                                            self.node(Kind::PatternParen, vec!(pre_ellipse_patterns, post_ellipse_patterns))
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
            self.node(Kind::PatternPreEllipse, patterns))
    }

    fn pattern_post_ellipse(&mut self) -> ParseResult {
        // <pattern>*
        Ok(from_fn(|| self.pattern().ok()).collect()).and_then(|patterns|
            self.node(Kind::PatternPostEllipse, patterns))
    }
    
    fn pattern_with_sharp_paren(&mut self) -> ParseResult {
        //  #( <pattern>* )
        // | #( <pattern>* <pattern> <ellipsis> <pattern>* )

        self.sharpopen().and_then(|_|
            self.pattern_with_sharp_paren_a().and_then(|pattern_list|
                self.paren_close().and_then(|_|
                    self.node(Kind::PatternSharp, vec![pattern_list]))))

    }

    fn pattern_with_sharp_paren_a(&mut self) -> ParseResult {
        match self.peek()? {
            Token::ParenClose => self.node(Kind::PatternSharp, vec!()),
            _ => {
                let pre_ellipse_patterns = self.pattern_pre_ellipse()?;
                match self.peek()? {
                    Token::ParenClose => self.node(Kind::PatternSharp, vec!(pre_ellipse_patterns)),
                    Token::Identifier(id) if  id.as_str() == "..." =>
                        self.identifier().and_then(|_ellipsis| {
                            let post_ellipse_patterns = self.pattern_post_ellipse()?;
                            match self.peek()? {
                                Token::ParenClose => self.node(Kind::PatternSharp, vec!(pre_ellipse_patterns, post_ellipse_patterns)),
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
            self.node(Kind::Template, vec![id]))
    }

    fn template_with_paren(&mut self) -> ParseResult {
        self.paren_open().and_then(|_|
            self.template_with_paren_a().and_then(|template|
                self.paren_close().and_then(|_|
                    self.node(Kind::TemplateWithParen, template))))
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
                                                })),
        }   
    }

    fn template_element(&mut self) -> ParseResult {
        let template = self.template()?;
        
        match self.peek()? {
            Token::Identifier(id) if id.as_str() == "..." => { 
                self.lexer.next();
                self.node(Kind::TemplateElementEllipsis, vec![template])
            }
            _ => self.node(Kind::TemplateElement, vec![template])
        }
    }

    fn template_with_sharp_paren(&mut self) -> ParseResult {
        self.sharpopen().and_then(|_|
            self.template_element_sequence().and_then(|template_list|
                self.paren_close().and_then(|_|
                    self.node(Kind::TemplateSharp, template_list))))
    }

    fn template_element_sequence(&mut self) -> Result<Vec<Box<Node>>, Error> {
        Ok(from_fn(|| self.template_element().ok()).collect())
    }

    fn template_datum(&mut self) -> ParseResult {
        self.leaf(Kind::TemplateDatum)
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
        .and_then(|node| self.node(Kind::Datum, vec![node]))
    }

    fn symbol(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Identifier(_) => self.leaf(Kind::Symbol),
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
                self.node(Kind::List, vec!(prefix, expr))))
    }

    fn list(&mut self) -> ParseResult {
        self.paren_open().and_then(|_| 
            self.datum_list().and_then(|data| 
                self.paren_close().and_then(|_| 
                    self.node(Kind::List, data))))
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
                            })),
       }
    }

    fn datum_sequence(&mut self) -> Result<Vec<Box<Node>>, Error> {
        Ok(from_fn(|| self.datum().ok()).collect())
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
    
    fn node(&mut self, kind: Kind, children: Vec<Box<Node>>) -> ParseResult {
        Ok(Box::new(Node::Inner(kind, children)))
    }
    
    fn leaf(&mut self, kind: Kind) -> ParseResult {
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
                self.leaf(Kind::Quote)
            },
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "Quote" })),
        }
    }

    fn quasiquote(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Quasiquote => {
                self.leaf(Kind::Quasiquote)
            },
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "quasiquote" })),
        }
    }

    fn comma(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Comma => {
                self.leaf(Kind::Comma)
            },
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "comma" })),
        }
    }

    fn comma_at(&mut self) -> ParseResult {
        match self.peek()? {
            Token::CommaAt => {
                self.leaf(Kind::CommaAt)
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
            Token::Identifier(_) => self.leaf(Kind::Identifier),
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "identifier" })),
        }        
    }

    fn identifier_list(&mut self) -> ParseResult {
        match self.peek()? {
            Token::ParenClose => self.node(Kind::List, vec!()), // empty list
            Token::Identifier(_) => self.identifier().and_then(|first| // non-empty list so get first identifier
                                        self.identifier_sequence().and_then(|identifier_sequence| // get rest of identifier sequence
                                                match self.peek()? {
                                                    Token::ParenClose => self.node(Kind::List, once(first).chain(identifier_sequence).collect()),
                                                    Token::Dot => self.dot().and_then(|_|
                                                                    self.identifier().and_then(|last|
                                                                        self.node(
                                                                            Kind::List, 
                                                                            once(first)
                                                                            .chain(identifier_sequence)
                                                                            .chain(once(last))
                                                                            .collect()))),
                                                    t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "close parenthesis or dot" })),
                                                })),
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "close parenthesis or dot" })),
        }   
    }
    
    fn identifier_sequence(&mut self) -> Result<Vec<Box<Node>>, Error> {
        Ok(from_fn(move || self.identifier().ok()).collect())
    }

    fn boolean(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Boolean(_) => self.leaf(Kind::Literal),
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "boolean" })),
        }
    }

    fn character(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Character(_) => self.leaf(Kind::Literal),
           t @  _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "character" })),
        }
    }

    fn string(&mut self) -> ParseResult {
        match self.peek()? {
            Token::String(_) => self.leaf(Kind::Literal),
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "string" })),
        }
    }

    fn number(&mut self) -> ParseResult {
        match self.peek()? {
            Token::Number(_) => self.leaf(Kind::Literal),
            t @ _ => Err(Error::new(ErrorKind::UnexpectedToken { unexpected: t.clone(), expected: "number" })),
        }
    }

    fn vector(&mut self) -> ParseResult {
        self.sharpopen().and_then(|_| 
            self.datum_list().and_then(|data| 
                self.paren_close().and_then(|_| 
                    self.node(Kind::Vector, data))))
    }

    fn bytevector(&mut self) -> ParseResult {
        self.sharpu8open().and_then(|_| 
            self.datum_list().and_then(|data| 
                self.paren_close().and_then(|_| 
                    self.node(Kind::Bytevector, data))))
    }

    /// Implements '<datum> (i.e. single quote followed by datum)
    fn quotation_apostrophe(&mut self) -> ParseResult {
        self.quote().and_then(
            |_| self.datum().and_then(
                |expr| self.node(Kind::Quotation, vec![expr])
            )
        )
    }

}
