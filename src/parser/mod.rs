//! Athir parser module
//!
//! TODO:
//! [P0]
//! - formals with a dot
//! - def formals with a dot
//! - begin
//! - define-values
//! - define-record-type
//! 
//! [P1]
//! - code for handling zero-or-one - zero-or-more - one-or-more
//! - transformers
//! - define-syntax
//! 
//! [P2]
//! - includer
//! - define-library
//! - derived-expression
//! 
//! Known issues:
//! - The parser is not yet complete.
//! - No support for # label in datum.
//! - Transformer spec implementation is incomplete.
//! - internal definitions in body not implemented yet
//! 
//! 

#[cfg(test)]
mod tests;
mod parsetree;

use std::iter::{from_fn, Peekable};

use crate::lexer::Lexer;
use crate::lexer::Token;
use crate::parser::parsetree::{Kind, Node, ParseTree};

type ParseError = &'static str;

/// Implements recursive descent parser and uses peekable lexer iterator 
/// to implement LL(1) lookahead
pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { 
            lexer: Lexer::new(input).peekable() 
        }
    }
    
    pub fn parse(&mut self) -> Result<ParseTree, ParseError> {
        self.expr()
        .and_then(|root| Ok(ParseTree { root: root}))
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
    fn expr(&mut self) -> Result<Box<Node>, ParseError> {        
        let node = match self.peek().ok_or("Error at end of input")? {
            Token::Boolean(_)
            | Token::Character(_)
            | Token::String(_)
            | Token::Number(_) 
            | Token::Vector
            | Token::ByteVector => self.leaf(Kind::Literal),
            Token::Quote => self.abbreviated_quotation(),
            Token::Identifier(_) => self.leaf(Kind::Identifier),
            Token::ParenOpen => self.compound_expr(),
            _ => Err("Unexpected token at beginning of expression"),
        }?;
        
        self.node(Kind::Expression, vec![node])
        
    }
    
    fn abbreviated_quotation(&mut self) -> Result<Box<Node>, ParseError> {
        // Implements '<datum> (i.e. single quote followed by datum)
        self.quote().and_then(
            |_| self.datum().and_then(
                |expr| self.node(Kind::Quotation, vec![expr])
            )
        )
    }
    
    fn compound_expr(&mut self) -> Result<Box<Node>, ParseError> {
        // 
        // this function handles a non-atomic expression 
        // starting with a parenthesis we then first look for keywords
        // if not keyword found we fall back to a procedure call
        // 
        self.paren_open().and_then(
            |_| match self.peek().ok_or("error at end of input")? {
                Token::Identifier(id) => 
                    match id.as_str() {
                        "define" => self.definition(),
                        "if" => self.conditional_if(),
                        "lambda" => self.lambda(),
                        "quote" => self.quotation(),
                        "set!" => self.assignment(),
                        "let-syntax" | "letrec-syntax" => self.macro_block(),
                        _ => self.procedure_call(),
                    },
                _ => self.procedure_call(),
            },
        )
    }
    
    ///
    /// Definitions
    /// 
    /// We start with the define keyword and then look for either 
    /// - an identifier (i.e. a variable definition) or,
    /// - a parenthesized list of identifiers (i.e. a function definition)
    /// 

    fn definition(&mut self) -> Result<Box<Node>, ParseError> {
        self.keyword("define").and_then(|_| 
            match self.peek().ok_or("error at end of input")? {
                Token::Identifier(_) => self.variable_definition(),
                Token::ParenOpen => self.function_definition(),
                _ => Err("Unexpected token. Expected identifier or open paren"),
            },
        )
    }
        
    fn variable_definition(&mut self) -> Result<Box<Node>, ParseError> {
        self.identifier().and_then(
            |id| self.expr().and_then(
                |expr| self.paren_close().and_then(
                    |_| self.node(Kind::VariableDefinition, vec![id, expr])
                )
            )
        )
    }
        
    fn function_definition(&mut self) -> Result<Box<Node>, ParseError> {
        self.paren_open().and_then(|_| 
            self.identifier().and_then(|id| 
                self.def_formals().and_then(|formals| 
                    self.paren_close().and_then(|_| 
                        self.body().and_then(|body| 
                            self.paren_close().and_then(|_| 
                                self.node(Kind::FunctionDefinition, vec![id, formals, body])))))))
    } 

    fn def_formals(&mut self) -> Result<Box<Node>, ParseError> {
        self.identifier_list().and_then(|ids| self.node(Kind::DefFormals, ids))
    }

    ///
    /// Conditionals
    /// 
    /// 
    ///
    fn conditional_if(&mut self) -> Result<Box<Node>, ParseError> {
        self.keyword("if").and_then(
            |_| self.expr().and_then(
                |test| self.expr().and_then(
                    |consequent| 
                    match self.peek().ok_or("Error at end of input")? {
                        
                        Token::ParenClose => {
                            self.lexer.next(); // consume ParenClose
                            return self.node(Kind::Conditional, vec![test, consequent])
                        },
                        
                        _ => self.expr().and_then(|alternative| 
                            self.paren_close()
                            .or(Err("Expected ParenClose or Expr."))
                            .and_then(|_| self.node(Kind::Conditional, vec![test, consequent, alternative]))),
                        
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

    fn lambda(&mut self) -> Result<Box<Node>, ParseError> {
        self.keyword("lambda").and_then(|_| 
            self.formals().and_then(|formals|
                self.body().and_then(|body| 
                    self.paren_close().and_then(|_| 
                        self.node(Kind::Lambda, vec![formals, body])))))
    }

    fn formals(&mut self) -> Result<Box<Node>, ParseError> {
        self.identifier().and_then(|id|
            self.node(Kind::Formals, vec![id]))
            .or_else(|_|
                self.paren_open().and_then(|_|
                    self.identifier_list().and_then(|ids|
                        self.paren_close().and_then(|_|
                            self.node(Kind::Formals, ids)))))
     }

    fn body(&mut self) -> Result<Box<Node>, ParseError> {
        self.body_definitions().and_then(|defs|
            self.body_expressions().and_then(|exprs| 
                self.node(Kind::Body, vec![defs, exprs])))
    }

    fn body_definitions(&mut self) -> Result<Box<Node>, ParseError> {
        Ok(from_fn(|| self.definition().ok()).collect::<Vec<Box<Node>>>())
            .and_then(|defs| self.node(Kind::BodyDefinitions, defs))
    }

    fn body_expressions(&mut self) -> Result<Box<Node>, ParseError> {
        Ok(from_fn(|| self.expr().ok()).collect::<Vec<Box<Node>>>())
            .and_then(|exprs| if exprs.is_empty() {
                    Err("Expected at least one expression in body")
                } else {
                    self.node(Kind::BodyExpressions, exprs)
                })
    }

    ///
    /// Quotations
    /// 
    /// 
    ///

    fn quotation(&mut self) -> Result<Box<Node>, ParseError> {
        self.keyword("quote").and_then(|_| 
            self.datum().and_then(|datum|
                self.paren_close().and_then(|_|
                    self.node(Kind::Quotation, vec![datum]))))
    }

    ///
    /// Assignments
    /// 
    /// 
    ///

    fn assignment(&mut self) -> Result<Box<Node>, ParseError> {
        self.keyword("set!").and_then(|_|
            self.identifier().and_then(|id| 
                self.expr().and_then(|expr| 
                    self.paren_close().and_then(|_| 
                        self.node(Kind::Assignment, vec![id, expr])))))        
    }


    ///
    /// Procedure calls
    ///
    ///
    /// 

    fn procedure_call(&mut self) -> Result<Box<Node>, ParseError> {
        self.operator().and_then(|operator| 
            self.operands().and_then(|operands|
                self.paren_close().and_then(|_| {
                    self.node(Kind::ProcedureCall, vec![operator, operands])    
                })))
    }

    fn operator(&mut self) -> Result<Box<Node>, ParseError> {
        self.expr().and_then(|expr|
            self.node(Kind::Operator, vec![expr]))
    }

    fn operands(&mut self) -> Result<Box<Node>, ParseError> {
        self.expr_list().and_then(|exprs|
            self.node(Kind::Operands, exprs))
    }

    ///
    /// Macro blocks
    /// 
    /// 
    ///

    fn macro_block(&mut self) -> Result<Box<Node>, ParseError> {
        let node = match self.peek().ok_or("error at end of input")? {
            Token::Identifier(id) => 
                match id.as_str() {
                    "let-syntax" => self.let_syntax(),
                    "letrec-syntax" => self.letrec_syntax(),
                    _ => Err("Unexpected token. Expected let-syntax or letrec-syntax"),
                },
            _ => Err("Unexpected token. Expected let-syntax or letrec-syntax"),
        }?;

        self.node(Kind::MacroBlock, vec![node])
    }

    fn let_syntax(&mut self) -> Result<Box<Node>, ParseError> {
        self.keyword("let-syntax").and_then(|_| 
            self.macro_block_suffix().and_then(|(transformation_specs, body)|
                self.node(Kind::LetSyntax, vec![transformation_specs, body])))
    }

    fn letrec_syntax(&mut self) -> Result<Box<Node>, ParseError> {
        println!("letrec_syntax {:?}", self.peek());
        self.keyword("letrec-syntax").and_then(|_| 
            self.macro_block_suffix().and_then(|(transformation_specs, body)|
                self.node(Kind::LetSyntax, vec![transformation_specs, body])))
    }

    fn macro_block_suffix(&mut self) -> Result<(Box<Node>, Box<Node>), ParseError> {
        self.transformation_specs().and_then(|transformation_specs|
            self.body().and_then(|body|
                self.paren_close().and_then(|_|
                Ok((transformation_specs, body)))))
    }

    ///
    /// Transformer (R7RS section 7.1.5) 
    /// [INCOMPLETE]
    /// 
    fn transformation_specs(&mut self) -> Result<Box<Node>, ParseError> {
        self.paren_open().and_then(|_|
            self.transformation_spec_list().and_then(|transformation_specs|
                self.paren_close().and_then(|_|
                    self.node(Kind::TransformationSpecList, transformation_specs))))
    }

    fn transformation_spec_list(&mut self) -> Result<Vec<Box<Node>>, ParseError> {
        Ok(from_fn(|| self.transformation_spec().ok()).collect())
    }

    fn transformation_spec(&mut self) -> Result<Box<Node>, ParseError> {
        self.paren_open().and_then(|_|
            self.keyword("syntax_rules").and_then(|_| 
                self.transformation_spec_optional_identifer().and_then(|id|
                    self.transformation_spec_suffix().and_then(|(ids, syntax_rules)|
                        self.paren_close().and_then(|_|
                            self.node(Kind::TransformationSpec, vec![id, ids, syntax_rules]))))))
    }

    fn transformation_spec_suffix(&mut self) -> Result<(Box<Node>, Box<Node>), ParseError> {
        self.paren_open().and_then(|_|
            self.transformation_spec_identifier_list().and_then(|transformation_spec_identifier_list|
                self.syntax_rule_list().and_then(|syntax_rule_list|
                    self.paren_close().and_then(|_| 
                        Ok((transformation_spec_identifier_list, syntax_rule_list))))))
    }

    fn transformation_spec_optional_identifer(&mut self) -> Result<Box<Node>, ParseError> {
        self.identifier().and_then(|id|
            self.node(Kind::TransformationSpecOptionalIdentifier, vec![id]))
            .or_else(|_| self.node(Kind::TransformationSpecOptionalIdentifier, vec!()))
    }

    fn transformation_spec_identifier_list(&mut self) -> Result<Box<Node>, ParseError> {
        Ok(from_fn(|| self.identifier().ok()).collect::<Vec<Box<Node>>>())
            .and_then(|ids| self.node(Kind::TransformationSpecIdentifierList, ids))
    }

    fn syntax_rule_list(&mut self) -> Result<Box<Node>, ParseError> {
        Ok(from_fn(|| self.syntax_rule().ok()).collect::<Vec<Box<Node>>>())
            .and_then(|syntax_rules| self.node(Kind::SyntaxRuleList, syntax_rules))
    }

    fn syntax_rule(&mut self) -> Result<Box<Node>, ParseError> {
        self.paren_open().and_then(|_|
            self.pattern().and_then(|pattern|
                self.template().and_then(|template|
                    self.paren_close().and_then(|_|
                        self.node(Kind::SyntaxRule, vec![pattern, template])))))
    }

    fn pattern(&mut self) -> Result<Box<Node>, ParseError> {
        self.pattern_identifier()
        .or_else(|_| self.underscore())
        .and_then(|pattern| self.node(Kind::Pattern, vec![pattern]))
    }

    fn pattern_identifier(&mut self) -> Result<Box<Node>, ParseError> {
        let token = self.peek().ok_or("Unexpected end of input")?;

        match token {
            Token::Identifier(s) if *s == "...".to_string() => Err("... is not a valid pattern identifier"),
            _ => self.identifier().and_then(|id| self.node(Kind::PatternIdentifier, vec![id])),
        }
    }

    fn underscore(&mut self) -> Result<Box<Node>, ParseError> {
        self.keyword("_").and_then(|_| 
            self.node(Kind::SyntaxRuleUnderscore, vec!()))
    }

    fn template(&mut self) -> Result<Box<Node>, ParseError> {
        self.pattern_identifier()
    }

    ///
    /// Datum (R7RS section 7.1.3 - External Representation)
    /// 
    /// 
    /// 
    fn datum(&mut self) -> Result<Box<Node>, ParseError> {
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

    fn symbol(&mut self) -> Result<Box<Node>, ParseError> {
        match self.peek().ok_or("Error at end of input")? {
            Token::Identifier(_) => self.leaf(Kind::Symbol),
            _ => return Err("Unexpected token. Expected symbol"),
        }
    }

    fn abbreviation(&mut self) -> Result<Box<Node>, ParseError> {
        self.quote()
        .or_else(|_| self.quasiquote())
        .or_else(|_| self.comma())
        .or_else(|_| self.comma_at())
        .map(|_| self.leaf(Kind::AbbreviationPrefix))
        .and_then(|prefix| 
            self.datum().and_then(|expr| 
                self.node(Kind::Abbreviation, vec![prefix?, expr])))
    }

    fn list(&mut self) -> Result<Box<Node>, ParseError> {
        self.paren_open().and_then(|_| 
            self.datum_list().and_then(|data| 
                self.paren_close().and_then(|_| 
                    self.node(Kind::List, data))))
    }

    fn datum_list(&mut self) -> Result<Vec<Box<Node>>, ParseError> {
        Ok(from_fn(|| self.datum().ok()).collect())
    }


    //
    // Helpers
    //
    //
    //

    fn peek(&mut self) -> Option<&Token> {
        self.lexer.peek()
    }
    
    fn node(&mut self, kind: Kind, children: Vec<Box<Node>>) -> Result<Box<Node>, ParseError> {
        Ok(Box::new(Node::Inner(kind, children)))
    }
    
    fn leaf(&mut self, kind: Kind) -> Result<Box<Node>, ParseError> {
        Ok(Box::new(Node::Leaf(kind, self.lexer.next().unwrap())))
    }

    fn keyword(&mut self, keyword: &str) -> Result<(), ParseError> {
        let token = self.peek().ok_or("Error at end of input")?;
        
        match token {
            Token::Identifier(s) if keyword == s => {
                self.lexer.next(); // consume keyword
                Ok(())
            },
            _ => Err("Unexpected token. Expected keyword"),
        }
        
    }
    
    fn quote(&mut self) -> Result<(), ParseError> {
        match self.peek().ok_or("Error at end of input")? {
            Token::Quote => {
                self.lexer.next(); // consume Quote
                Ok(())
            },
            _ => Err("syntax error: expected Quote"),
        }
    }

    fn quasiquote(&mut self) -> Result<(), ParseError> {
        match self.peek().ok_or("Error at end of input")? {
            Token::Quasiquote => {
                self.lexer.next(); // consume Quasiquote
                Ok(())
            },
            _ => Err("syntax error: expected Quasiquote"),
        }
    }

    fn comma(&mut self) -> Result<(), ParseError> {
        match self.peek().ok_or("Error at end of input")? {
            Token::Comma => {
                self.lexer.next(); // consume Comma
                Ok(())
            },
            _ => Err("syntax error: expected Comma"),
        }
    }

    fn comma_at(&mut self) -> Result<(), ParseError> {
        match self.peek().ok_or("Error at end of input")? {
            Token::CommaAt => {
                self.lexer.next(); // consume CommaAt
                Ok(())
            },
            _ => Err("syntax error: expected CommaAt"),
        }
    }

    fn paren_open(&mut self) -> Result<(), ParseError> {
        match self.peek().ok_or("Error at end of input")? {
            Token::ParenOpen => {
                self.lexer.next(); // consume ParenOpen
                Ok(())
            },
            _ => Err("syntax error: expected ParenOpen"),
        }
    }
    
    fn paren_close(&mut self) -> Result<(), ParseError> {
        match self.peek().ok_or("Error at end of input")? {
            Token::ParenClose => {
                self.lexer.next(); // consume ParenClose
                Ok(())
            },
            _ => Err("syntax error: expected ParenClose"),
        }
    }

    fn sharpopen(&mut self) -> Result<(), ParseError> {
        match self.peek().ok_or("Error at end of input")? {
            Token::SharpOpen => {
                self.lexer.next(); // consume SharpOpen
                Ok(())
            },
            _ => Err("syntax error: expected SharpOpen"),
        }
    }

    fn expr_list(&mut self) -> Result<Vec<Box<Node>>, ParseError> {
        Ok(from_fn(|| self.expr().ok()).collect())
    }
    
    fn identifier(&mut self) -> Result<Box<Node>, ParseError> {        
        match self.peek().ok_or("Error at end of input")? {
            Token::Identifier(_) => self.leaf(Kind::Identifier),
            _ => Err("Unexpected token. Expected identifier"),
        }        
    }

    fn identifier_list(&mut self) -> Result<Vec<Box<Node>>, ParseError> {
        Ok(from_fn(|| self.identifier().ok()).collect())
    }
        
    fn boolean(&mut self) -> Result<Box<Node>, ParseError> {
        match self.peek().ok_or("Error at end of input")? {
            Token::Boolean(_) => self.leaf(Kind::Literal),
            _ => return Err("Unexpected token. Expected boolean"),
        }
    }

    fn character(&mut self) -> Result<Box<Node>, ParseError> {
        match self.peek().ok_or("Error at end of input")? {
            Token::Character(_) => self.leaf(Kind::Literal),
            _ => return Err("Unexpected token. Expected character"),
        }
    }

    fn string(&mut self) -> Result<Box<Node>, ParseError> {
        match self.peek().ok_or("Error at end of input")? {
            Token::String(_) => self.leaf(Kind::Literal),
            _ => return Err("Unexpected token. Expected string"),
        }
    }

    fn number(&mut self) -> Result<Box<Node>, ParseError> {
        match self.peek().ok_or("Error at end of input")? {
            Token::Number(_) => self.leaf(Kind::Literal),
            _ => return Err("Unexpected token. Expected number"),
        }
    }

    fn vector(&mut self) -> Result<Box<Node>, ParseError> {
        self.sharpopen().and_then(|_| 
            self.paren_open().and_then(|_|
                self.datum_list().and_then(|data| 
                    self.paren_close().and_then(|_| 
                        self.node(Kind::Vector, data)))))
    }

}
