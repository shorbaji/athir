//! Node
//!

use crate::read::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum NodeKind {
    Comma,
    CommaAt,
    Dot,
    ParenClose,
    ParenOpen,
    Quasiquote,
    Quote,
    SharpOpen,
    SharpU8Open,

    ByteVector,
    Vector,
    Datum,
    Symbol,

    Literal,
    Identifier,

    List,

    Assignment,
    Begin(bool),
    Conditional,
    Define,
    DefineFunction,
    DefineLibrary,
    DefineRecordType,
    DefineValues,
    DefineSyntax,
    Includer,
    Lambda,
    ProcedureCall,
    Quasiquotation(u32),
    Quotation,
    Unquotation(u32),    
    MacroBlock,
}

#[derive(Debug, Clone)]
pub enum Node {
    Inner(NodeKind, Vec<Box<Node>>),
    Leaf(NodeKind, Token),
}

impl Node {
    pub fn kind(&self) -> &NodeKind {
        match self {
            Node::Inner(kind, _) => kind,
            Node::Leaf(kind, _) => kind,
        }
    }

    pub fn children(&self) -> Option<&Vec<Box<Node>>> {
        match self {
            Node::Inner(_, children) => Some(children),
            _ => None,
        }
    }

    pub fn is_definition_expr(&self) -> bool {
        matches!(
            self.kind(),
            NodeKind::Define
            | NodeKind::DefineFunction
            | NodeKind::DefineSyntax
            | NodeKind::Begin(true)
            | NodeKind::DefineValues
            | NodeKind::DefineRecordType
        ) 
    }
}

impl From<&Token> for NodeKind {
    fn from(value: &Token) -> Self {
        match value {
            Token::Boolean(_) => NodeKind::Literal,
            Token::Character(_) => NodeKind::Literal,
            Token::String(_) => NodeKind::Literal,
            Token::Number(_) => NodeKind::Literal,
            Token::Identifier(_) => NodeKind::Identifier,
            Token::Comma => NodeKind::Comma,
            Token::CommaAt => NodeKind::CommaAt,
            Token::Dot => NodeKind::Dot,
            Token::ParenClose => NodeKind::ParenClose,
            Token::ParenOpen => NodeKind::ParenOpen,
            Token::Quasiquote => NodeKind::Quasiquote,
            Token::Quote => NodeKind::Quote,
            Token::SharpOpen => NodeKind::SharpOpen,
            Token::SharpU8Open => NodeKind::SharpU8Open,
            _ => panic!("Cannot convert {:?} to NodeKind", value),
        }
    }
}
