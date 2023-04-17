//! Node
//!

use crate::read::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum NodeKind {
    Comma,
    CommaAt,
    Quasiquote,
    Quote,

    ByteVector,
    Vector,
    Datum,
    Symbol,

    Literal,
    Identifier,
    List,

    Assignment,
    Begin,
    BeginDef,
    Conditional,
    FunctionDefinition,
    Includer,
    IncluderCI,
    Lambda,
    ProcedureCall,
    Quasiquotation(u32),
    Quotation,
    Unquotation(u32),
    RecordTypeDefinition,
    VariableDefinition,
    ValuesDefinition,

    LetRecSyntax,
    LetSyntax,
    MacroBlock,
    Pattern,
    PatternDatum,
    PatternIdentifier,
    PatternParen,
    PatternPostEllipse,
    PatternPreEllipse,
    PatternSharp,
    PatternUnderscore,
    PatternWithParen,
    SyntaxDefinition,
    SyntaxRule,
    SyntaxRuleList,
    SyntaxSpec,
    SyntaxSpecList,
    Template,
    TemplateDatum,
    TemplateElement,
    TemplateElementEllipsis,
    TemplateSharp,
    TemplateWithParen,
    TransformerSpec,
    TransformerSpecIdentifierList,
}

#[derive(Debug, Clone)]
pub enum Node {
    Inner(NodeKind, Vec<Box<Node>>),
    Leaf(NodeKind, Token),
}

impl Node {
    pub fn add_child(&mut self, child: Box<Node>) {
        match self {
            Node::Inner(_, children) => children.push(child),
            Node::Leaf(_, _) => panic!("Cannot add child to leaf node"),
        }
    }

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
            NodeKind::VariableDefinition
            | NodeKind::FunctionDefinition
            | NodeKind::SyntaxDefinition
            | NodeKind::BeginDef
            | NodeKind::ValuesDefinition
            | NodeKind::RecordTypeDefinition
        ) 
    }
}
