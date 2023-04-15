use crate::read::lexer::Token;

#[derive(Debug, Clone)]
pub enum Kind {
    Comma,
    CommaAt,
    Quasiquote,
    Quote,

    Bytevector,
    Vector,
    Datum,
    Symbol,

    Program,

    Literal,
    Identifier,
    List,

    Assignment,
    Begin,
    BeginDef,
    Conditional,
    FunctionDefinition,
    Lambda,
    RecordTypeDefinition,
    VariableDefinition,
    ValuesDefinition,
    ProcedureCall,
    Quotation,

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
    Inner(Kind, Vec<Box<Node>>),
    Leaf(Kind, Token),
}

impl Node {
    pub fn add_child(&mut self, child: Box<Node>) {
        match self {
            Node::Inner(_, children) => children.push(child),
            Node::Leaf(_, _) => panic!("Cannot add child to leaf node"),
        }
    }

    pub fn kind(&self) -> &Kind {
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
        matches!(self.kind(), Kind::VariableDefinition | Kind::FunctionDefinition | Kind::SyntaxDefinition | Kind::BeginDef)
    }

}
