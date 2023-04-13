use crate::lexer::Token;

#[derive(Debug)]
pub enum Kind {
    Expression,
    ProcedureCall,
    Operands,
    Operator,
    VariableDefinition,
    FunctionDefinition,
    Conditional,
    Lambda,
    Body,
    BodyDefinitions,
    BodyExpressions,
    Assignment,
    Literal,
    Identifier,
    Formals,
    DefFormals,
    Quotation,
    Datum,
    List,
    Vector,
    Symbol,
    Abbreviation,
    AbbreviationPrefix,
    MacroBlock,
    LetRecSyntax,
    LetSyntax,
    SyntaxSpecList,
    SyntaxSpec,
    TransformerSpec,
    TransformerSpecIdentifierList,
    SyntaxRule,
    SyntaxRuleList,
    SyntaxRuleUnderscore,
    Pattern,
    PatternIdentifier,
    PatternDatum,
    PatternWithParen,
    PatternPreEllipse,
    PatternPostEllipse,
    PatternSharp,
    PatternParen,
    Template,
    TemplateElement,
    TemplateDatum,
    TemplateSharp,
    TemplateWithParen,
    Ellipsis,
    Underscore,
}

#[derive(Debug)]
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
}

#[derive(Debug)]
pub struct ParseTree {
    pub root: Box<Node>
}