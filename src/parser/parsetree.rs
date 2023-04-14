use crate::lexer::Token;

#[derive(Debug)]
pub enum Kind {
    Begin,
    BeginDef,
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
    Symbol,
    Abbreviation,
    AbbreviationPrefix,
    MacroBlock,
    LetRecSyntax,
    LetSyntax,
    SyntaxDefinition,
    SyntaxSpecList,
    SyntaxSpec,
    TransformerSpec,
    TransformerSpecIdentifierList,
    SyntaxRule,
    SyntaxRuleList,
    PatternUnderscore,
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
    TemplateElementEllipsis,
    TemplateDatum,
    TemplateSharp,
    TemplateWithParen,
    Vector,
    Bytevector,
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

    pub fn is_definition(&self) -> bool {
        match self {
            Node::Inner(Kind::Expression, nodes) => 
                match *nodes[0] {
                    Node::Inner(Kind::VariableDefinition, _) 
                    | Node::Inner(Kind::FunctionDefinition, _) => true,
                    _ => false,
                },
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct ParseTree {
    pub root: Box<Node>
}