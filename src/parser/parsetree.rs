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
    IdentifierList,
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
    TransformationSpecList,
    TransformationSpec,
    TransformationSpecOptionalIdentifier,
    TransformationSpecIdentifierList,
    SyntaxRule,
    SyntaxRuleList,
    SyntaxRuleUnderscore,
    Pattern,
    PatternIdentifier,
    PatternDatum,
    Template,
    TemplateElement,
    TemplateDatum,
    Ellipsis,
    Underscore,
}

#[derive(Debug)]
pub enum Node {
    Inner(Kind, Vec<Box<Node>>),
    Leaf(Kind, Token),
}

#[derive(Debug)]
pub struct ParseTree {
    pub root: Box<Node>
}