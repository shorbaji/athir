use crate::{value::number::Number, alloc::R};

#[derive(Debug)]
pub struct Transformer {
    identifier: String,
    identifiers: Vec<String>,
    rules: Vec<SyntaxRule>,
}

#[derive(Debug)]
pub struct SyntaxRule {
    pattern: Pattern,
    template: Template,
}

#[derive(Debug)]
pub enum Pattern {
    Identifier(String),
    Underscore,
    PatternList(Vec<Box<Pattern>>),
    PatterEllipsis(Vec<Box<Pattern>>, Vec<Box<Pattern>>),
    PatternVector(Vec<Box<Pattern>>),
    PatternVectorEllipsis(Vec<Box<Pattern>>, Vec<Box<Pattern>>),
    PatternString(String),
    PatternChar(char),
    PatternBoolean(bool),
    PatternNumber(Number),
}

#[derive(Debug)]
pub enum Template {
    Identifier(String),
    TemplateElementList(Vec<TemplateElement>),
    TemplateElementVector(Vec<TemplateElement>),
    TemplateString(String),
    TemplateChar(char),
    TemplateBoolean(bool),
    TemplateNumber(Number),
}

#[derive(Debug)]
pub enum TemplateElement {
    Template(Box<Template>),
    TemplateEllipsis(Box<Template>),
}


impl Transformer {
    pub fn new(identifier: String, identifiers: Vec<String>, rules: Vec<SyntaxRule>) -> Self {
        Self { identifier, identifiers, rules }
    }

    pub fn expand(&self, e: &R) -> R {
        e.clone()
    }
}

impl Pattern {
    pub fn is_match(&self, e: &R) -> bool {
        panic!("pattern match not implemented!")
    }
}

impl Template {
    pub fn new_identifier(s: String) -> Self {
        Self::Identifier(s)
    }

    pub fn new_template_element_list(v: Vec<TemplateElement>) -> Self {
        Self::TemplateElementList(v)
    }

    pub fn new_template_element_vector(v: Vec<TemplateElement>) -> Self {
        Self::TemplateElementVector(v)
    }

    pub fn new_template_string(s: String) -> Self {
        Self::TemplateString(s)
    }

    pub fn new_template_char(c: char) -> Self {
        Self::TemplateChar(c)
    }

    pub fn new_template_boolean(b: bool) -> Self {
        Self::TemplateBoolean(b)
    }

    pub fn new_template_number(n: Number) -> Self {
        Self::TemplateNumber(n)
    }

    pub fn transcribe(&self, e: &R) -> R {
        e.clone()
    }
}