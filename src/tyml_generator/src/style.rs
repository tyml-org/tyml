use std::{
    borrow::Cow,
    collections::{BTreeMap, HashMap},
    ops::Range,
};

use allocator_api2::vec::Vec;
use bumpalo::Bump;
use error::GeneratedParseError;
use tyml_formatter::{FormatterTokenKind, SpaceFormat};
use tyml_validate::validate::ValueTypeChecker;

use crate::lexer::{GeneratorLexer, GeneratorTokenKind, TokenizerRegistry};

pub mod comment;
pub mod error;
pub mod key_value;
pub mod language;
pub mod literal;
pub mod section;
pub mod value;

pub trait ParserGenerator<'input, T: AST<'input>, P: Parser<'input, T>> {
    fn generate(&self, registry: &mut TokenizerRegistry) -> P;
}

pub trait Parser<'input, T: AST<'input>>: ParserPart {
    fn parse(
        &self,
        lexer: &mut GeneratorLexer<'input, '_>,
        errors: &mut Vec<GeneratedParseError>,
    ) -> Option<T>;

    fn first_token_kinds(&self) -> impl Iterator<Item = GeneratorTokenKind>;

    fn map_formatter_token_kind(&self, map: &mut HashMap<GeneratorTokenKind, FormatterTokenKind>);
}

pub trait AST<'input> {
    /// UTF-8 byte span
    fn span(&self) -> Range<usize>;

    fn take_value(
        &self,
        section_name_stack: &mut Vec<(Cow<'input, str>, Range<usize>, Range<usize>, bool), &Bump>,
        validator: &mut ValueTypeChecker<'_, '_, '_, 'input, 'input>,
    );

    fn take_token(&self, tokens: &mut BTreeMap<usize, (ASTTokenKind, Range<usize>)>);

    fn take_formatter_token_space(&self, tokens: &mut Vec<FormatterTokenInfo>);
}

#[derive(Debug)]
pub struct FormatterTokenInfo {
    pub span: Range<usize>,
    pub left_space: SpaceFormat,
    pub right_space: SpaceFormat,
}

pub enum ASTTokenKind {
    Section,
    Key,
    TreeKey,
    NumericValue,
    InfNan,
    StringValue,
    BoolValue,
    Comment,
    Null,
}

pub trait ParserPart {
    fn parse_error_code(&self) -> usize;

    fn expected_format(&self) -> Option<Cow<'static, str>>;
}

pub struct NamedParserPart {
    pub parse_error_code: usize,
    pub expected_format: Option<&'static str>,
}

impl NamedParserPart {
    pub const LINE_FEED: Self = Self {
        parse_error_code: 0005,
        expected_format: None,
    };
    pub const KEY_VALUE_COLON: Self = Self {
        parse_error_code: 0006,
        expected_format: None,
    };
    pub const KEY_VALUE_EQUAL: Self = Self {
        parse_error_code: 0007,
        expected_format: None,
    };
    pub const BRACE_RIGHT: Self = Self {
        parse_error_code: 0010,
        expected_format: None,
    };
}

impl ParserPart for NamedParserPart {
    fn parse_error_code(&self) -> usize {
        self.parse_error_code
    }

    fn expected_format(&self) -> Option<Cow<'static, str>> {
        self.expected_format.map(|expected| expected.into())
    }
}
