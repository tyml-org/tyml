use std::{borrow::Cow, ops::Range};

use allocator_api2::vec::Vec;
use bumpalo::Bump;
use error::GeneratedParseError;
use tyml_validate::validate::ValueTypeChecker;

use crate::lexer::{GeneratorLexer, GeneratorTokenKind, TokenizerRegistry};

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

    fn first_token_kind(&self) -> GeneratorTokenKind;
}

pub trait AST<'input> {
    /// UTF-8 byte span
    fn span(&self) -> Range<usize>;

    fn take_value(
        &self,
        section_name_stack: &mut Vec<(Cow<'input, str>, Range<usize>), &Bump>,
        validator: &mut ValueTypeChecker<'_, '_, '_, '_, 'input, 'input>,
    );
}

pub trait ParserPart {
    fn parse_error_code(&self) -> usize;

    fn expected_format(&self) -> Option<Cow<'static, str>>;
}

pub struct NamedParserPart {
    pub parse_error_code: usize,
    pub expected_format: &'static str,
}

impl NamedParserPart {
    pub const LINE_FEED: Self = Self {
        parse_error_code: 0005,
        expected_format: "expected.format.line_feed",
    };
    pub const KEY_VALUE_COLON: Self = Self {
        parse_error_code: 0006,
        expected_format: "expected.format.colon_on_key_value",
    };
    pub const KEY_VALUE_EQUAL: Self = Self {
        parse_error_code: 0007,
        expected_format: "expected.format.equal_on_key_value",
    };
}

impl ParserPart for NamedParserPart {
    fn parse_error_code(&self) -> usize {
        self.parse_error_code
    }

    fn expected_format(&self) -> Option<Cow<'static, str>> {
        Some(self.expected_format.into())
    }
}
