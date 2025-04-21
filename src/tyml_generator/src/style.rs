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
    fn span() -> Range<usize>;

    fn take_value(
        &self,
        validator: &mut ValueTypeChecker<'_, '_, '_, '_, 'input, 'input>,
        section_name_stack: &mut Vec<&'input str, &Bump>,
    );
}

pub trait ParserPart {
    fn expected_message_key(&self) -> Cow<'static, str>;

    fn expected_format_key(&self) -> Option<Cow<'static, str>>;
}

pub struct NamedParserPart {
    pub expected_message_key: &'static str,
    pub expected_format_key: &'static str,
}

impl NamedParserPart {
    pub const LINE_FEED: Self = Self {
        expected_message_key: "expected.message.line_feed",
        expected_format_key: "expected.format.line_feed",
    };
    pub const KEY_VALUE_COLON: Self = Self {
        expected_message_key: "expected.message.colon_on_key_value",
        expected_format_key: "expected.format.colon_on_key_value",
    };
    pub const KEY_VALUE_EQUAL: Self = Self {
        expected_message_key: "expected.message.equal_on_key_value",
        expected_format_key: "expected.format.equal_on_key_value",
    };
    pub const VALUE: Self = Self {
        expected_message_key: "expected.message.value",
        expected_format_key: "expected.format.value",
    };
}

impl ParserPart for NamedParserPart {
    fn expected_message_key(&self) -> Cow<'static, str> {
        self.expected_message_key.into()
    }

    fn expected_format_key(&self) -> Option<Cow<'static, str>> {
        Some(self.expected_format_key.into())
    }
}
