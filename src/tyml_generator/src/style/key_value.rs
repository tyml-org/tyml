use std::ops::Range;

use allocator_api2::vec::Vec;
use tyml_validate::validate::ValueTypeChecker;

use crate::lexer::GeneratorTokenKind;

use super::{
    AST, Parser, ParserGenerator,
    error::GeneratedParseError,
    literal::Literal,
    value::{Value, ValueParser},
};

#[derive(Debug)]
pub struct KeyValue {
    pub key: Literal,
    pub kind: KeyValueKind,
    pub value: Value,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KeyValueKind {
    /// key: value
    Colon,
    /// key = value
    Equal,
}

pub struct KeyValueParser {
    pub key: GeneratorTokenKind,
    pub kind: KeyValueKind,
    pub value: ValueParser,
}

pub struct KeyValueAST {}

impl<'input> ParserGenerator<'input, KeyValueAST, KeyValueParser> for KeyValue {
    fn generate(&self, registry: &mut crate::lexer::TokenizerRegistry) -> KeyValueParser {
        KeyValueParser {
            key: self.key.register(registry),
            kind: self.kind,
            value: self.value.generate(registry),
        }
    }
}

impl<'input> Parser<'input, KeyValueAST> for KeyValueParser {
    fn parse(
        &self,
        lexer: &mut crate::lexer::GeneratorLexer<'input, '_>,
        errors: &mut Vec<GeneratedParseError>,
    ) -> Option<KeyValueAST> {
        todo!()
    }

    fn first_token_kind(&self) -> GeneratorTokenKind {
        todo!()
    }

    fn expected_message_key(&self) -> std::borrow::Cow<'static, str> {
        "expected.key_value".into()
    }

    fn expected_format_key(&self) -> Option<std::borrow::Cow<'static, str>> {
        match self.kind {
            KeyValueKind::Colon => Some("key: value".into()),
            KeyValueKind::Equal => Some("key = value".into()),
        }
    }
}

impl<'input> AST<'input> for KeyValueAST {
    fn span() -> Range<usize> {
        todo!()
    }

    fn take_value(
        &self,
        validator: &mut ValueTypeChecker<'_, '_, '_, '_, 'input, 'input>,
        section_name_stack: &mut allocator_api2::vec::Vec<&'input str, &bumpalo::Bump>,
    ) {
        todo!()
    }
}
