use std::ops::Range;

use allocator_api2::vec::Vec;
use tyml_validate::validate::ValueTypeChecker;

use crate::lexer::{GeneratorTokenKind, GeneratorTokenizer, SpannedText};

use super::{
    AST, NamedParserPart, Parser, ParserGenerator, ParserPart,
    error::{GeneratedParseError, recover_until_or_lf},
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
    /// key value
    NoSeparator,
}

pub struct KeyValueParser {
    pub key: GeneratorTokenKind,
    pub kind: Option<GeneratorTokenKind>,
    pub parser_kind: KeyValueKind,
    pub value: ValueParser,
}

#[derive(Debug)]
pub struct KeyValueAST<'input> {
    pub key: SpannedText<'input>,
    pub value: Option<SpannedText<'input>>,
}

impl<'input> ParserGenerator<'input, KeyValueAST<'input>, KeyValueParser> for KeyValue {
    fn generate(&self, registry: &mut crate::lexer::TokenizerRegistry) -> KeyValueParser {
        let kind = match self.kind {
            KeyValueKind::Colon => Some(registry.register(GeneratorTokenizer::Keyword(":".into()))),
            KeyValueKind::Equal => Some(registry.register(GeneratorTokenizer::Keyword("=".into()))),
            KeyValueKind::NoSeparator => None,
        };

        KeyValueParser {
            key: self.key.register(registry),
            kind,
            parser_kind: self.kind,
            value: self.value.generate(registry),
        }
    }
}

impl<'input> Parser<'input, KeyValueAST<'input>> for KeyValueParser {
    fn parse(
        &self,
        lexer: &mut crate::lexer::GeneratorLexer<'input, '_>,
        errors: &mut Vec<GeneratedParseError>,
    ) -> Option<KeyValueAST<'input>> {
        let key = match lexer.current_contains(self.key) {
            true => lexer.next().unwrap().into_spanned(),
            false => return None,
        };

        if let Some(kind) = self.kind {
            if !lexer.current_contains(kind) {
                let parser_part = match self.parser_kind {
                    KeyValueKind::Colon => NamedParserPart::KEY_VALUE_COLON,
                    KeyValueKind::Equal => NamedParserPart::KEY_VALUE_EQUAL,
                    KeyValueKind::NoSeparator => unreachable!(),
                };

                let error = recover_until_or_lf(lexer, &[], &parser_part);
                errors.push(error);

                return Some(KeyValueAST { key, value: None });
            }
        }
        lexer.next();

        let value = match lexer.current_contains(self.value.first_token_kind()) {
            true => Some(lexer.next().unwrap().into_spanned()),
            false => {
                let error = recover_until_or_lf(lexer, &[], &NamedParserPart::VALUE);
                errors.push(error);

                None
            }
        };

        Some(KeyValueAST { key, value })
    }

    fn first_token_kind(&self) -> GeneratorTokenKind {
        self.key
    }
}

impl ParserPart for KeyValueParser {
    fn expected_message_key(&self) -> std::borrow::Cow<'static, str> {
        "expected.message.key_value".into()
    }

    fn expected_format_key(&self) -> Option<std::borrow::Cow<'static, str>> {
        match self.parser_kind {
            KeyValueKind::Colon => Some("expected.format.key_value_colon".into()),
            KeyValueKind::Equal => Some("expected.format.key_value_equal".into()),
            KeyValueKind::NoSeparator => Some("expected.format.key_value_none".into()),
        }
    }
}

impl<'input> AST<'input> for KeyValueAST<'input> {
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
