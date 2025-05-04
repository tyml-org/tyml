use std::{borrow::Cow, ops::Range};

use allocator_api2::vec::Vec;
use serde::{Deserialize, Serialize};
use tyml_source::AsUtf8ByteRange;
use tyml_validate::validate::{ValidateValue, ValueTree, ValueTypeChecker};

use crate::lexer::{GeneratorAnchor, GeneratorTokenKind, GeneratorTokenizer, SpannedText};

use super::{
    AST, ASTTokenKind, NamedParserPart, Parser, ParserGenerator, ParserPart,
    error::{GeneratedParseError, recover_until_or_lf},
    literal::{CustomLiteralOption, Literal},
    value::{Value, ValueAST, ValueParser},
};

#[derive(Debug, Serialize, Deserialize)]
pub struct KeyValue {
    pub key: Literal,
    pub kind: KeyValueKind,
    pub value: Value,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum KeyValueKind {
    /// key: value
    Colon,
    /// key = value
    Equal,
    /// key value
    NoSeparator,
}

pub struct KeyValueParser {
    pub key: (GeneratorTokenKind, Option<CustomLiteralOption>),
    pub kind: Option<GeneratorTokenKind>,
    pub parser_kind: KeyValueKind,
    pub value: ValueParser,
}

#[derive(Debug)]
pub struct KeyValueAST<'input> {
    pub key: SpannedText<'input>,
    pub key_literl_option: Option<CustomLiteralOption>,
    pub value: Option<ValueAST<'input>>,
    pub span: Range<usize>,
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
        let anchor = lexer.cast_anchor();

        let key = match lexer.current_contains(self.key.0) {
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

                // contains key span for error
                lexer.back_to_anchor(GeneratorAnchor {
                    byte_position: key.span.start,
                });

                let error = recover_until_or_lf(lexer, &[], &parser_part);
                errors.push(error);

                return Some(KeyValueAST {
                    key,
                    key_literl_option: self.key.1.clone(),
                    value: None,
                    span: anchor.elapsed(lexer),
                });
            }
        }
        lexer.next();

        let value = match self.value.parse(lexer, errors) {
            Some(value) => Some(value),
            None => {
                let error = recover_until_or_lf(lexer, &[], &self.value);
                errors.push(error);

                None
            }
        };

        Some(KeyValueAST {
            key,
            key_literl_option: self.key.1.clone(),
            value,
            span: anchor.elapsed(lexer),
        })
    }

    fn first_token_kind(&self) -> GeneratorTokenKind {
        self.key.0
    }
}

impl ParserPart for KeyValueParser {
    fn parse_error_code(&self) -> usize {
        0002
    }

    fn expected_format(&self) -> Option<std::borrow::Cow<'static, str>> {
        match self.parser_kind {
            KeyValueKind::Colon => Some("key: value".into()),
            KeyValueKind::Equal => Some("key = value".into()),
            KeyValueKind::NoSeparator => Some("key value".into()),
        }
    }
}

impl<'input> AST<'input> for KeyValueAST<'input> {
    fn span(&self) -> Range<usize> {
        self.span.clone()
    }

    fn take_value(
        &self,
        section_name_stack: &mut allocator_api2::vec::Vec<
            (Cow<'input, str>, Range<usize>),
            &bumpalo::Bump,
        >,
        validator: &mut ValueTypeChecker<'_, '_, '_, '_, 'input, 'input>,
    ) {
        let literal_option = self
            .key_literl_option
            .as_ref()
            .map(|option| option.clone())
            .unwrap_or_default();

        let key_text = match literal_option.trim_space {
            true => self.key.text.trim(),
            false => self.key.text,
        };

        let key_text = literal_option.resolve_escape(key_text);

        section_name_stack.push((key_text, self.key.span.clone()));

        match &self.value {
            Some(value) => {
                value.take_value(section_name_stack, validator);
            }
            None => {
                validator.set_value(
                    section_name_stack
                        .iter()
                        .map(|(name, span)| (name.clone(), span.as_utf8_byte_range())),
                    ValueTree::Value {
                        value: ValidateValue::None,
                        span: self.span.as_utf8_byte_range(),
                    },
                );
            }
        }

        section_name_stack.pop().unwrap();
    }

    fn take_token(
        &self,
        tokens: &mut std::collections::BTreeMap<usize, (super::ASTTokenKind, Range<usize>)>,
    ) {
        // TODO : check value is tree
        tokens.insert(
            self.key.span.start,
            (ASTTokenKind::Key, self.key.span.clone()),
        );

        if let Some(value) = &self.value {
            value.take_token(tokens);
        }
    }
}
