use std::{borrow::Cow, ops::Range};

use allocator_api2::vec::Vec;
use either::Either;
use serde::{Deserialize, Serialize};
use tyml_source::AsUtf8ByteRange;
use tyml_validate::validate::{ValidateValue, ValueTree, ValueTypeChecker};

use crate::lexer::{GeneratorAnchor, GeneratorTokenKind, GeneratorTokenizer};

use super::{
    AST, ASTTokenKind, NamedParserPart, Parser, ParserGenerator, ParserPart,
    error::{GeneratedParseError, recover_until_or_lf},
    literal::{LiteralSet, LiteralSetAST, LiteralSetParser},
    value::{Value, ValueAST, ValueParser},
};

#[derive(Debug, Serialize, Deserialize)]
pub struct KeyValue {
    pub key: LiteralSet,
    pub key_option: KeyOption,
    pub kind: KeyValueKind,
    pub value: Value,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KeyOption {
    pub allow_dot_section_split: bool,
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
    pub key: LiteralSetParser,
    pub key_option: KeyOptionTokenKind,
    pub kind: Option<GeneratorTokenKind>,
    pub parser_kind: KeyValueKind,
    pub value: ValueParser,
}

#[derive(Debug, Clone)]
pub struct KeyOptionTokenKind {
    pub dot: Option<GeneratorTokenKind>,
}

#[derive(Debug)]
pub struct KeyValueAST<'input> {
    pub key: Vec<LiteralSetAST<'input>>,
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
            key: self.key.generate(registry),
            key_option: KeyOptionTokenKind {
                dot: self
                    .key_option
                    .allow_dot_section_split
                    .then(|| registry.register(GeneratorTokenizer::Keyword(".".into()))),
            },
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

        let Some(literal) = self.key.parse(lexer, errors) else {
            return None;
        };

        let mut key = Vec::new();
        key.push(literal);

        if let Some(dot) = self.key_option.dot {
            loop {
                if !lexer.current_contains(dot) {
                    break;
                }
                lexer.next();

                let Some(literal) = self.key.parse(lexer, errors) else {
                    let error = recover_until_or_lf(
                        lexer,
                        [self.kind]
                            .into_iter()
                            .flatten()
                            .chain(self.value.first_token_kinds()),
                        self,
                    );
                    errors.push(error);

                    break;
                };

                key.push(literal);
            }
        }

        if let Some(kind) = self.kind {
            if !lexer.current_contains(kind) {
                let parser_part = match self.parser_kind {
                    KeyValueKind::Colon => NamedParserPart::KEY_VALUE_COLON,
                    KeyValueKind::Equal => NamedParserPart::KEY_VALUE_EQUAL,
                    KeyValueKind::NoSeparator => unreachable!(),
                };

                // contains key span for error
                lexer.back_to_anchor(GeneratorAnchor {
                    byte_position: key.first().unwrap().span().start,
                });

                let error = recover_until_or_lf(lexer, [].into_iter(), &parser_part);
                errors.push(error);

                return Some(KeyValueAST {
                    key,
                    value: None,
                    span: anchor.elapsed(lexer),
                });
            }
        }
        lexer.next();

        let value = match self.value.parse(lexer, errors) {
            Some(value) => Some(value),
            None => {
                let error = recover_until_or_lf(lexer, [].into_iter(), &self.value);
                errors.push(error);

                None
            }
        };

        Some(KeyValueAST {
            key,
            value,
            span: anchor.elapsed(lexer),
        })
    }

    fn first_token_kinds(&self) -> impl Iterator<Item = GeneratorTokenKind> {
        self.key.first_token_kinds()
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
            (Cow<'input, str>, Range<usize>, Range<usize>, bool),
            &bumpalo::Bump,
        >,
        validator: &mut ValueTypeChecker<'_, '_, '_, '_, 'input, 'input>,
    ) {
        section_name_stack.extend(
            self.key
                .iter()
                .map(|literal| literal.to_section_name(self.span()))
                .flatten(),
        );

        match &self.value {
            Some(value) => {
                value.take_value(section_name_stack, validator);
            }
            None => {
                let first_key_span = self.key.first().unwrap().span();
                let last_key_span = self.key.last().unwrap().span();

                validator.set_value(
                    section_name_stack
                        .iter()
                        .map(|(name, name_span, define_span, is_array)| {
                            (
                                name.clone(),
                                name_span.as_utf8_byte_range(),
                                define_span.as_utf8_byte_range(),
                                *is_array,
                            )
                        }),
                    Either::Left(ValueTree::Value {
                        value: ValidateValue::None,
                        key_span: (first_key_span.start..last_key_span.end).as_utf8_byte_range(),
                        span: self.span.as_utf8_byte_range(),
                    }),
                );
            }
        }

        section_name_stack.pop().unwrap();
    }

    fn take_token(
        &self,
        tokens: &mut std::collections::BTreeMap<usize, (super::ASTTokenKind, Range<usize>)>,
    ) {
        for key in self.key.iter() {
            tokens.insert(key.span().start, (ASTTokenKind::Key, key.span()));
        }

        if let Some(value) = &self.value {
            value.take_token(tokens);
        }
    }
}
