use std::{ops::Range, sync::Arc};

use allocator_api2::vec::Vec;
use tyml_source::AsUtf8ByteRange;
use tyml_validate::validate::{ValidateValue, ValueTree};

use crate::lexer::{GeneratorTokenKind, SpannedText};

use super::{
    AST, Parser, ParserGenerator, ParserPart,
    error::GeneratedParseError,
    literal::{
        BinaryLiteral, BoolLiteral, CustomLiteralOption, FloatLiteral, Literal, QuotesKind,
        StringLiteral,
    },
};

#[derive(Debug, Default, Clone)]
pub struct Value {
    pub string: Option<StringLiteral>,
    pub float: Option<FloatLiteral>,
    pub binary: Option<BinaryLiteral>,
    pub bool: Option<BoolLiteral>,
    pub any_string: Option<Literal>,
}

#[derive(Debug)]
pub struct ValueParser {
    pub style: Arc<Value>,
    pub string: Option<GeneratorTokenKind>,
    pub float: Option<GeneratorTokenKind>,
    pub binary: Option<GeneratorTokenKind>,
    pub bool: Option<GeneratorTokenKind>,
    pub any_string: Option<(GeneratorTokenKind, Option<CustomLiteralOption>)>,
}

#[derive(Debug)]
pub struct ValueAST<'input> {
    pub style: Arc<Value>,
    pub value: SpannedText<'input>,
    pub kind: ValueASTKind,
    pub span: Range<usize>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ValueASTKind {
    String,
    Float,
    Binary,
    Bool,
    AnyString,
}

impl<'input> ParserGenerator<'input, ValueAST<'input>, ValueParser> for Value {
    fn generate(&self, registry: &mut crate::lexer::TokenizerRegistry) -> ValueParser {
        ValueParser {
            style: Arc::new(self.clone()),
            string: self.string.as_ref().map(|string| string.register(registry)),
            float: self.float.as_ref().map(|float| float.register(registry)),
            binary: self.binary.as_ref().map(|binary| binary.register(registry)),
            bool: self.bool.as_ref().map(|bool| bool.register(registry)),
            any_string: self
                .any_string
                .as_ref()
                .map(|any_string| any_string.register(registry)),
        }
    }
}

impl<'input> Parser<'input, ValueAST<'input>> for ValueParser {
    fn parse(
        &self,
        lexer: &mut crate::lexer::GeneratorLexer<'input, '_>,
        _: &mut Vec<GeneratedParseError>,
    ) -> Option<ValueAST<'input>> {
        let anchor = lexer.cast_anchor();

        if let Some(string) = self.string {
            if lexer.current_contains(string) {
                return Some(ValueAST {
                    style: self.style.clone(),
                    value: lexer.next().unwrap().into_spanned(),
                    kind: ValueASTKind::String,
                    span: anchor.elapsed(lexer),
                });
            }
        }

        if let Some(float) = self.float {
            if lexer.current_contains(float) {
                return Some(ValueAST {
                    style: self.style.clone(),
                    value: lexer.next().unwrap().into_spanned(),
                    kind: ValueASTKind::Float,
                    span: anchor.elapsed(lexer),
                });
            }
        }

        if let Some(binary) = self.binary {
            if lexer.current_contains(binary) {
                return Some(ValueAST {
                    style: self.style.clone(),
                    value: lexer.next().unwrap().into_spanned(),
                    kind: ValueASTKind::Binary,
                    span: anchor.elapsed(lexer),
                });
            }
        }

        if let Some(bool) = self.bool {
            if lexer.current_contains(bool) {
                return Some(ValueAST {
                    style: self.style.clone(),
                    value: lexer.next().unwrap().into_spanned(),
                    kind: ValueASTKind::Bool,
                    span: anchor.elapsed(lexer),
                });
            }
        }

        if let Some((any_string, _)) = self.any_string {
            if lexer.current_contains(any_string) {
                return Some(ValueAST {
                    style: self.style.clone(),
                    value: lexer.next().unwrap().into_spanned(),
                    kind: ValueASTKind::AnyString,
                    span: anchor.elapsed(lexer),
                });
            }
        }

        None
    }

    fn first_token_kind(&self) -> GeneratorTokenKind {
        self.string
            .or(self.float)
            .or(self.binary)
            .or(self.binary)
            .or(self.bool)
            .or(self.any_string.as_ref().map(|(kind, _)| *kind))
            .unwrap()
    }
}

impl ParserPart for ValueParser {
    fn expected_message_key(&self) -> std::borrow::Cow<'static, str> {
        "expected.message.value".into()
    }

    fn expected_format_key(&self) -> Option<std::borrow::Cow<'static, str>> {
        None
    }
}

impl<'input> AST<'input> for ValueAST<'input> {
    fn span(&self) -> std::ops::Range<usize> {
        self.span.clone()
    }

    fn take_value(
        &self,
        section_name_stack: &mut allocator_api2::vec::Vec<
            (&'input str, Range<usize>),
            &bumpalo::Bump,
        >,
        validator: &mut tyml_validate::validate::ValueTypeChecker<'_, '_, '_, '_, 'input, 'input>,
    ) {
        let value = match self.kind {
            ValueASTKind::String => {
                let style = self.style.string.as_ref().unwrap();

                let value = match style.quotes_kind {
                    QuotesKind::DoubleQuotes => &self.value.text[1..(self.value.text.len() - 1)],
                    QuotesKind::TripleDoubleQuotes => {
                        &self.value.text[3..(self.value.text.len() - 3)]
                    }
                    QuotesKind::SingleQuote => &self.value.text[1..(self.value.text.len() - 1)],
                    QuotesKind::TripleQuotes => &self.value.text[3..(self.value.text.len() - 3)],
                };

                ValidateValue::String(value.into())
            }
            ValueASTKind::Float => {
                let space_removed = self.value.text.replace(" ", "");

                if let Ok(value) = space_removed.parse::<u64>() {
                    ValidateValue::UnsignedInt(value)
                } else if let Ok(value) = space_removed.parse::<i64>() {
                    ValidateValue::Int(value)
                } else {
                    let value = space_removed.parse::<f64>().unwrap();

                    ValidateValue::Float(value)
                }
            }
            ValueASTKind::Binary => {
                let style = self.style.binary.as_ref().unwrap();

                let space_removed = self.value.text.replace(" ", "");

                let prefix = match style.allow_plus_minus {
                    true => &space_removed[1..3],
                    false => &space_removed[0..2],
                };

                let binary_text = match style.allow_plus_minus {
                    true => &space_removed[3..],
                    false => &space_removed[2..],
                };

                let value = match prefix {
                    "0x" => u64::from_str_radix(binary_text, 16),
                    "0o" => u64::from_str_radix(binary_text, 8),
                    "0b" => u64::from_str_radix(binary_text, 2),
                    _ => unreachable!(),
                }
                .unwrap();

                ValidateValue::UnsignedInt(value)
            }
            ValueASTKind::Bool => {
                let lower = self.value.text.to_ascii_lowercase();

                ValidateValue::Bool(lower.parse::<bool>().unwrap())
            }
            ValueASTKind::AnyString => {
                let style = self.style.any_string.as_ref().unwrap();

                let value = match style {
                    Literal::Custom(custom_regex_literal) => {
                        match custom_regex_literal.option.trim_space {
                            true => self.value.text.trim(),
                            false => self.value.text,
                        }
                    }
                    _ => self.value.text,
                };

                ValidateValue::String(value.into())
            }
        };

        validator.set_value(
            section_name_stack
                .iter()
                .map(|(name, span)| (*name, span.as_utf8_byte_range())),
            ValueTree::Value {
                value,
                span: self.value.span.as_utf8_byte_range(),
            },
        );
    }
}
