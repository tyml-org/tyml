use std::{
    borrow::Cow,
    iter::once,
    ops::Range,
    sync::{Arc, LazyLock},
};

use allocator_api2::vec::Vec;
use either::Either;
use regex::Regex;
use serde::{Deserialize, Serialize};
use tyml_source::AsUtf8ByteRange;
use tyml_validate::validate::{ValidateValue, ValueTree};

use crate::{
    lexer::{GeneratorTokenKind, GeneratorTokenizer, SpannedText},
    style::error::recover_until_or_lf,
};

use super::{
    AST, ASTTokenKind, Parser, ParserGenerator, ParserPart,
    error::GeneratedParseError,
    literal::{
        BinaryLiteral, BoolLiteral, CustomLiteralOption, FloatLiteral, Literal, QuotesKind,
        StringLiteral,
    },
};

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct Value {
    pub strings: std::vec::Vec<StringLiteral>,
    pub float: Option<FloatLiteral>,
    pub binary: Option<BinaryLiteral>,
    pub bool: Option<BoolLiteral>,
    pub any_string: Option<Literal>,
    pub array: Option<ArrayValue>,
}

#[derive(Debug, Clone)]
pub struct ValueParser {
    pub style: Arc<Value>,
    pub strings: Vec<GeneratorTokenKind>,
    pub float: Option<GeneratorTokenKind>,
    pub binary: Option<GeneratorTokenKind>,
    pub bool: Option<GeneratorTokenKind>,
    pub any_string: Option<(GeneratorTokenKind, Option<CustomLiteralOption>)>,
    pub array: Option<ArrayValueParser>,
}

#[derive(Debug)]
pub struct ValueAST<'input> {
    pub style: Arc<Value>,
    pub parser: Arc<ValueParser>,
    pub value: Option<SpannedText<'input>>,
    pub kind: ValueASTKind<'input>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum ValueASTKind<'input> {
    String { kind: GeneratorTokenKind },
    Float,
    Binary,
    Bool,
    AnyString,
    Array { array: ArrayValueAST<'input> },
}

impl<'input> ParserGenerator<'input, ValueAST<'input>, ValueParser> for Value {
    fn generate(&self, registry: &mut crate::lexer::TokenizerRegistry) -> ValueParser {
        ValueParser {
            style: Arc::new(self.clone()),
            strings: self
                .strings
                .iter()
                .map(|string| string.register(registry))
                .collect(),
            float: self.float.as_ref().map(|float| float.register(registry)),
            binary: self.binary.as_ref().map(|binary| binary.register(registry)),
            bool: self.bool.as_ref().map(|bool| bool.register(registry)),
            any_string: self
                .any_string
                .as_ref()
                .map(|any_string| any_string.register(registry)),
            array: self.array.as_ref().map(|array| array.generate(registry)),
        }
    }
}

impl<'input> Parser<'input, ValueAST<'input>> for ValueParser {
    fn parse(
        &self,
        lexer: &mut crate::lexer::GeneratorLexer<'input, '_>,
        errors: &mut Vec<GeneratedParseError>,
    ) -> Option<ValueAST<'input>> {
        let anchor = lexer.cast_anchor();

        for string in self.strings.iter() {
            if lexer.current_contains(*string) {
                return Some(ValueAST {
                    style: self.style.clone(),
                    parser: Arc::new(self.clone()),
                    value: Some(lexer.next().unwrap().into_spanned()),
                    kind: ValueASTKind::String { kind: *string },
                    span: anchor.elapsed(lexer),
                });
            }
        }

        if let Some(float) = self.float {
            if lexer.current_contains(float) {
                return Some(ValueAST {
                    style: self.style.clone(),
                    parser: Arc::new(self.clone()),
                    value: Some(lexer.next().unwrap().into_spanned()),
                    kind: ValueASTKind::Float,
                    span: anchor.elapsed(lexer),
                });
            }
        }

        if let Some(binary) = self.binary {
            if lexer.current_contains(binary) {
                return Some(ValueAST {
                    style: self.style.clone(),
                    parser: Arc::new(self.clone()),
                    value: Some(lexer.next().unwrap().into_spanned()),
                    kind: ValueASTKind::Binary,
                    span: anchor.elapsed(lexer),
                });
            }
        }

        if let Some(bool) = self.bool {
            if lexer.current_contains(bool) {
                return Some(ValueAST {
                    style: self.style.clone(),
                    parser: Arc::new(self.clone()),
                    value: Some(lexer.next().unwrap().into_spanned()),
                    kind: ValueASTKind::Bool,
                    span: anchor.elapsed(lexer),
                });
            }
        }

        if let Some((any_string, _)) = self.any_string {
            if lexer.current_contains(any_string) {
                return Some(ValueAST {
                    style: self.style.clone(),
                    parser: Arc::new(self.clone()),
                    value: Some(lexer.next().unwrap().into_spanned()),
                    kind: ValueASTKind::AnyString,
                    span: anchor.elapsed(lexer),
                });
            }
        }

        if let Some(array) = &self.array {
            if let Some(array) = array.parse(self, lexer, errors) {
                let span = array.span();

                return Some(ValueAST {
                    style: self.style.clone(),
                    parser: Arc::new(self.clone()),
                    value: None,
                    kind: ValueASTKind::Array { array },
                    span,
                });
            }
        }

        None
    }

    fn first_token_kinds(&self) -> impl Iterator<Item = GeneratorTokenKind> {
        [
            self.float,
            self.binary,
            self.binary,
            self.bool,
            self.any_string.as_ref().map(|(kind, _)| *kind),
        ]
        .into_iter()
        .flatten()
        .chain(self.strings.iter().cloned())
        .chain(
            self.array
                .as_ref()
                .map(|array| array.first_token_kinds())
                .into_iter()
                .flatten(),
        )
    }
}

impl ParserPart for ValueParser {
    fn parse_error_code(&self) -> usize {
        0004
    }

    fn expected_format(&self) -> Option<std::borrow::Cow<'static, str>> {
        None
    }
}

impl<'input> ValueAST<'input> {
    fn create_value_tree(
        &self,
        section_name_stack: &mut allocator_api2::vec::Vec<
            (Cow<'input, str>, Range<usize>, Range<usize>, bool),
            &bumpalo::Bump,
        >,
    ) -> ValueTree<'input, 'input> {
        let value = match &self.kind {
            ValueASTKind::String { kind } => {
                let quotes_kind = self
                    .style
                    .strings
                    .iter()
                    .zip(self.parser.strings.iter())
                    .find(|(_, parser)| *parser == kind)
                    .map(|(style, _)| style)
                    .unwrap()
                    .quotes_kind;

                let value = self.value.as_ref().unwrap();

                let value = match quotes_kind {
                    QuotesKind::DoubleQuotes => &value.text[1..(value.text.len() - 1)],
                    QuotesKind::TripleDoubleQuotes => &value.text[3..(value.text.len() - 3)],
                    QuotesKind::SingleQuote => &value.text[1..(value.text.len() - 1)],
                    QuotesKind::TripleQuotes => &value.text[3..(value.text.len() - 3)],
                };

                ValidateValue::String(value.into())
            }
            ValueASTKind::Float => {
                let space_removed = self.value.as_ref().unwrap().text.replace(" ", "");

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

                let space_removed = self.value.as_ref().unwrap().text.replace(" ", "");

                static HEX: LazyLock<Regex> =
                    LazyLock::new(|| Regex::new(r"([+-]?)0x([a-f|A-F|0-9|_]+)").unwrap());
                static OCT: LazyLock<Regex> =
                    LazyLock::new(|| Regex::new(r"([+-]?)0o([0-7|_]+)").unwrap());
                static BIN: LazyLock<Regex> =
                    LazyLock::new(|| Regex::new(r"([+-]?)0b([01_]+)").unwrap());

                let (plus_minus, value_text, radix) = if space_removed.contains("0x") {
                    let captured = HEX.captures(space_removed.as_str()).unwrap();
                    (captured[1].to_string(), &captured[2].to_string(), 16)
                } else if space_removed.contains("0o") {
                    let captured = OCT.captures(space_removed.as_str()).unwrap();
                    (captured[1].to_string(), &captured[2].to_string(), 8)
                } else if space_removed.contains("0b") {
                    let captured = BIN.captures(space_removed.as_str()).unwrap();
                    (captured[1].to_string(), &captured[2].to_string(), 2)
                } else {
                    unreachable!()
                };

                match style.allow_plus_minus {
                    true => {
                        let value = i64::from_str_radix(value_text.as_str(), radix).ok();

                        if plus_minus == "-" {
                            match value.map(|value| -value) {
                                Some(value) => ValidateValue::Int(value),
                                None => ValidateValue::OverflowInt(value_text.clone()),
                            }
                        } else {
                            match value {
                                Some(value) => ValidateValue::Int(value),
                                None => ValidateValue::OverflowInt(value_text.clone()),
                            }
                        }
                    }
                    false => match u64::from_str_radix(value_text.as_str(), radix).ok() {
                        Some(value) => ValidateValue::UnsignedInt(value),
                        None => ValidateValue::OverflowInt(value_text.to_string()),
                    },
                }
            }
            ValueASTKind::Bool => {
                let lower = self.value.as_ref().unwrap().text.to_ascii_lowercase();

                ValidateValue::Bool(lower.parse::<bool>().unwrap())
            }
            ValueASTKind::AnyString => {
                let value = self.value.as_ref().unwrap();
                let style = self.style.any_string.as_ref().unwrap();

                let value = match style {
                    Literal::Custom(custom_regex_literal) => {
                        match custom_regex_literal.option.trim_space {
                            true => value.text.trim(),
                            false => value.text,
                        }
                    }
                    _ => value.text,
                };

                ValidateValue::String(value.into())
            }
            ValueASTKind::Array { array } => return array.create_value_tree(section_name_stack),
        };

        // take last section(maybe key)'s span
        let key_span = match section_name_stack.last() {
            Some((_, span, _, _)) => span.clone(),
            None => self.span.clone(),
        };

        ValueTree::Value {
            value,
            key_span: key_span.as_utf8_byte_range(),
            span: self.span.as_utf8_byte_range(),
        }
    }
}

impl<'input> AST<'input> for ValueAST<'input> {
    fn span(&self) -> std::ops::Range<usize> {
        self.span.clone()
    }

    fn take_value(
        &self,
        section_name_stack: &mut allocator_api2::vec::Vec<
            (Cow<'input, str>, Range<usize>, Range<usize>, bool),
            &bumpalo::Bump,
        >,
        validator: &mut tyml_validate::validate::ValueTypeChecker<'_, '_, '_, '_, 'input, 'input>,
    ) {
        let value_tree = self.create_value_tree(section_name_stack);

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
            Either::Left(value_tree),
        );
    }

    fn take_token(
        &self,
        tokens: &mut std::collections::BTreeMap<usize, (super::ASTTokenKind, Range<usize>)>,
    ) {
        if let ValueASTKind::Array { array } = &self.kind {
            return array.take_token(tokens);
        }

        let value = self.value.as_ref().unwrap();

        let kind = match &self.kind {
            ValueASTKind::String { kind: _ } => ASTTokenKind::StringValue,
            ValueASTKind::Float => {
                let lower = value.text.to_lowercase();

                if lower.contains("inf") || lower.contains("nan") {
                    ASTTokenKind::InfNan
                } else {
                    ASTTokenKind::NumericValue
                }
            }
            ValueASTKind::Binary => ASTTokenKind::NumericValue,
            ValueASTKind::Bool => ASTTokenKind::BoolValue,
            ValueASTKind::AnyString => ASTTokenKind::StringValue,
            ValueASTKind::Array { array: _ } => unreachable!(),
        };

        tokens.insert(value.span.start, (kind, value.span.clone()));
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ArrayValue {
    Bracket {
        allow_line_feed: bool,
        allow_extra_comma: bool,
    },
}

#[derive(Debug, Clone)]
pub enum ArrayValueParser {
    Bracket {
        bracket_left: GeneratorTokenKind,
        bracket_right: GeneratorTokenKind,
        comma: GeneratorTokenKind,
        allow_line_feed: bool,
        allow_extra_comma: bool,
    },
}

#[derive(Debug)]
pub struct ArrayValueAST<'input> {
    pub values: Vec<ValueAST<'input>>,
    pub span: Range<usize>,
}

impl<'input> ParserGenerator<'input, ArrayValueAST<'input>, ArrayValueParser> for ArrayValue {
    fn generate(&self, registry: &mut crate::lexer::TokenizerRegistry) -> ArrayValueParser {
        match self {
            ArrayValue::Bracket {
                allow_line_feed,
                allow_extra_comma,
            } => ArrayValueParser::Bracket {
                bracket_left: registry.register(GeneratorTokenizer::Keyword("[".to_string())),
                bracket_right: registry.register(GeneratorTokenizer::Keyword("]".to_string())),
                comma: registry.register(GeneratorTokenizer::Keyword(",".to_string())),
                allow_line_feed: *allow_line_feed,
                allow_extra_comma: *allow_extra_comma,
            },
        }
    }
}

impl<'input> ArrayValueParser {
    fn parse(
        &self,
        value_parser: &ValueParser,
        lexer: &mut crate::lexer::GeneratorLexer<'input, '_>,
        errors: &mut Vec<GeneratedParseError>,
    ) -> Option<ArrayValueAST<'input>> {
        match self {
            ArrayValueParser::Bracket {
                bracket_left,
                bracket_right,
                comma,
                allow_line_feed,
                allow_extra_comma,
            } => {
                let anchor = lexer.cast_anchor();

                if !lexer.current_contains(*bracket_left) {
                    return None;
                }
                lexer.next();

                let mut values = Vec::new();

                let mut is_last_comma = false;
                loop {
                    if *allow_line_feed {
                        lexer.skip_lf();
                    }

                    let Some(value) = value_parser.parse(lexer, errors) else {
                        break;
                    };
                    values.push(value);

                    is_last_comma = false;

                    if !lexer.current_contains(*comma) {
                        break;
                    }
                    lexer.next();
                    is_last_comma = true;
                }

                if *allow_line_feed {
                    lexer.skip_lf();
                }

                let mut has_error = false;
                if !lexer.current_contains(*bracket_right) {
                    let error = recover_until_or_lf(lexer, [*bracket_right].into_iter(), self);
                    errors.push(error);

                    has_error = true;
                }

                if !*allow_extra_comma && !has_error && is_last_comma {
                    let error = recover_until_or_lf(lexer, [*bracket_right].into_iter(), self);
                    errors.push(error);
                }

                if lexer.current_contains(*bracket_right) {
                    lexer.next();
                }

                Some(ArrayValueAST {
                    values,
                    span: anchor.elapsed(lexer),
                })
            }
        }
    }
}

impl<'input> Parser<'input, ArrayValueAST<'input>> for ArrayValueParser {
    fn parse(
        &self,
        _: &mut crate::lexer::GeneratorLexer<'input, '_>,
        _: &mut Vec<GeneratedParseError>,
    ) -> Option<ArrayValueAST<'input>> {
        unreachable!()
    }

    fn first_token_kinds(&self) -> impl Iterator<Item = GeneratorTokenKind> {
        match self {
            ArrayValueParser::Bracket {
                bracket_left,
                bracket_right: _,
                comma: _,
                allow_line_feed: _,
                allow_extra_comma: _,
            } => once(*bracket_left),
        }
    }
}

impl ParserPart for ArrayValueParser {
    fn parse_error_code(&self) -> usize {
        match self {
            ArrayValueParser::Bracket {
                bracket_left: _,
                bracket_right: _,
                comma: _,
                allow_line_feed: _,
                allow_extra_comma: _,
            } => 0008,
        }
    }

    fn expected_format(&self) -> Option<Cow<'static, str>> {
        match self {
            ArrayValueParser::Bracket {
                bracket_left: _,
                bracket_right: _,
                comma: _,
                allow_line_feed: _,
                allow_extra_comma: _,
            } => Some("[value1, value2]".into()),
        }
    }
}

impl<'input> ArrayValueAST<'input> {
    fn create_value_tree(
        &self,
        section_name_stack: &mut allocator_api2::vec::Vec<
            (Cow<'input, str>, Range<usize>, Range<usize>, bool),
            &bumpalo::Bump,
        >,
    ) -> ValueTree<'input, 'input> {
        let elements = self
            .values
            .iter()
            .map(|value| value.create_value_tree(section_name_stack))
            .collect();

        // take last section(maybe key)'s span
        let key_span = match section_name_stack.last() {
            Some((_, span, _, _)) => span.clone(),
            None => self.span.clone(),
        };

        ValueTree::Array {
            elements,
            key_span: key_span.as_utf8_byte_range(),
            span: (key_span.start..self.span.end).as_utf8_byte_range(),
        }
    }
}

impl<'input> AST<'input> for ArrayValueAST<'input> {
    fn span(&self) -> Range<usize> {
        self.span.clone()
    }

    fn take_value(
        &self,
        section_name_stack: &mut Vec<
            (Cow<'input, str>, Range<usize>, Range<usize>, bool),
            &bumpalo::Bump,
        >,
        validator: &mut tyml_validate::validate::ValueTypeChecker<'_, '_, '_, '_, 'input, 'input>,
    ) {
        let value_tree = self.create_value_tree(section_name_stack);

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
            Either::Left(value_tree),
        );
    }

    fn take_token(
        &self,
        tokens: &mut std::collections::BTreeMap<usize, (ASTTokenKind, Range<usize>)>,
    ) {
        for value in self.values.iter() {
            value.take_token(tokens);
        }
    }
}
