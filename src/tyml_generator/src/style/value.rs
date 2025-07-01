use std::{
    borrow::Cow,
    iter::once,
    ops::Range,
    sync::{Arc, LazyLock},
};

use allocator_api2::vec::Vec;
use regex::Regex;
use serde::{Deserialize, Serialize};
use tyml_formatter::{FormatterTokenKind, SpaceFormat};
use tyml_source::AsUtf8ByteRange;
use tyml_type::types::{NamedTypeMap, Type, TypeTree};
use tyml_validate::validate::{SetValue, ValidateValue, ValueTree, ValueTypeChecker};

use crate::{
    lexer::{GeneratorTokenKind, GeneratorTokenizer, SpannedText},
    style::{
        FormatterTokenInfo, NamedParserPart,
        error::recover_until_or_lf,
        key_value::{KeyValueAST, KeyValueParser},
    },
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
    pub inline_section: Option<InlineSection>,
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
    pub inline_section: Option<InlineSectionParser>,
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
    String {
        kind: GeneratorTokenKind,
    },
    Float,
    Binary,
    Bool,
    AnyString,
    Array {
        array: ArrayValueAST<'input>,
    },
    InlineSection {
        inline_section: InlineSectionAST<'input>,
    },
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
            inline_section: self
                .inline_section
                .as_ref()
                .map(|inline_section| inline_section.generate(registry)),
        }
    }
}

impl<'input> ValueParser {
    pub fn parse(
        &self,
        key_value_parser: &KeyValueParser,
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
            if let Some(array) = array.parse(key_value_parser, lexer, errors) {
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

        if let Some(inline_section) = &self.inline_section {
            if let Some(inline_section) = inline_section.parse(key_value_parser, lexer, errors) {
                let span = inline_section.span();

                return Some(ValueAST {
                    style: self.style.clone(),
                    parser: Arc::new(self.clone()),
                    value: None,
                    kind: ValueASTKind::InlineSection { inline_section },
                    span,
                });
            }
        }

        None
    }
}

impl<'input> Parser<'input, ValueAST<'input>> for ValueParser {
    fn parse(
        &self,
        _: &mut crate::lexer::GeneratorLexer<'input, '_>,
        _: &mut Vec<GeneratedParseError>,
    ) -> Option<ValueAST<'input>> {
        unreachable!()
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

    fn map_formatter_token(
        &self,
        map: &mut std::collections::HashMap<GeneratorTokenKind, FormatterTokenKind>,
    ) {
        if let Some(array) = &self.array {
            array.map_formatter_token(map);
        }
        if let Some(inline_section) = &self.inline_section {
            inline_section.map_formatter_token(map);
        }
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
    fn create_value(
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
            ValueASTKind::Array { array } => {
                return array.create_value(section_name_stack);
            }
            ValueASTKind::InlineSection { inline_section } => {
                return inline_section.create_value(section_name_stack);
            }
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
        let value_tree = self.create_value(section_name_stack);

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
            SetValue::Value(value_tree),
        );
    }

    fn take_token(
        &self,
        tokens: &mut std::collections::BTreeMap<usize, (super::ASTTokenKind, Range<usize>)>,
    ) {
        if let ValueASTKind::Array { array } = &self.kind {
            return array.take_token(tokens);
        }

        if let ValueASTKind::InlineSection { inline_section } = &self.kind {
            return inline_section.take_token(tokens);
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
            ValueASTKind::InlineSection { inline_section: _ } => unreachable!(),
        };

        tokens.insert(value.span.start, (kind, value.span.clone()));
    }

    fn take_formatter_token(&self, tokens: &mut Vec<FormatterTokenInfo>) {
        match &self.kind {
            ValueASTKind::Array { array } => {
                array.take_formatter_token(tokens);
            }
            ValueASTKind::InlineSection { inline_section } => {
                inline_section.take_formatter_token(tokens);
            }
            _ => {}
        }
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
        key_value_parser: &KeyValueParser,
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

                let mut last_comma = None;
                loop {
                    if *allow_line_feed {
                        lexer.skip_lf();
                    }

                    let Some(value) = key_value_parser
                        .value
                        .parse(key_value_parser, lexer, errors)
                    else {
                        break;
                    };
                    values.push(value);

                    last_comma = None;

                    if !lexer.current_contains(*comma) {
                        break;
                    }
                    last_comma = lexer.next();
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

                if !*allow_extra_comma && !has_error {
                    if let Some(last_comma) = last_comma {
                        let error = GeneratedParseError {
                            span: last_comma.span.clone(),
                            parse_error_code: 0011,
                            expected_format: None,
                        };
                        errors.push(error);
                    }
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

    fn map_formatter_token(
        &self,
        map: &mut std::collections::HashMap<GeneratorTokenKind, FormatterTokenKind>,
    ) {
        match self {
            ArrayValueParser::Bracket {
                bracket_left,
                bracket_right,
                comma: _,
                allow_line_feed: _,
                allow_extra_comma: _,
            } => {
                map.insert(*bracket_left, FormatterTokenKind::TreeIn);
                map.insert(*bracket_right, FormatterTokenKind::TreeOut);
            }
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
    fn create_value(
        &self,
        section_name_stack: &mut allocator_api2::vec::Vec<
            (Cow<'input, str>, Range<usize>, Range<usize>, bool),
            &bumpalo::Bump,
        >,
    ) -> ValueTree<'input, 'input> {
        let mut elements = Vec::new();
        for value in self.values.iter() {
            elements.push(value.create_value(section_name_stack));
        }

        // take last section(maybe key)'s span
        let key_span = match section_name_stack.last() {
            Some((_, span, _, _)) => span.clone(),
            None => self.span.clone(),
        };

        ValueTree::Array {
            elements,
            key_span: key_span.as_utf8_byte_range(),
            span: self.span.as_utf8_byte_range(),
        }
    }
}

impl<'input> AST<'input> for ArrayValueAST<'input> {
    fn span(&self) -> Range<usize> {
        self.span.clone()
    }

    fn take_value(
        &self,
        _: &mut Vec<(Cow<'input, str>, Range<usize>, Range<usize>, bool), &bumpalo::Bump>,
        _: &mut tyml_validate::validate::ValueTypeChecker<'_, '_, '_, '_, 'input, 'input>,
    ) {
        unreachable!()
    }

    fn take_token(
        &self,
        tokens: &mut std::collections::BTreeMap<usize, (ASTTokenKind, Range<usize>)>,
    ) {
        for value in self.values.iter() {
            value.take_token(tokens);
        }
    }

    fn take_formatter_token(&self, tokens: &mut Vec<FormatterTokenInfo>) {
        for value in self.values.iter() {
            value.take_formatter_token(tokens);
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InlineSection {
    pub kind: InlineSectionKind,
    pub separator: InlineSectionSeparator,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum InlineSectionSeparator {
    LineFeed,
    Comma {
        allow_line_feed: bool,
        allow_extra_comma: bool,
    },
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum InlineSectionKind {
    Brace,
}

#[derive(Debug, Clone)]
pub struct InlineSectionParser {
    pub kind: InlineSectionKindParser,
    pub separator: InlineSectionSeparatorParser,
}

#[derive(Debug, Clone)]
pub enum InlineSectionKindParser {
    Brace {
        brace_left: GeneratorTokenKind,
        brace_right: GeneratorTokenKind,
    },
}

#[derive(Debug, Clone)]
pub enum InlineSectionSeparatorParser {
    LineFeed,
    Comma {
        allow_line_feed: bool,
        allow_extra_comma: bool,
        comma: GeneratorTokenKind,
    },
}

#[derive(Debug)]
pub struct InlineSectionAST<'input> {
    pub key_values: Vec<KeyValueAST<'input>>,
    pub span: Range<usize>,
}

impl<'input> ParserGenerator<'input, InlineSectionAST<'input>, InlineSectionParser>
    for InlineSection
{
    fn generate(&self, registry: &mut crate::lexer::TokenizerRegistry) -> InlineSectionParser {
        let kind = match self.kind {
            InlineSectionKind::Brace => InlineSectionKindParser::Brace {
                brace_left: registry.register(GeneratorTokenizer::Keyword("{".to_string())),
                brace_right: registry.register(GeneratorTokenizer::Keyword("}".to_string())),
            },
        };
        let separator = match &self.separator {
            InlineSectionSeparator::LineFeed => InlineSectionSeparatorParser::LineFeed,
            InlineSectionSeparator::Comma {
                allow_line_feed,
                allow_extra_comma,
            } => InlineSectionSeparatorParser::Comma {
                allow_line_feed: *allow_line_feed,
                allow_extra_comma: *allow_extra_comma,
                comma: registry.register(GeneratorTokenizer::Keyword(",".to_string())),
            },
        };

        InlineSectionParser { kind, separator }
    }
}

impl<'input> InlineSectionParser {
    fn parse(
        &self,
        key_value_parser: &KeyValueParser,
        lexer: &mut crate::lexer::GeneratorLexer<'input, '_>,
        errors: &mut Vec<GeneratedParseError>,
    ) -> Option<InlineSectionAST<'input>> {
        match &self.kind {
            InlineSectionKindParser::Brace {
                brace_left,
                brace_right,
            } => {
                let anchor = lexer.cast_anchor();

                if !lexer.current_contains(*brace_left) {
                    return None;
                }
                lexer.next();

                let mut key_values = Vec::new();

                let mut last_comma = None;
                loop {
                    if let InlineSectionSeparatorParser::Comma {
                        allow_line_feed,
                        allow_extra_comma: _,
                        comma: _,
                    } = &self.separator
                    {
                        if *allow_line_feed {
                            lexer.skip_lf();
                        }
                    }

                    let Some(key_value) = key_value_parser.parse(lexer, errors) else {
                        break;
                    };
                    key_values.push(key_value);

                    last_comma = None;

                    match &self.separator {
                        InlineSectionSeparatorParser::LineFeed => {
                            if !lexer.current_contains(GeneratorTokenKind::LineFeed) {
                                break;
                            }
                            lexer.skip_lf();
                        }
                        InlineSectionSeparatorParser::Comma {
                            allow_line_feed: _,
                            allow_extra_comma: _,
                            comma,
                        } => {
                            if !lexer.current_contains(*comma) {
                                break;
                            }
                            last_comma = lexer.next();
                        }
                    }
                }

                if !lexer.current_contains(*brace_right) {
                    let error =
                        recover_until_or_lf(lexer, [].into_iter(), &NamedParserPart::BRACE_RIGHT);
                    errors.push(error);
                }

                if let InlineSectionSeparatorParser::Comma {
                    allow_line_feed,
                    allow_extra_comma,
                    comma: _,
                } = &self.separator
                {
                    if *allow_line_feed {
                        lexer.skip_lf();
                    }

                    if !*allow_extra_comma {
                        if let Some(last_comma) = last_comma {
                            let error = GeneratedParseError {
                                span: last_comma.span.clone(),
                                parse_error_code: 0011,
                                expected_format: None,
                            };
                            errors.push(error);
                        }
                    }
                }

                if !lexer.current_contains(*brace_right) {
                    let error = recover_until_or_lf(
                        lexer,
                        [*brace_right].into_iter(),
                        &NamedParserPart::BRACE_RIGHT,
                    );
                    errors.push(error);
                }

                if lexer.current_contains(*brace_right) {
                    lexer.next();
                }

                Some(InlineSectionAST {
                    key_values,
                    span: anchor.elapsed(lexer),
                })
            }
        }
    }
}

impl<'input> Parser<'input, InlineSectionAST<'input>> for InlineSectionParser {
    fn parse(
        &self,
        _: &mut crate::lexer::GeneratorLexer<'input, '_>,
        _: &mut Vec<GeneratedParseError>,
    ) -> Option<InlineSectionAST<'input>> {
        unreachable!()
    }

    fn first_token_kinds(&self) -> impl Iterator<Item = GeneratorTokenKind> {
        match &self.kind {
            InlineSectionKindParser::Brace {
                brace_left,
                brace_right: _,
            } => once(*brace_left),
        }
    }

    fn map_formatter_token(
        &self,
        map: &mut std::collections::HashMap<GeneratorTokenKind, tyml_formatter::FormatterTokenKind>,
    ) {
        match self.kind {
            InlineSectionKindParser::Brace {
                brace_left,
                brace_right,
            } => {
                map.insert(brace_left, FormatterTokenKind::TreeIn);
                map.insert(brace_right, FormatterTokenKind::TreeOut);
            }
        }
    }
}

impl ParserPart for InlineSectionParser {
    fn parse_error_code(&self) -> usize {
        0009
    }

    fn expected_format(&self) -> Option<Cow<'static, str>> {
        None
    }
}

impl<'input> InlineSectionAST<'input> {
    fn create_value(
        &self,
        section_name_stack: &mut allocator_api2::vec::Vec<
            (Cow<'input, str>, Range<usize>, Range<usize>, bool),
            &bumpalo::Bump,
        >,
    ) -> ValueTree<'input, 'input> {
        // take last section(maybe key)'s span
        let key_span = match section_name_stack.last() {
            Some((_, span, _, _)) => span.clone(),
            None => self.span.clone(),
        };

        let mut section_name_stack = Vec::new_in(*section_name_stack.allocator());

        let dummy_tree = TypeTree::Leaf {
            ty: Type::Unknown,
            documents: Vec::new_in(*section_name_stack.allocator()),
            span: 0..0,
        };
        let dummy_named_map = NamedTypeMap::new(section_name_stack.allocator());

        // dummy validator for create value tree
        let mut validator = ValueTypeChecker::new(&dummy_tree, &dummy_named_map);

        for key_value in self.key_values.iter() {
            key_value.take_value(&mut section_name_stack, &mut validator);
        }

        // get root section
        if let ValueTree::Section {
            mut elements,
            name_span: _,
            define_span: _,
        } = validator.value_tree
        {
            if let Some(mut root) = elements.remove("root") {
                if let ValueTree::Section {
                    elements,
                    name_span: _,
                    define_span: _,
                } = root.remove(0)
                {
                    return ValueTree::Section {
                        elements,
                        name_span: key_span.as_utf8_byte_range(),
                        define_span: self.span.as_utf8_byte_range(),
                    };
                }
            }
        }
        ValueTree::Section {
            elements: Default::default(),
            name_span: key_span.as_utf8_byte_range(),
            define_span: self.span.as_utf8_byte_range(),
        }
    }
}

impl<'input> AST<'input> for InlineSectionAST<'input> {
    fn span(&self) -> Range<usize> {
        self.span.clone()
    }

    fn take_value(
        &self,
        _: &mut Vec<(Cow<'input, str>, Range<usize>, Range<usize>, bool), &bumpalo::Bump>,
        _: &mut tyml_validate::validate::ValueTypeChecker<'_, '_, '_, '_, 'input, 'input>,
    ) {
        unreachable!()
    }

    fn take_token(
        &self,
        tokens: &mut std::collections::BTreeMap<usize, (ASTTokenKind, Range<usize>)>,
    ) {
        for key_value in self.key_values.iter() {
            key_value.take_token(tokens);
        }
    }

    fn take_formatter_token(&self, tokens: &mut Vec<super::FormatterTokenInfo>) {
        for (index, key_value) in self.key_values.iter().enumerate() {
            key_value.take_formatter_token(tokens);

            if index == self.key_values.len() - 1 {
                tokens.push(FormatterTokenInfo {
                    span: key_value.span(),
                    left_space: SpaceFormat::None,
                    right_space: SpaceFormat::LineFeedOrSplit(","),
                });
            } else {
                tokens.push(FormatterTokenInfo {
                    span: key_value.span(),
                    left_space: SpaceFormat::None,
                    right_space: SpaceFormat::LineFeed,
                });
            }
        }
    }
}
