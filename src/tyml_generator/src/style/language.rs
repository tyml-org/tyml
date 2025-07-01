use std::{borrow::Cow, iter::once, ops::Range};

use allocator_api2::vec::Vec;
use auto_enums::auto_enum;
use serde::{Deserialize, Serialize};
use tyml_source::AsUtf8ByteRange;
use tyml_validate::validate::{SetValue, ValueTree, ValueTypeChecker};

use crate::lexer::{GeneratorTokenKind, GeneratorTokenizer};

use super::{
    AST, NamedParserPart, Parser, ParserGenerator, ParserPart,
    comment::Comment,
    error::{GeneratedParseError, recover_until_or_lf},
    key_value::{KeyValue, KeyValueAST, KeyValueParser},
    section::{Section, SectionAST, SectionParser},
};

#[derive(Debug, Serialize, Deserialize)]
pub enum LanguageStyle {
    Section(SectionStyle),
    Empty,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct SectionStyle {
    pub section: Section,
    pub key_value: KeyValue,
    pub comments: std::vec::Vec<Comment>,
    pub allow_non_section_key_value: bool,
}

pub enum LanguageParser {
    Section {
        section: SectionParser,
        key_value: KeyValueParser,
        comments: Vec<GeneratorTokenKind>,
        allow_non_section_key_value: bool,
    },
    Empty {
        empty_lexer: GeneratorTokenKind,
    },
}

#[derive(Debug)]
pub enum LanguageAST<'input> {
    Section {
        non_section_key_values: Vec<KeyValueAST<'input>>,
        sections: Vec<(SectionAST<'input>, Vec<KeyValueAST<'input>>, Range<usize>)>,
        span: Range<usize>,
    },
    Empty {
        span: Range<usize>,
    },
}

impl<'input> ParserGenerator<'input, LanguageAST<'input>, LanguageParser> for LanguageStyle {
    fn generate(&self, registry: &mut crate::lexer::TokenizerRegistry) -> LanguageParser {
        match self {
            LanguageStyle::Section(section) => LanguageParser::Section {
                section: section.section.generate(registry),
                key_value: section.key_value.generate(registry),
                comments: section
                    .comments
                    .iter()
                    .map(|comment| registry.register(GeneratorTokenizer::Regex(comment.regex())))
                    .collect(),
                allow_non_section_key_value: section.allow_non_section_key_value,
            },
            LanguageStyle::Empty => LanguageParser::Empty {
                empty_lexer: registry.register(GeneratorTokenizer::regex(".*")),
            },
        }
    }
}

impl<'input> Parser<'input, LanguageAST<'input>> for LanguageParser {
    fn parse(
        &self,
        lexer: &mut crate::lexer::GeneratorLexer<'input, '_>,
        errors: &mut Vec<GeneratedParseError>,
    ) -> Option<LanguageAST<'input>>
    where
        Self: Sized,
    {
        let anchor = lexer.cast_anchor();

        match self {
            LanguageParser::Section {
                section,
                key_value,
                comments,
                allow_non_section_key_value,
            } => {
                let mut non_section_key_values = Vec::new();
                let mut sections = Vec::new();

                lexer.comments.extend(comments.iter().cloned());

                if *allow_non_section_key_value {
                    loop {
                        lexer.skip_lf();

                        let Some(key_value) = key_value.parse(lexer, errors) else {
                            break;
                        };

                        non_section_key_values.push(key_value);
                    }
                }

                loop {
                    lexer.skip_lf();

                    if lexer.is_reached_eof() {
                        break;
                    }

                    let anchor = lexer.cast_anchor();

                    let section_ast = match section.parse(lexer, errors) {
                        Some(section) => section,
                        None => {
                            let error =
                                recover_until_or_lf(lexer, section.first_token_kinds(), section);
                            errors.push(error);

                            continue;
                        }
                    };

                    if !lexer.is_current_lf() {
                        let error = recover_until_or_lf(
                            lexer,
                            [GeneratorTokenKind::LineFeed].into_iter(),
                            &NamedParserPart::LINE_FEED,
                        );
                        errors.push(error);
                    }
                    lexer.skip_lf();

                    let mut key_values = Vec::new();

                    loop {
                        if lexer.is_reached_eof() {
                            break;
                        }

                        lexer.skip_lf();

                        let key_value = match key_value.parse(lexer, errors) {
                            Some(key_value) => key_value,
                            None => {
                                // next token must be start of section
                                if section
                                    .first_token_kinds()
                                    .any(|kind| lexer.current_contains(kind))
                                {
                                    break;
                                }

                                let error = recover_until_or_lf(
                                    lexer,
                                    key_value
                                        .first_token_kinds()
                                        .chain(section.first_token_kinds()),
                                    key_value,
                                );
                                errors.push(error);

                                if section
                                    .first_token_kinds()
                                    .any(|kind| lexer.current_contains(kind))
                                {
                                    break;
                                }

                                continue;
                            }
                        };

                        if !lexer.is_current_lf() && !lexer.is_reached_eof() {
                            let error = recover_until_or_lf(
                                lexer,
                                [GeneratorTokenKind::LineFeed].into_iter(),
                                &NamedParserPart::LINE_FEED,
                            );
                            errors.push(error);
                        }
                        lexer.skip_lf();

                        key_values.push(key_value);
                    }

                    sections.push((section_ast, key_values, anchor.elapsed(lexer)));
                }

                Some(LanguageAST::Section {
                    non_section_key_values,
                    sections,
                    span: anchor.elapsed(lexer),
                })
            }
            LanguageParser::Empty { empty_lexer: _ } => {
                lexer.next();

                Some(LanguageAST::Empty {
                    span: anchor.elapsed(lexer),
                })
            }
        }
    }

    #[auto_enum(Iterator)]
    fn first_token_kinds(&self) -> impl Iterator<Item = crate::lexer::GeneratorTokenKind> {
        match self {
            LanguageParser::Section {
                section,
                key_value: _,
                comments: _,
                allow_non_section_key_value: _,
            } => section.first_token_kinds(),
            LanguageParser::Empty { empty_lexer } => once(*empty_lexer),
        }
    }
}

impl ParserPart for LanguageParser {
    fn parse_error_code(&self) -> usize {
        0001
    }

    fn expected_format(&self) -> Option<std::borrow::Cow<'static, str>> {
        None
    }
}

impl<'input> AST<'input> for LanguageAST<'input> {
    fn span(&self) -> Range<usize> {
        match self {
            LanguageAST::Section {
                non_section_key_values: _,
                sections: _,
                span,
            } => span.clone(),
            LanguageAST::Empty { span } => span.clone(),
        }
    }

    fn take_value(
        &self,
        section_name_stack: &mut allocator_api2::vec::Vec<
            (Cow<'input, str>, Range<usize>, Range<usize>, bool),
            &bumpalo::Bump,
        >,
        validator: &mut ValueTypeChecker<'_, '_, '_, '_, 'input, 'input>,
    ) {
        if let ValueTree::Section {
            elements: _,
            name_span,
            define_span,
        } = &mut validator.value_tree
        {
            // set root span
            *name_span = self.span().as_utf8_byte_range();
            *define_span = self.span().as_utf8_byte_range();
        }

        match self {
            LanguageAST::Section {
                non_section_key_values,
                sections,
                span: _,
            } => {
                for key_value in non_section_key_values.iter() {
                    key_value.take_value(section_name_stack, validator);
                }

                for (section, key_values, span) in sections.iter() {
                    // stack this section
                    let stacked = if section.sections.is_empty() {
                        section_name_stack.push((
                            "unknown".into(),
                            section.span.clone(),
                            section.span.clone(),
                            false,
                        ));

                        1
                    } else {
                        section_name_stack.extend(
                            section
                                .sections
                                .iter()
                                .map(|literal| literal.to_section_name(span.clone()))
                                .flatten()
                                .enumerate()
                                .map(|(index, (literal, name_span, define_span, _))| {
                                    (
                                        literal,
                                        name_span,
                                        define_span,
                                        section.is_array && index == (section.sections.len() - 1),
                                    )
                                }),
                        );

                        section.sections.len()
                    };

                    validator.set_value(
                        section_name_stack.iter().map(
                            |(name, name_span, define_span, is_array)| {
                                (
                                    name.clone(),
                                    name_span.as_utf8_byte_range(),
                                    define_span.as_utf8_byte_range(),
                                    *is_array,
                                )
                            },
                        ),
                        SetValue::CreateSection,
                    );

                    for key_value in key_values.iter() {
                        key_value.take_value(section_name_stack, validator);
                    }

                    for _ in 0..stacked {
                        section_name_stack.pop().unwrap();
                    }
                }
            }
            LanguageAST::Empty { span: _ } => {}
        }
    }

    fn take_token(
        &self,
        tokens: &mut std::collections::BTreeMap<usize, (super::ASTTokenKind, Range<usize>)>,
    ) {
        match self {
            LanguageAST::Section {
                non_section_key_values,
                sections,
                span: _,
            } => {
                for key_value in non_section_key_values.iter() {
                    key_value.take_token(tokens);
                }

                for (section, key_values, _) in sections.iter() {
                    section.take_token(tokens);

                    for key_value in key_values.iter() {
                        key_value.take_token(tokens);
                    }
                }
            }
            LanguageAST::Empty { span: _ } => {}
        }
    }

    fn take_formatter_token(&self, tokens: &mut Vec<super::FormatterTokenInfo>) {
        match self {
            LanguageAST::Section {
                non_section_key_values,
                sections,
                span: _,
            } => {
                for key_value in non_section_key_values.iter() {
                    key_value.take_formatter_token(tokens);
                }

                for (section, key_values, _) in sections.iter() {
                    section.take_formatter_token(tokens);

                    for key_value in key_values.iter() {
                        key_value.take_formatter_token(tokens);
                    }
                }
            }
            LanguageAST::Empty { span: _ } => {}
        }
    }
}
