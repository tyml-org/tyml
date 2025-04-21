use std::ops::Range;

use allocator_api2::vec::Vec;
use tyml_validate::validate::ValueTypeChecker;

use crate::lexer::GeneratorTokenKind;

use super::{
    AST, NamedParserPart, Parser, ParserGenerator, ParserPart,
    error::{GeneratedParseError, recover_until_or_lf},
    key_value::{KeyValue, KeyValueAST, KeyValueParser},
    section::{Section, SectionAST, SectionParser},
};

#[derive(Debug)]
pub enum LanguageStyle {
    Section {
        section: Section,
        key_value: KeyValue,
    },
}

pub enum LanguageParser {
    Section {
        section: SectionParser,
        key_value: KeyValueParser,
    },
}

#[derive(Debug)]
pub enum LanguageAST<'input> {
    Section {
        sections: Vec<(SectionAST<'input>, Vec<KeyValueAST<'input>>)>,
    },
}

impl<'input> ParserGenerator<'input, LanguageAST<'input>, LanguageParser> for LanguageStyle {
    fn generate(&self, registry: &mut crate::lexer::TokenizerRegistry) -> LanguageParser {
        match self {
            LanguageStyle::Section { section, key_value } => LanguageParser::Section {
                section: section.generate(registry),
                key_value: key_value.generate(registry),
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
        let mut sections = Vec::new();

        match self {
            LanguageParser::Section { section, key_value } => loop {
                if lexer.is_reached_eof() {
                    break;
                }

                lexer.skip_lf();

                let section_ast = match section.parse(lexer, errors) {
                    Some(section) => section,
                    None => {
                        let error =
                            recover_until_or_lf(lexer, &[section.first_token_kind()], section);
                        errors.push(error);

                        continue;
                    }
                };

                if !lexer.is_current_lf() {
                    let error = recover_until_or_lf(
                        lexer,
                        &[GeneratorTokenKind::LineFeed],
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

                    let key_value = match key_value.parse(lexer, errors) {
                        Some(key_value) => key_value,
                        None => {
                            // next token must be start of section
                            if lexer.current_contains(section.first_token_kind()) {
                                break;
                            }

                            let error = recover_until_or_lf(
                                lexer,
                                &[key_value.first_token_kind(), section.first_token_kind()],
                                key_value,
                            );
                            errors.push(error);

                            if lexer.current_contains(section.first_token_kind()) {
                                break;
                            }

                            continue;
                        }
                    };

                    if !lexer.is_current_lf() {
                        let error = recover_until_or_lf(
                            lexer,
                            &[GeneratorTokenKind::LineFeed],
                            &NamedParserPart::LINE_FEED,
                        );
                        errors.push(error);
                    }
                    lexer.skip_lf();

                    key_values.push(key_value);
                }

                sections.push((section_ast, key_values));
            },
        }

        Some(LanguageAST::Section { sections })
    }

    fn first_token_kind(&self) -> crate::lexer::GeneratorTokenKind {
        match self {
            LanguageParser::Section {
                section,
                key_value: _,
            } => section.first_token_kind(),
        }
    }
}

impl ParserPart for LanguageParser {
    fn expected_message_key(&self) -> std::borrow::Cow<'static, str> {
        "_unused".into()
    }

    fn expected_format_key(&self) -> Option<std::borrow::Cow<'static, str>> {
        None
    }
}

impl<'input> AST<'input> for LanguageAST<'input> {
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
