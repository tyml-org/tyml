use std::ops::Range;

use allocator_api2::vec::Vec;
use tyml_validate::validate::ValueTypeChecker;

use super::{
    AST, Parser, ParserGenerator,
    error::{GeneratedParseError, recover_until},
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

pub enum LanguageAST {
    Section {
        sections: Vec<(SectionAST, Vec<KeyValueAST>)>,
    },
}

impl ParserGenerator<'_, LanguageAST, LanguageParser> for LanguageStyle {
    fn generate(&self, registry: &mut crate::lexer::TokenizerRegistry) -> LanguageParser {
        match self {
            LanguageStyle::Section { section, key_value } => LanguageParser::Section {
                section: section.generate(registry),
                key_value: key_value.generate(registry),
            },
        }
    }
}

impl<'input> Parser<'input, LanguageAST> for LanguageParser {
    fn parse(
        &self,
        lexer: &mut crate::lexer::GeneratorLexer<'input>,
        errors: &mut Vec<GeneratedParseError>,
    ) -> Option<LanguageAST>
    where
        Self: Sized,
    {
        let mut sections = Vec::new();

        match self {
            LanguageParser::Section { section, key_value } => loop {
                if lexer.is_reached_eof() {
                    break;
                }

                let section = match section.parse(lexer, errors) {
                    Some(section) => section,
                    None => {
                        let error = recover_until(lexer, &[section.first_token_kind()], section);
                        errors.push(error);

                        continue;
                    }
                };

                let mut key_values = Vec::new();

                loop {
                    if lexer.is_reached_eof() {
                        break;
                    }

                    let key_value = match key_value.parse(lexer, errors) {
                        Some(key_value) => key_value,
                        None => {
                            // next token must be start of section
                            

                            break;
                        },
                    };
                }
            },
        }

        Some(LanguageAST::Section { sections })
    }

    fn first_token_kind(&self) -> crate::lexer::GeneratorTokenKind {
        todo!()
    }

    fn expected_message_key(&self) -> std::borrow::Cow<'static, str> {
        "_unused".into()
    }

    fn expected_format_key(&self) -> Option<std::borrow::Cow<'static, str>> {
        None
    }
}

impl<'input> AST<'input> for LanguageAST {
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
