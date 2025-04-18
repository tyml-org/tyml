use std::ops::Range;

use tyml_validate::validate::ValueTypeChecker;

use super::{
    AST, Parser, ParserGenerator,
    key_value::{KeyValue, KeyValueParser},
    section::{Section, SectionParser},
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

pub enum LanguageAST {}

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
    fn parse(&self, lexer: &mut crate::lexer::GeneratorLexer<'input>) -> Option<LanguageAST>
    where
        Self: Sized,
    {
        match self {
            LanguageParser::Section { section, key_value } => {
                loop {
                    let section = match section.parse(lexer) {
                        Some(section) => section,
                        None => {
                            let error = 
                        },
                    };
                }
            },
        }
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
