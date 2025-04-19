use std::ops::Range;

use allocator_api2::vec::Vec;
use tyml_validate::validate::ValueTypeChecker;

use crate::lexer::{GeneratorTokenKind, GeneratorTokenizer};

use super::{AST, Parser, ParserGenerator, error::GeneratedParseError, literal::Literal};

#[derive(Debug)]
pub struct Section {
    pub literal: Literal,
    pub kind: SectionKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SectionKind {
    /// [section]
    Bracket,
    /// [section1][section2]
    MultiBracket,
}

pub struct SectionParser {
    pub literal: GeneratorTokenKind,
    pub kind: SectionParserKind,
}

pub enum SectionParserKind {
    Bracket {
        bracket_left: GeneratorTokenKind,
        bracket_right: GeneratorTokenKind,
    },
    MultiBracket {
        bracket_left: GeneratorTokenKind,
        bracket_right: GeneratorTokenKind,
    },
}

pub struct SectionAST {}

impl ParserGenerator<'_, SectionAST, SectionParser> for Section {
    fn generate(&self, registry: &mut crate::lexer::TokenizerRegistry) -> SectionParser {
        let kind = match self.kind {
            SectionKind::Bracket => SectionParserKind::Bracket {
                bracket_left: registry.register(GeneratorTokenizer::Keyword("[".into())),
                bracket_right: registry.register(GeneratorTokenizer::Keyword("]".into())),
            },
            SectionKind::MultiBracket => SectionParserKind::MultiBracket {
                bracket_left: registry.register(GeneratorTokenizer::Keyword("[".into())),
                bracket_right: registry.register(GeneratorTokenizer::Keyword("]".into())),
            },
        };

        SectionParser {
            literal: self.literal.register(registry),
            kind,
        }
    }
}

impl Parser<'_, SectionAST> for SectionParser {
    fn parse(
        &self,
        lexer: &mut crate::lexer::GeneratorLexer<'_, '_>,
        errors: &mut Vec<GeneratedParseError>,
    ) -> Option<SectionAST> {
        todo!()
    }

    fn first_token_kind(&self) -> GeneratorTokenKind {
        match self.kind {
            SectionParserKind::Bracket {
                bracket_left,
                bracket_right: _,
            } => bracket_left,
            SectionParserKind::MultiBracket {
                bracket_left,
                bracket_right: _,
            } => bracket_left,
        }
    }

    fn expected_message_key(&self) -> std::borrow::Cow<'static, str> {
        match self.kind {
            SectionParserKind::Bracket {
                bracket_left: _,
                bracket_right: _,
            } => "expected.bracket_section".into(),
            SectionParserKind::MultiBracket {
                bracket_left: _,
                bracket_right: _,
            } => "expected.multi_bracket_section".into(),
        }
    }

    fn expected_format_key(&self) -> Option<std::borrow::Cow<'static, str>> {
        match self.kind {
            SectionParserKind::Bracket {
                bracket_left: _,
                bracket_right: _,
            } => Some("[section]".into()),
            SectionParserKind::MultiBracket {
                bracket_left: _,
                bracket_right: _,
            } => Some("[section1][section2]".into()),
        }
    }
}

impl<'input> AST<'input> for SectionAST {
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
