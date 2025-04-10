use std::ops::Range;

use tyml_validate::validate::ValueTypeChecker;

use crate::lexer::GeneratorTokenKind;

use super::{AST, Parser, ParserGenerator, literal::Literal};

#[derive(Debug)]
pub struct Section {
    pub literal: Literal,
    pub kind: SectionKind,
}

#[derive(Debug)]
pub enum SectionKind {
    /// [section]
    Bracket,
    /// [section1][section2]
    MultiBracket,
}

pub struct SectionParser {
    pub literal: GeneratorTokenKind,
}

pub struct SectionAST {}

impl ParserGenerator<'_, SectionAST, SectionParser> for Section {
    fn generate(&self, registry: &mut crate::lexer::TokenizerRegistry) -> SectionParser {
        todo!()
    }
}

impl Parser<'_, SectionAST> for SectionParser {
    fn parse(lexer: &mut crate::lexer::GeneratorLexer<'_>) -> Option<SectionAST> {
        todo!()
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
