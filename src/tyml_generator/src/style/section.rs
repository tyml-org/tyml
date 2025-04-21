use std::ops::Range;

use allocator_api2::vec::Vec;
use tyml_validate::validate::ValueTypeChecker;

use crate::lexer::{GeneratorTokenKind, GeneratorTokenizer, SpannedText};

use super::{
    AST, Parser, ParserGenerator, ParserPart,
    error::{GeneratedParseError, recover_until_or_lf},
    literal::Literal,
};

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

#[derive(Debug)]
pub struct SectionAST<'input> {
    sections: Vec<SpannedText<'input>>,
}

impl<'input> ParserGenerator<'input, SectionAST<'input>, SectionParser> for Section {
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

impl<'input> Parser<'input, SectionAST<'input>> for SectionParser {
    fn parse(
        &self,
        lexer: &mut crate::lexer::GeneratorLexer<'input, '_>,
        errors: &mut Vec<GeneratedParseError>,
    ) -> Option<SectionAST<'input>> {
        match self.kind {
            SectionParserKind::Bracket {
                bracket_left,
                bracket_right,
            } => {
                if !lexer.current_contains(bracket_left) {
                    return None;
                }
                lexer.next();

                let literal = match lexer.current_contains(self.literal) {
                    true => lexer.next().unwrap(),
                    false => {
                        let error = recover_until_or_lf(lexer, &[bracket_right], self);
                        errors.push(error);

                        return Some(SectionAST {
                            sections: Vec::new(),
                        });
                    }
                };

                if !lexer.current_contains(bracket_right) {
                    let error = recover_until_or_lf(lexer, &[bracket_right], self);
                    errors.push(error);

                    if lexer.current_contains(bracket_right) {
                        lexer.next().unwrap();
                    }

                    return Some(SectionAST {
                        sections: Vec::new(),
                    });
                }
                lexer.next();

                let mut sections = Vec::new();
                sections.push(literal.into_spanned());

                Some(SectionAST { sections })
            }
            SectionParserKind::MultiBracket {
                bracket_left,
                bracket_right,
            } => {
                if !lexer.current_contains(bracket_left) {
                    return None;
                }

                let mut sections = Vec::new();

                loop {
                    if !lexer.current_contains(bracket_left) {
                        break;
                    }
                    lexer.next();

                    let literal = match lexer.current_contains(self.literal) {
                        true => lexer.next().unwrap(),
                        false => {
                            let error = recover_until_or_lf(lexer, &[bracket_right], self);
                            errors.push(error);

                            return Some(SectionAST {
                                sections: Vec::new(),
                            });
                        }
                    };

                    sections.push(literal.into_spanned());

                    if !lexer.current_contains(bracket_right) {
                        let error = recover_until_or_lf(lexer, &[bracket_right], self);
                        errors.push(error);

                        return Some(SectionAST {
                            sections: Vec::new(),
                        });
                    }
                    lexer.next();
                }

                Some(SectionAST { sections })
            }
        }
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
}

impl ParserPart for SectionParser {
    fn expected_message_key(&self) -> std::borrow::Cow<'static, str> {
        match self.kind {
            SectionParserKind::Bracket {
                bracket_left: _,
                bracket_right: _,
            } => "expected.message.bracket_section".into(),
            SectionParserKind::MultiBracket {
                bracket_left: _,
                bracket_right: _,
            } => "expected.message.multi_bracket_section".into(),
        }
    }

    fn expected_format_key(&self) -> Option<std::borrow::Cow<'static, str>> {
        match self.kind {
            SectionParserKind::Bracket {
                bracket_left: _,
                bracket_right: _,
            } => Some("expected.format.bracket_section".into()),
            SectionParserKind::MultiBracket {
                bracket_left: _,
                bracket_right: _,
            } => Some("expected.format.multi_bracket_section".into()),
        }
    }
}

impl<'input> AST<'input> for SectionAST<'input> {
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
