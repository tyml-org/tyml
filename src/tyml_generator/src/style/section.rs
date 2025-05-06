use std::{borrow::Cow, ops::Range};

use allocator_api2::vec::Vec;
use serde::{Deserialize, Serialize};
use tyml_validate::validate::ValueTypeChecker;

use crate::lexer::{GeneratorTokenKind, GeneratorTokenizer, SpannedText};

use super::{
    AST, ASTTokenKind, Parser, ParserGenerator, ParserPart,
    error::{GeneratedParseError, recover_until_or_lf},
    literal::{CustomLiteralOption, Literal},
};

#[derive(Debug, Serialize, Deserialize)]
pub struct Section {
    pub literal: Literal,
    pub kind: SectionKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SectionKind {
    /// [section]
    Bracket,
    /// [section1][section2]
    MultiBracket,
}

pub struct SectionParser {
    pub literal: (GeneratorTokenKind, Option<CustomLiteralOption>),
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
    pub sections: Vec<SpannedText<'input>>,
    pub literal_option: Option<CustomLiteralOption>,
    /// Only section name part span
    pub span: Range<usize>,
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
        let anchor = lexer.cast_anchor();

        match self.kind {
            SectionParserKind::Bracket {
                bracket_left,
                bracket_right,
            } => {
                if !lexer.current_contains(bracket_left) {
                    return None;
                }
                lexer.next();

                let literal = match lexer.current_contains(self.literal.0) {
                    true => lexer.next().unwrap(),
                    false => {
                        let error = recover_until_or_lf(lexer, &[bracket_right], self);
                        errors.push(error);

                        return Some(SectionAST {
                            sections: Vec::new(),
                            literal_option: self.literal.1.clone(),
                            span: anchor.elapsed(lexer),
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
                        literal_option: self.literal.1.clone(),
                        span: anchor.elapsed(lexer),
                    });
                }
                lexer.next();

                let mut sections = Vec::new();
                sections.push(literal.into_spanned());

                Some(SectionAST {
                    sections,
                    literal_option: self.literal.1.clone(),
                    span: anchor.elapsed(lexer),
                })
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

                    let literal = match lexer.current_contains(self.literal.0) {
                        true => lexer.next().unwrap(),
                        false => {
                            let error = recover_until_or_lf(lexer, &[bracket_right], self);
                            errors.push(error);

                            return Some(SectionAST {
                                sections: Vec::new(),
                                literal_option: self.literal.1.clone(),
                                span: anchor.elapsed(lexer),
                            });
                        }
                    };

                    sections.push(literal.into_spanned());

                    if !lexer.current_contains(bracket_right) {
                        let error = recover_until_or_lf(lexer, &[bracket_right], self);
                        errors.push(error);

                        return Some(SectionAST {
                            sections: Vec::new(),
                            literal_option: self.literal.1.clone(),
                            span: anchor.elapsed(lexer),
                        });
                    }
                    lexer.next();
                }

                Some(SectionAST {
                    sections,
                    literal_option: self.literal.1.clone(),
                    span: anchor.elapsed(lexer),
                })
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
    fn parse_error_code(&self) -> usize {
        0003
    }

    fn expected_format(&self) -> Option<std::borrow::Cow<'static, str>> {
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

impl<'input> AST<'input> for SectionAST<'input> {
    fn span(&self) -> Range<usize> {
        self.span.clone()
    }

    fn take_value(
        &self,
        _: &mut allocator_api2::vec::Vec<
            (Cow<'input, str>, Range<usize>, Range<usize>),
            &bumpalo::Bump,
        >,
        _: &mut ValueTypeChecker<'_, '_, '_, '_, 'input, 'input>,
    ) {
        unreachable!()
    }

    fn take_token(
        &self,
        tokens: &mut std::collections::BTreeMap<usize, (super::ASTTokenKind, Range<usize>)>,
    ) {
        for section in self.sections.iter() {
            tokens.insert(
                section.span.start,
                (ASTTokenKind::Section, section.span.clone()),
            );
        }
    }
}
