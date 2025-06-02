use std::{borrow::Cow, iter::once, ops::Range};

use allocator_api2::vec::Vec;
use serde::{Deserialize, Serialize};
use tyml_validate::validate::ValueTypeChecker;

use crate::lexer::{GeneratorTokenKind, GeneratorTokenizer};

use super::{
    AST, ASTTokenKind, Parser, ParserGenerator, ParserPart,
    error::{GeneratedParseError, recover_until_or_lf},
    literal::{LiteralSet, LiteralSetAST, LiteralSetParser},
};

#[derive(Debug, Serialize, Deserialize)]
pub struct Section {
    pub literal: LiteralSet,
    pub kind: SectionKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SectionKind {
    /// [section]
    Bracket {
        allow_space_split: bool,
        allow_double_bracket_array: bool,
    },
    /// [section1][section2]
    MultiBracket,
}

pub struct SectionParser {
    pub literal: LiteralSetParser,
    pub kind: SectionParserKind,
}

pub enum SectionParserKind {
    Bracket {
        bracket_left: GeneratorTokenKind,
        bracket_right: GeneratorTokenKind,
        dot: GeneratorTokenKind,
        allow_space_split: bool,
        allow_double_bracket_array: bool,
    },
    MultiBracket {
        bracket_left: GeneratorTokenKind,
        bracket_right: GeneratorTokenKind,
    },
}

#[derive(Debug)]
pub struct SectionAST<'input> {
    pub sections: Vec<LiteralSetAST<'input>>,
    pub is_array: bool,
    /// Only section name part span
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct SpannedSection<'input> {
    pub text: Cow<'input, str>,
    pub span: Range<usize>,
}

impl<'input> ParserGenerator<'input, SectionAST<'input>, SectionParser> for Section {
    fn generate(&self, registry: &mut crate::lexer::TokenizerRegistry) -> SectionParser {
        let kind = match self.kind {
            SectionKind::Bracket {
                allow_space_split,
                allow_double_bracket_array,
            } => SectionParserKind::Bracket {
                bracket_left: registry.register(GeneratorTokenizer::Keyword("[".into())),
                bracket_right: registry.register(GeneratorTokenizer::Keyword("]".into())),
                dot: registry.register(GeneratorTokenizer::Keyword(".".into())),
                allow_space_split,
                allow_double_bracket_array,
            },
            SectionKind::MultiBracket => SectionParserKind::MultiBracket {
                bracket_left: registry.register(GeneratorTokenizer::Keyword("[".into())),
                bracket_right: registry.register(GeneratorTokenizer::Keyword("]".into())),
            },
        };

        SectionParser {
            literal: self.literal.generate(registry),
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
                dot,
                allow_space_split,
                allow_double_bracket_array,
            } => {
                if !lexer.current_contains(bracket_left) {
                    return None;
                }
                lexer.next();

                let mut double_bracket = false;
                if allow_double_bracket_array && lexer.current_contains(bracket_left) {
                    // double bracket array array
                    lexer.next();
                    double_bracket = true;
                }

                let Some(literal) = self.literal.parse(lexer, errors) else {
                    return Some(SectionAST {
                        sections: Vec::new(),
                        is_array: false,
                        span: anchor.elapsed(lexer),
                    });
                };

                let mut sections = Vec::new();
                sections.push(literal);

                loop {
                    if allow_space_split {
                        if let Some(literal) = self.literal.parse(lexer, errors) {
                            sections.push(literal);

                            if lexer.current_contains(bracket_right) {
                                break;
                            }

                            continue;
                        }
                    }

                    if !lexer.current_contains(dot) {
                        break;
                    }
                    lexer.next();

                    let Some(literal) = self.literal.parse(lexer, errors) else {
                        let error = recover_until_or_lf(lexer, [bracket_right].into_iter(), self);
                        errors.push(error);

                        break;
                    };
                    sections.push(literal);

                    if lexer.current_contains(bracket_right) {
                        break;
                    }
                }

                if !lexer.current_contains(bracket_right) {
                    let error = recover_until_or_lf(lexer, [bracket_right].into_iter(), self);
                    errors.push(error);

                    if lexer.current_contains(bracket_right) {
                        lexer.next().unwrap();
                    }

                    return Some(SectionAST {
                        sections,
                        is_array: double_bracket,
                        span: anchor.elapsed(lexer),
                    });
                }
                lexer.next();

                if double_bracket {
                    if !lexer.current_contains(bracket_right) {
                        let error = recover_until_or_lf(lexer, [bracket_right].into_iter(), self);
                        errors.push(error);

                        if lexer.current_contains(bracket_right) {
                            lexer.next().unwrap();
                        }

                        return Some(SectionAST {
                            sections,
                            is_array: double_bracket,
                            span: anchor.elapsed(lexer),
                        });
                    }
                    lexer.next();
                }

                Some(SectionAST {
                    sections,
                    is_array: double_bracket,
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

                    let Some(literal) = self.literal.parse(lexer, errors) else {
                        let error = recover_until_or_lf(lexer, [bracket_right].into_iter(), self);
                        errors.push(error);

                        return Some(SectionAST {
                            sections: Vec::new(),
                            is_array: false,
                            span: anchor.elapsed(lexer),
                        });
                    };

                    sections.push(literal);

                    if !lexer.current_contains(bracket_right) {
                        let error = recover_until_or_lf(lexer, [bracket_right].into_iter(), self);
                        errors.push(error);

                        return Some(SectionAST {
                            sections: Vec::new(),
                            is_array: false,
                            span: anchor.elapsed(lexer),
                        });
                    }
                    lexer.next();
                }

                Some(SectionAST {
                    sections,
                    is_array: false,
                    span: anchor.elapsed(lexer),
                })
            }
        }
    }

    fn first_token_kinds(&self) -> impl Iterator<Item = GeneratorTokenKind> {
        once(match self.kind {
            SectionParserKind::Bracket {
                bracket_left,
                bracket_right: _,
                dot: _,
                allow_space_split: _,
                allow_double_bracket_array: _,
            } => bracket_left,
            SectionParserKind::MultiBracket {
                bracket_left,
                bracket_right: _,
            } => bracket_left,
        })
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
                dot: _,
                allow_space_split: _,
                allow_double_bracket_array: _,
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
            (Cow<'input, str>, Range<usize>, Range<usize>, bool),
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
                section.span().start,
                (ASTTokenKind::Section, section.span().clone()),
            );
        }
    }
}
