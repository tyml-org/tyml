use std::collections::HashSet;

use extension_fn::extension_fn;
use tyml_formatter::{FormatterToken, FormatterTokenKind, SpaceFormat};

use crate::{
    ast::{Define, Defines, TypeDefine},
    lexer::{Lexer, TokenKind},
};

#[extension_fn(<'input> Lexer<'input>)]
pub fn into_formatter_token(self, ast: &Defines) -> Vec<FormatterToken<'input>> {
    let comma_positions = collect_comma_position(ast);

    self.into_iter()
        .map(|token| match token.kind {
            TokenKind::Colon => FormatterToken {
                text: token.text.into(),
                kind: FormatterTokenKind::Normal,
                left_space: SpaceFormat::None,
                right_space: SpaceFormat::Space,
            },
            TokenKind::Equal => FormatterToken {
                text: token.text.into(),
                kind: FormatterTokenKind::Normal,
                left_space: SpaceFormat::Space,
                right_space: SpaceFormat::Space,
            },
            TokenKind::Comma => FormatterToken {
                text: token.text.into(),
                kind: FormatterTokenKind::Normal,
                left_space: SpaceFormat::LineFeedOrSplit(","),
                right_space: SpaceFormat::SpaceOrLineFeed,
            },
            TokenKind::BraceLeft => FormatterToken {
                text: token.text.into(),
                kind: FormatterTokenKind::TreeIn,
                left_space: SpaceFormat::Space,
                right_space: SpaceFormat::SpaceOrLineFeed,
            },
            TokenKind::BraceRight => FormatterToken {
                text: token.text.into(),
                kind: FormatterTokenKind::TreeOut,
                left_space: SpaceFormat::SpaceOrLineFeed,
                right_space: SpaceFormat::None,
            },
            TokenKind::BracketLeft => FormatterToken {
                text: token.text.into(),
                kind: FormatterTokenKind::TreeIn,
                left_space: SpaceFormat::Space,
                right_space: SpaceFormat::SpaceOrLineFeed,
            },
            TokenKind::BracketRight => FormatterToken {
                text: token.text.into(),
                kind: FormatterTokenKind::TreeOut,
                left_space: SpaceFormat::SpaceOrLineFeed,
                right_space: SpaceFormat::None,
            },
            TokenKind::ParenthesisLeft => FormatterToken {
                text: token.text.into(),
                kind: FormatterTokenKind::TreeIn,
                left_space: SpaceFormat::Space,
                right_space: SpaceFormat::SpaceOrLineFeed,
            },
            TokenKind::ParenthesisRight => FormatterToken {
                text: token.text.into(),
                kind: FormatterTokenKind::TreeOut,
                left_space: SpaceFormat::SpaceOrLineFeed,
                right_space: SpaceFormat::None,
            },
            TokenKind::VerticalLine => FormatterToken {
                text: token.text.into(),
                kind: FormatterTokenKind::Normal,
                left_space: SpaceFormat::Space,
                right_space: SpaceFormat::Space,
            },
            TokenKind::Type => FormatterToken {
                text: token.text.into(),
                kind: FormatterTokenKind::Normal,
                left_space: SpaceFormat::None,
                right_space: SpaceFormat::Space,
            },
            TokenKind::Enum => FormatterToken {
                text: token.text.into(),
                kind: FormatterTokenKind::Normal,
                left_space: SpaceFormat::None,
                right_space: SpaceFormat::Space,
            },
            TokenKind::And => FormatterToken {
                text: token.text.into(),
                kind: FormatterTokenKind::Normal,
                left_space: SpaceFormat::Space,
                right_space: SpaceFormat::Space,
            },
            TokenKind::Or => FormatterToken {
                text: token.text.into(),
                kind: FormatterTokenKind::Normal,
                left_space: SpaceFormat::Space,
                right_space: SpaceFormat::Space,
            },
            TokenKind::AtValue => FormatterToken {
                text: token.text.into(),
                kind: FormatterTokenKind::Normal,
                left_space: SpaceFormat::Space,
                right_space: SpaceFormat::Space,
            },
            TokenKind::AtLength => FormatterToken {
                text: token.text.into(),
                kind: FormatterTokenKind::Normal,
                left_space: SpaceFormat::Space,
                right_space: SpaceFormat::Space,
            },
            TokenKind::AtU8Size => FormatterToken {
                text: token.text.into(),
                kind: FormatterTokenKind::Normal,
                left_space: SpaceFormat::Space,
                right_space: SpaceFormat::Space,
            },
            TokenKind::AtRegex => FormatterToken {
                text: token.text.into(),
                kind: FormatterTokenKind::Normal,
                left_space: SpaceFormat::Space,
                right_space: SpaceFormat::Space,
            },
            TokenKind::LineFeed => FormatterToken {
                text: token.text.into(),
                kind: FormatterTokenKind::LineFeed,
                left_space: SpaceFormat::None,
                right_space: SpaceFormat::None,
            },
            TokenKind::Comment => FormatterToken {
                text: token.text.into(),
                kind: FormatterTokenKind::CommentOrDocument,
                left_space: SpaceFormat::None,
                right_space: SpaceFormat::None,
            },
            TokenKind::Document => FormatterToken {
                text: token.text.into(),
                kind: FormatterTokenKind::CommentOrDocument,
                left_space: SpaceFormat::None,
                right_space: SpaceFormat::None,
            },
            _ => {
                if comma_positions.contains(&token.span.end) {
                    FormatterToken {
                        text: token.text.into(),
                        kind: FormatterTokenKind::Normal,
                        left_space: SpaceFormat::None,
                        right_space: SpaceFormat::LineFeedOrSplit(","),
                    }
                } else {
                    FormatterToken {
                        text: token.text.into(),
                        kind: FormatterTokenKind::Normal,
                        left_space: SpaceFormat::None,
                        right_space: SpaceFormat::None,
                    }
                }
            }
        })
        .collect()
}

fn collect_comma_position(ast: &Defines) -> HashSet<usize> {
    let mut positions = HashSet::new();

    collect_defines(ast, &mut positions);

    positions
}

fn collect_defines(ast: &Defines, positions: &mut HashSet<usize>) {
    for element in ast.defines.iter() {
        match element {
            Define::Element(element_define) => {
                positions.insert(element_define.span.end);

                if let Some(inline_type) = &element_define.inline_type {
                    collect_defines(&inline_type.defines, positions);
                }
            }
            Define::Type(type_define) => {
                if let TypeDefine::Enum(enum_define) = type_define {
                    for element in enum_define.elements.iter() {
                        positions.insert(element.span.end);
                    }
                }
            }
        }
    }
}
