use extension_fn::extension_fn;
use tyml_formatter::{FormatterToken, FormatterTokenKind, SpaceFormat};

use crate::lexer::{Lexer, TokenKind};

#[extension_fn(<'input> Lexer<'input>)]
pub fn into_formatter_token(self) -> impl Iterator<Item = FormatterToken<'input>> {
    self.map(|token| match token.kind {
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
            left_space: SpaceFormat::None,
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
            left_space: SpaceFormat::None,
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
            left_space: SpaceFormat::None,
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
            left_space: SpaceFormat::None,
            right_space: SpaceFormat::Space,
        },
        TokenKind::Or => FormatterToken {
            text: token.text.into(),
            kind: FormatterTokenKind::Normal,
            left_space: SpaceFormat::None,
            right_space: SpaceFormat::Space,
        },
        TokenKind::LineFeed => FormatterToken {
            text: token.text.into(),
            kind: FormatterTokenKind::LineFeed,
            left_space: SpaceFormat::None,
            right_space: SpaceFormat::None,
        },
        _ => FormatterToken {
            text: token.text.into(),
            kind: FormatterTokenKind::Normal,
            left_space: SpaceFormat::None,
            right_space: SpaceFormat::None,
        },
    })
}
