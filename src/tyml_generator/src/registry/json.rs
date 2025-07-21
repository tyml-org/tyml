use crate::style::{
    key_value::{KeyOption, KeyValue, KeyValueSeparatorKind},
    language::LanguageStyle,
    literal::{
        BoolKind, BoolLiteral, EscapeOption, FloatLiteral, InfNanKind, LiteralSet, QuotesKind,
        StringLiteral, UnicodeFormatKind,
    },
    value::{ArrayValue, InlineSection, InlineSectionKind, InlineSectionSeparator, Value},
};

pub fn json() -> LanguageStyle {
    LanguageStyle::Value(KeyValue {
        key: LiteralSet {
            normal: None,
            strings: vec![StringLiteral {
                quotes_kind: QuotesKind::DoubleQuotes,
                escape: EscapeOption {
                    allow_escape: true,
                    unicode: UnicodeFormatKind::Normal,
                },
            }],
            custom: None,
        },
        key_option: KeyOption {
            allow_dot_section_split: false,
        },
        separator: KeyValueSeparatorKind::Colon,
        value: Value {
            strings: vec![StringLiteral {
                quotes_kind: QuotesKind::DoubleQuotes,
                escape: EscapeOption {
                    allow_escape: true,
                    unicode: UnicodeFormatKind::Normal,
                },
            }],
            float: Some(FloatLiteral {
                allow_e: true,
                inf_nan_kind: InfNanKind::None,
                allow_plus_minus: true,
                allow_under_line: false,
            }),
            binary: None,
            bool: Some(BoolLiteral {
                kind: BoolKind::LowerCase,
            }),
            any_string: None,
            array: Some(ArrayValue::Bracket {
                allow_line_feed: true,
                allow_extra_comma: false,
            }),
            inline_section: Some(InlineSection {
                kind: InlineSectionKind::Brace,
                separator: InlineSectionSeparator::Comma {
                    allow_line_feed: true,
                    allow_extra_comma: false,
                },
            }),
        },
    })
}
