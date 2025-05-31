use crate::style::{
    comment::Comment,
    key_value::{KeyOption, KeyValue, KeyValueKind},
    language::{LanguageStyle, SectionStyle},
    literal::{
        BinaryLiteral, BoolKind, BoolLiteral, EscapeOption, FloatLiteral, InfNanKind, LiteralSet,
        NormalLiteral, QuotesKind, StringLiteral, UnicodeFormatKind,
    },
    section::{Section, SectionKind},
    value::Value,
};

pub fn toml() -> LanguageStyle {
    let literal = NormalLiteral {
        allow_line: true,
        symbol_regex: None,
    };

    let double_quotes_string = StringLiteral {
        quotes_kind: QuotesKind::DoubleQuotes,
        escape: EscapeOption {
            allow_escape: true,
            unicode: UnicodeFormatKind::Normal,
        },
    };
    let single_quote_string = StringLiteral {
        quotes_kind: QuotesKind::SingleQuote,
        escape: EscapeOption {
            allow_escape: true,
            unicode: UnicodeFormatKind::Normal,
        },
    };

    let section_key_literal = LiteralSet {
        normal: Some(literal.clone()),
        strings: vec![double_quotes_string.clone(), single_quote_string.clone()],
        custom: None,
    };

    LanguageStyle::Section(SectionStyle {
        section: Section {
            literal: section_key_literal.clone(),
            kind: SectionKind::Bracket {
                allow_space_split: true,
                allow_double_bracket_array: true,
            },
        },
        key_value: KeyValue {
            key: section_key_literal,
            kind: KeyValueKind::Equal,
            value: Value {
                strings: vec![double_quotes_string, single_quote_string],
                float: Some(FloatLiteral {
                    allow_e: true,
                    inf_nan_kind: InfNanKind::LowerCase,
                    allow_plus_minus: true,
                    allow_under_line: true,
                }),
                binary: Some(BinaryLiteral {
                    allow_bit: true,
                    allow_oct: true,
                    allow_hex: true,
                    allow_plus_minus: true,
                    allow_under_line: true,
                }),
                bool: Some(BoolLiteral {
                    kind: BoolKind::LowerCase,
                }),
                ..Default::default()
            },
            key_option: KeyOption {
                allow_dot_section_split: false,
            },
        },
        comments: vec![Comment::Line {
            start: "#".to_string(),
        }],
    })
}
