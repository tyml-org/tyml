use crate::style::{
    comment::Comment,
    key_value::{KeyValue, KeyValueKind},
    language::{LanguageStyle, SectionStyle},
    literal::{
        CustomLiteralOption, CustomRegexLiteral, EscapeOption, FloatLiteral, InfNanKind, Literal,
        UnicodeFormatKind,
    },
    section::{Section, SectionKind},
    value::Value,
};

pub fn ini() -> LanguageStyle {
    let literal = Literal::Custom(CustomRegexLiteral {
        regex: r"[^ 　\t\[\]\n\r=;][^\[\]\n\r=;]*".into(),
        option: CustomLiteralOption {
            trim_space: true,
            escape: EscapeOption {
                allow_escape: true,
                unicode: UnicodeFormatKind::Normal,
            },
        },
    });

    LanguageStyle::Section(SectionStyle {
        section: Section {
            literal: literal.clone(),
            kind: SectionKind::Bracket,
        },
        key_value: KeyValue {
            key: literal.clone(),
            kind: KeyValueKind::Equal,
            value: Value {
                float: Some(FloatLiteral {
                    allow_e: true,
                    inf_nan_kind: InfNanKind::Insensitive,
                    allow_plus_minus: true,
                    allow_under_line: true,
                }),
                any_string: Some(literal),
                ..Default::default()
            },
        },
        comments: vec![
            Comment::Line {
                start: ";".to_string(),
            },
            Comment::Line {
                start: "#".to_string(),
            },
        ],
    })
}
