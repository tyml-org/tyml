use style::{
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

pub mod lexer;
pub mod style;

pub fn _ini_file_define() -> LanguageStyle {
    let ini_literal = CustomRegexLiteral {
        regex: r"[^ 　\t\[\]\n\r=;][^\[\]\n\r=;]+".into(),
        option: CustomLiteralOption {
            trim_space: true,
            escape: EscapeOption {
                allow_escape: true,
                unicode: UnicodeFormatKind::Normal,
            },
        },
    };

    let literal = Literal::Custom(ini_literal.clone());

    let any_string_literal = Literal::Custom(ini_literal);

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
                any_string: Some(any_string_literal),
                ..Default::default()
            },
        },
        comments: vec![Comment::Hash],
    })
}

#[cfg(test)]
mod test {
    use std::{fs::OpenOptions, io::Write};

    use allocator_api2::vec::Vec;
    use bumpalo::Bump;

    use crate::{
        _ini_file_define,
        lexer::{GeneratorLexer, TokenizerRegistry},
        style::{Parser, ParserGenerator},
    };

    #[test]
    fn generate() {
        let language = _ini_file_define();

        let toml = toml::to_string_pretty(&language).unwrap();
        let mut file = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open("ini.toml")
            .unwrap();

        file.write_all(toml.as_bytes()).unwrap();
        file.flush().unwrap();

        let mut registry = TokenizerRegistry::new();

        let parser = language.generate(&mut registry);

        registry.freeze();

        let source = "
[section]
key1 = value
key2 = value
";

        let allocator = Bump::new();

        let mut lexer = GeneratorLexer::new(source, &registry, &allocator);

        let mut errors = Vec::new();

        let ast = parser.parse(&mut lexer, &mut errors).unwrap();

        dbg!(ast);
        dbg!(errors);
    }
}
