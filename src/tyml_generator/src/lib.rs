pub mod lexer;
pub mod style;

#[cfg(test)]
mod test {
    use allocator_api2::{vec, vec::Vec};
    use bumpalo::Bump;

    use crate::{
        lexer::{GeneratorLexer, TokenizerRegistry},
        style::{
            Parser, ParserGenerator,
            key_value::{KeyValue, KeyValueKind},
            language::LanguageStyle,
            literal::{CustomRegexLiteral, Literal},
            section::{Section, SectionKind},
            value::Value,
        },
    };

    #[test]
    fn generate() {
        let ini_literal = CustomRegexLiteral {
            regex: r"[^\[\]\n\r=;]+".into(),
        };

        let literal = Literal::Custom(ini_literal.clone());

        let any_string_literal = Literal::Custom(ini_literal);

        let language = LanguageStyle::Section {
            section: Section {
                literal: literal.clone(),
                kind: SectionKind::Bracket,
            },
            key_value: KeyValue {
                key: literal.clone(),
                kind: KeyValueKind::Equal,
                value: Value {
                    any_string: Some(vec![any_string_literal]),
                    ..Default::default()
                },
            },
        };

        let mut registry = TokenizerRegistry::new();

        let parser = language.generate(&mut registry);

        registry.freeze();

        let source = "
[section]
key = value
";

        let allocator = Bump::new();

        let mut lexer = GeneratorLexer::new(source, &registry, &allocator);

        let mut errors = Vec::new();

        let ast = parser.parse(&mut lexer, &mut errors).unwrap();

        dbg!(ast);
        dbg!(errors);
    }
}
