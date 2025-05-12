pub mod lexer;
pub mod registry;
pub mod style;

#[cfg(test)]
mod test {
    use std::{fs::OpenOptions, io::Write, ops::Deref};

    use allocator_api2::vec::Vec;
    use bumpalo::Bump;

    use crate::{
        lexer::{GeneratorLexer, TokenizerRegistry},
        registry::STYLE_REGISTRY,
        style::{Parser, ParserGenerator},
    };

    #[test]
    fn generate() {
        let language = STYLE_REGISTRY.resolve("ini").unwrap();

        let toml = toml::to_string_pretty(language.deref()).unwrap();
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
#
[section]
key1 = value
kay2 = value
#";

        let allocator = Bump::new();

        let mut lexer = GeneratorLexer::new(source, &registry, &allocator);

        let mut errors = Vec::new();

        let ast = parser.parse(&mut lexer, &mut errors).unwrap();

        dbg!(ast);
        dbg!(errors);
    }
}
