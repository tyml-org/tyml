pub mod ast;
pub mod error;
pub mod formatter;
pub mod lexer;
pub mod parser;

#[cfg(test)]
mod test {
    use allocator_api2::vec::Vec;
    use bumpalo::Bump;
    use tyml_formatter::GeneralFormatter;

    use crate::{formatter::IntoFormatterToken, lexer::Lexer, parser::parse_defines};

    #[test]
    fn parse_test() {
        let source = r#"
settings = {
    ip = "192.168.1.1"
    port = 25565
    mode = "Debug"
}
/* comment
*/
// comment
/// Document1
/// Document2
test: {
    mode: Mode
}

enum Mode {
    "Debug"
    "Release"
}
"#;
        let mut lexer = Lexer::new(source);
        let allocator = Bump::new();
        let mut errors = Vec::new_in(&allocator);

        let ast = parse_defines(&mut lexer, &mut errors, &allocator);
        dbg!(ast);
        dbg!(errors);
    }

    #[test]
    fn formatter() {
        let source = r#"
settings: {
    ip: string
    port: int
    mode: Mode?
}

/* comment
*/

// comment
/// Document1
/// Document2
test: {
    /// AAAA
    /// AAAA
    mode: Mode
}

enum Mode {
    "Debug"
    "Release"
}
        "#;

        let mut lexer = Lexer::new(source);
        let allocator = Bump::new();
        let mut errors = Vec::new_in(&allocator);
        let ast = parse_defines(&mut lexer, &mut errors, &allocator);

        let mut formatter = GeneralFormatter::new(
            Lexer::new(source)
                .enable_comment_token()
                .into_formatter_token(ast)
                .into_iter(),
            20,
        );
        formatter.format();

        println!("{}", formatter.generate_code());
    }
}
