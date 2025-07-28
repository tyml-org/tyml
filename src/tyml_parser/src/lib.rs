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

    static SOURCE: &'static str = r#"
settings = {
    ip: string @regex '\d+\.\d+\.\d+\.\d+' and ( @length 0..<10 )
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

type Type {
    test: int
    test2: {
        test: int
    }
}

interface {
    function test(test1: Mode = "Debug", test2: Type = { test = 100, test2 = { test = 100 } }) -> Type {
        return {
            test = 100,
            test2 = {
                test = 200
            }
        }
    }
}
"#;

    #[test]
    fn parse_test() {
        let mut lexer = Lexer::new(SOURCE);
        let allocator = Bump::new();
        let mut errors = Vec::new_in(&allocator);

        let ast = parse_defines(&mut lexer, &mut errors, &allocator);
        dbg!(ast);
        dbg!(errors);
    }

    #[test]
    fn formatter() {
        let mut lexer = Lexer::new(SOURCE);
        let allocator = Bump::new();
        let mut errors = Vec::new_in(&allocator);
        let ast = parse_defines(&mut lexer, &mut errors, &allocator);

        let mut formatter = GeneralFormatter::new(
            Lexer::new(SOURCE)
                .enable_comment_token()
                .into_formatter_token(ast)
                .into_iter(),
            20,
        );
        formatter.format();

        println!("{}", formatter.generate_code());
    }
}
