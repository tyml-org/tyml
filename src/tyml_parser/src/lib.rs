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

#[test = "aaa"]
interface Test {
    #[test = true]
    function test(test1: Mode = "Debug") -> Type throws default: Type {
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
            25,
        );
        formatter.format();

        println!("{}", formatter.generate_code());
    }

    static COOKIE_SOURCE: &'static str = r#"
type Token {
    access_token: string
    expires_in: int
}

interface Auth {
    cookie function refresh() -> Token

    authed cookie function rotate(@claim: Token) -> Token

    function login() -> Token
}
"#;

    #[test]
    fn cookie_parse_test() {
        let mut lexer = Lexer::new(COOKIE_SOURCE);
        let allocator = Bump::new();
        let mut errors = Vec::new_in(&allocator);

        let ast = parse_defines(&mut lexer, &mut errors, &allocator);

        assert!(errors.is_empty(), "parse errors: {:?}", errors);

        let mut found_refresh_cookie = false;
        let mut found_rotate_authed_cookie = false;
        let mut found_login_no_modifier = false;

        for define in ast.defines.iter() {
            if let crate::ast::Define::Interface(interface) = define {
                for function in interface.functions.iter() {
                    match function.name.value {
                        "refresh" => {
                            assert!(function.cookie.is_some(), "refresh must be cookie");
                            assert!(function.authed.is_none(), "refresh must not be authed");
                            found_refresh_cookie = true;
                        }
                        "rotate" => {
                            assert!(function.cookie.is_some(), "rotate must be cookie");
                            assert!(function.authed.is_some(), "rotate must be authed");
                            found_rotate_authed_cookie = true;
                        }
                        "login" => {
                            assert!(function.cookie.is_none(), "login must not be cookie");
                            assert!(function.authed.is_none(), "login must not be authed");
                            found_login_no_modifier = true;
                        }
                        _ => {}
                    }
                }
            }
        }

        assert!(found_refresh_cookie);
        assert!(found_rotate_authed_cookie);
        assert!(found_login_no_modifier);
    }

    #[test]
    fn cookie_formatter_test() {
        let mut lexer = Lexer::new(COOKIE_SOURCE);
        let allocator = Bump::new();
        let mut errors = Vec::new_in(&allocator);
        let ast = parse_defines(&mut lexer, &mut errors, &allocator);

        let mut formatter = GeneralFormatter::new(
            Lexer::new(COOKIE_SOURCE)
                .enable_comment_token()
                .into_formatter_token(ast)
                .into_iter(),
            25,
        );
        formatter.format();

        let formatted = formatter.generate_code();
        println!("{}", formatted);

        assert!(formatted.contains("cookie function refresh"));
        assert!(formatted.contains("authed cookie function rotate"));
    }
}
