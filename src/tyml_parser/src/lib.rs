pub mod ast;
pub mod error;
pub mod lexer;
pub mod parser;

#[cfg(test)]
mod test {
    use allocator_api2::vec::Vec;
    use bumpalo::Bump;

    use crate::{lexer::Lexer, parser::parse_defines};

    #[test]
    fn parse_test() {
        let source = "
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
    mode: Mode
}

enum Mode {
    \"Debug\"
    \"Release\"
}
";
        let mut lexer = Lexer::new(source);
        let allocator = Bump::new();
        let mut errors = Vec::new_in(&allocator);

        let ast = parse_defines(&mut lexer, &mut errors, &allocator);
        dbg!(ast);
        dbg!(errors);
    }
}
