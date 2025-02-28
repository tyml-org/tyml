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
    fn test() {
        let source = "
settings: {
    number = -3.65e-10
    binary = 0xFF
    string = \"aaaa\"
}

type Server {
    name: [ string | [ int | int ] ]
    ip: string
    port: int?
}
enum Enum {
    Element0
    Element1
}
";
        let mut lexer = Lexer::new(source);
        let allocator = Bump::new();
        let mut errors = Vec::new_in(&allocator);

        let ast = parse_defines(&mut lexer, &mut errors, &allocator);
        dbg!(ast);
    }
}
