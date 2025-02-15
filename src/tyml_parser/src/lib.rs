use ast::Defines;
use bumpalo::Bump;
use parser::parse_tyml;

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod error;

pub struct Tyml<'input, 'allocator> {
    source_code: &'input str,
    ast: Option<&'allocator Defines<'input, 'allocator>>,
    allocator: Bump,
}

pub struct IncompleteTyml<'input, 'allocator> {
    tyml: Tyml<'input, 'allocator>,
}

impl<'input, 'allocator> Tyml<'input, 'allocator> {
    pub fn parse(
        source_code: &'input str,
    ) -> Result<Tyml<'input, 'allocator>, IncompleteTyml<'input, 'allocator>> {
        parse_tyml(source_code)
    }
}

#[cfg(test)]
mod test {

    use crate::lexer::Lexer;

    #[test]
    fn test() {
        let source = "
settings: {
    number = -3.65e-10
    binary = 0xFF
    string = \"aaaa\"
}

type Type {}
enum Enum {}
";
        let lexer = Lexer::new(source);

        for token in lexer {
            dbg!(token);
        }
    }
}
