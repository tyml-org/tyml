
pub mod lexer;

#[cfg(test)]
mod test {
    use crate::lexer::Lexer;


    #[test]
    fn test() {
        let source = 
"
settings: {
    number: int = -3.65e-10
    binary: int = 0xFF
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
