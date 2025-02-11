
pub mod lexer;

#[cfg(test)]
mod test {
    use crate::lexer::Lexer;


    #[test]
    fn test() {
        let source = 
"
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
