
pub mod lexer;

#[cfg(test)]
mod test {
    use crate::lexer::Lexer;


    #[test]
    fn test() {
        let source = 
"
settings: {
    number: int = 100
}
";
        let lexer = Lexer::new(source);

        for token in lexer {
            dbg!(token);
        }
    }

}
