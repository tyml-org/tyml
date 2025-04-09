use crate::lexer::GeneratorLexer;

pub mod key_value_style;
pub mod language_style;
pub mod section_style;

pub trait Parser<'input> {
    fn parse(lexer: &mut GeneratorLexer<'input>) -> Option<Self>
    where
        Self: Sized;
}
