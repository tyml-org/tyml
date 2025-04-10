use std::ops::Range;

use allocator_api2::vec::Vec;
use bumpalo::Bump;
use tyml_validate::validate::ValueTypeChecker;

use crate::lexer::{GeneratorLexer, TokenizerRegistry};

pub mod key_value;
pub mod language;
pub mod literal;
pub mod section;
pub mod value;

pub trait ParserGenerator<'input, T: AST<'input>, P: Parser<'input, T>> {
    fn generate(&self, registry: &mut TokenizerRegistry) -> P;
}

pub trait Parser<'input, T: AST<'input>> {
    fn parse(lexer: &mut GeneratorLexer<'input>) -> Option<T>;
}

pub trait AST<'input> {
    /// UTF-8 byte span
    fn span() -> Range<usize>;

    fn take_value(
        &self,
        validator: &mut ValueTypeChecker<'_, '_, '_, '_, 'input, 'input>,
        section_name_stack: &mut Vec<&'input str, &Bump>,
    );
}
