use std::{borrow::Cow, ops::Range};

use allocator_api2::vec::Vec;
use bumpalo::Bump;
use error::GeneratedParseError;
use tyml_validate::validate::ValueTypeChecker;

use crate::lexer::{GeneratorLexer, GeneratorTokenKind, TokenizerRegistry};

pub mod error;
pub mod key_value;
pub mod language;
pub mod literal;
pub mod section;
pub mod value;

pub trait ParserGenerator<'input, T: AST<'input>, P: Parser<'input, T>> {
    fn generate(&self, registry: &mut TokenizerRegistry) -> P;
}

pub trait Parser<'input, T: AST<'input>> {
    fn parse(
        &self,
        lexer: &mut GeneratorLexer<'input>,
        errors: &mut Vec<GeneratedParseError>,
    ) -> Option<T>;

    fn first_token_kind(&self) -> GeneratorTokenKind;

    fn expected_message_key(&self) -> Cow<'static, str>;

    fn expected_format_key(&self) -> Option<Cow<'static, str>>;
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
