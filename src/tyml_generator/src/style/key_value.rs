use std::ops::Range;

use tyml_validate::validate::ValueTypeChecker;

use super::{AST, Parser, ParserGenerator, literal::Literal, value::Value};

#[derive(Debug)]
pub struct KeyValue {
    pub key: Literal,
    pub value: Value,
}

pub struct KeyValueParser {}

pub struct KeyValueAST {}

impl<'input> ParserGenerator<'input, KeyValueAST, KeyValueParser> for KeyValue {
    fn generate(&self, registry: &mut crate::lexer::TokenizerRegistry) -> KeyValueParser {
        todo!()
    }
}

impl<'input> Parser<'input, KeyValueAST> for KeyValueParser {
    fn parse(lexer: &mut crate::lexer::GeneratorLexer<'input>) -> Option<KeyValueAST> {
        todo!()
    }
}

impl<'input> AST<'input> for KeyValueAST {
    fn span() -> Range<usize> {
        todo!()
    }

    fn take_value(
        &self,
        validator: &mut ValueTypeChecker<'_, '_, '_, '_, 'input, 'input>,
        section_name_stack: &mut allocator_api2::vec::Vec<&'input str, &bumpalo::Bump>,
    ) {
        todo!()
    }
}
