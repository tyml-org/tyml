use crate::lexer::GeneratorTokenKind;

use super::{
    AST, Parser, ParserGenerator,
    literal::{BinaryLiteral, BoolLiteral, FloatLiteral, Literal, StringLiteral},
};

#[derive(Debug)]
pub struct Value {
    pub string: Option<StringLiteral>,
    pub float: Option<FloatLiteral>,
    pub binary: Option<BinaryLiteral>,
    pub bool: Option<BoolLiteral>,
    pub any_string: Option<Vec<Literal>>,
}

#[derive(Debug)]
pub struct ValueParser {
    pub string: Option<GeneratorTokenKind>,
    pub float: Option<GeneratorTokenKind>,
    pub binary: Option<GeneratorTokenKind>,
    pub bool: Option<GeneratorTokenKind>,
    pub any_string: Option<Vec<GeneratorTokenKind>>,
}

#[derive(Debug)]
pub struct ValueAST {}

impl<'input> ParserGenerator<'input, ValueAST, ValueParser> for Value {
    fn generate(&self, registry: &mut crate::lexer::TokenizerRegistry) -> ValueParser {
        ValueParser {
            string: self.string.as_ref().map(|string| string.register(registry)),
            float: self.float.as_ref().map(|float| float.register(registry)),
            binary: self.binary.as_ref().map(|binary| binary.register(registry)),
            bool: self.bool.as_ref().map(|bool| bool.register(registry)),
            any_string: self.any_string.as_ref().map(|any_string| {
                any_string
                    .iter()
                    .map(|literal| literal.register(registry))
                    .collect()
            }),
        }
    }
}

impl<'input> Parser<'input, ValueAST> for ValueParser {
    fn parse(&self, lexer: &mut crate::lexer::GeneratorLexer<'input>) -> Option<ValueAST> {
        todo!()
    }
}

impl<'input> AST<'input> for ValueAST {
    fn span() -> std::ops::Range<usize> {
        todo!()
    }

    fn take_value(
        &self,
        validator: &mut tyml_validate::validate::ValueTypeChecker<'_, '_, '_, '_, 'input, 'input>,
        section_name_stack: &mut allocator_api2::vec::Vec<&'input str, &bumpalo::Bump>,
    ) {
        todo!()
    }
}
