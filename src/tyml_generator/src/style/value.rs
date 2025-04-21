use allocator_api2::vec::Vec;

use crate::lexer::{GeneratorTokenKind, SpannedText};

use super::{
    AST, Parser, ParserGenerator, ParserPart,
    error::GeneratedParseError,
    literal::{BinaryLiteral, BoolLiteral, FloatLiteral, Literal, Register, StringLiteral},
};

#[derive(Debug, Default)]
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
    pub any_string: Option<GeneratorTokenKind>,
}

#[derive(Debug)]
pub struct ValueAST<'input> {
    pub value: SpannedText<'input>,
    pub kind: ValueASTKind,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ValueASTKind {
    String,
    Float,
    Binary,
    Bool,
    AnyString,
}

impl<'input> ParserGenerator<'input, ValueAST<'input>, ValueParser> for Value {
    fn generate(&self, registry: &mut crate::lexer::TokenizerRegistry) -> ValueParser {
        ValueParser {
            string: self.string.as_ref().map(|string| string.register(registry)),
            float: self.float.as_ref().map(|float| float.register(registry)),
            binary: self.binary.as_ref().map(|binary| binary.register(registry)),
            bool: self.bool.as_ref().map(|bool| bool.register(registry)),
            any_string: self
                .any_string
                .as_ref()
                .map(|any_string| any_string.iter().register(registry)),
        }
    }
}

impl<'input> Parser<'input, ValueAST<'input>> for ValueParser {
    fn parse(
        &self,
        lexer: &mut crate::lexer::GeneratorLexer<'input, '_>,
        _: &mut Vec<GeneratedParseError>,
    ) -> Option<ValueAST<'input>> {
        if let Some(string) = self.string {
            if lexer.current_contains(string) {
                return Some(ValueAST {
                    value: lexer.next().unwrap().into_spanned(),
                    kind: ValueASTKind::String,
                });
            }
        }

        if let Some(float) = self.float {
            if lexer.current_contains(float) {
                return Some(ValueAST {
                    value: lexer.next().unwrap().into_spanned(),
                    kind: ValueASTKind::Float,
                });
            }
        }

        if let Some(binary) = self.binary {
            if lexer.current_contains(binary) {
                return Some(ValueAST {
                    value: lexer.next().unwrap().into_spanned(),
                    kind: ValueASTKind::Binary,
                });
            }
        }

        if let Some(bool) = self.bool {
            if lexer.current_contains(bool) {
                return Some(ValueAST {
                    value: lexer.next().unwrap().into_spanned(),
                    kind: ValueASTKind::Bool,
                });
            }
        }

        if let Some(any_string) = self.any_string {
            if lexer.current_contains(any_string) {
                return Some(ValueAST {
                    value: lexer.next().unwrap().into_spanned(),
                    kind: ValueASTKind::AnyString,
                });
            }
        }

        None
    }

    fn first_token_kind(&self) -> GeneratorTokenKind {
        self.string
            .or(self.float)
            .or(self.binary)
            .or(self.binary)
            .or(self.bool)
            .or(self.any_string)
            .unwrap()
    }
}

impl ParserPart for ValueParser {
    fn expected_message_key(&self) -> std::borrow::Cow<'static, str> {
        "expected.message.value".into()
    }

    fn expected_format_key(&self) -> Option<std::borrow::Cow<'static, str>> {
        None
    }
}

impl<'input> AST<'input> for ValueAST<'input> {
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
