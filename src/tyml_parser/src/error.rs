use allocator_api2::vec::Vec;
use bumpalo::Bump;

use crate::lexer::{Lexer, Token, TokenKind};

#[derive(Debug)]
pub struct ParseError<'input, 'allocator> {
    pub kind: ParseErrorKind,
    pub scope: Scope,
    pub expected: Expected,
    pub error_tokens: Vec<Token<'input>, &'allocator Bump>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Scope {
    Defines,
    ElementDefine,
    StructDefine,
    EnumDefine,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Expected {
    Define,
    DefineSeparator,
    NodeLiteral,
    TypeOrValue,
    Type,
    Value,
    StructName,
    StructElementBlockOrTypeName,
    EnumName,
    EnumElementBlock,
    EnumElement,
    EnumElementSeparator,
    BraceRight,
    BracketRight,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseErrorKind {
    InvalidDefineElement,
    InvalidDefineSeparator,
    NotFoundNodeLiteralAfterPeriod,
    NotFoundElementTypeAndDefaultValue,
    InvalidElementTypeFormat,
    NonClosedBrace,
    UnknownDefaultValueFormat,
    NotFoundStructName,
    NotFoundStructBlock,
    NotFoundEnumName,
    NotFoundEnumBlock,
    NotFoundEnumElement,
    InvalidEnumElementSeparator,
    InvalidOrTypeFormat,
    NotFoundArrayBaseType,
    NonClosedBracket,
}

pub(crate) fn recover_until<'input, 'allocator>(
    kind: ParseErrorKind,
    lexer: &mut Lexer<'input>,
    until: &[TokenKind],
    expected: Expected,
    scope: Scope,
    allocator: &'allocator Bump,
) -> ParseError<'input, 'allocator> {
    let mut error_tokens = Vec::new_in(allocator);

    loop {
        let token = match lexer.current() {
            Some(token) => token,
            None => break,
        };

        if until.contains(&token.kind) {
            break;
        }

        error_tokens.push(token);

        lexer.next();
    }

    ParseError {
        kind,
        scope,
        expected,
        error_tokens,
    }
}
