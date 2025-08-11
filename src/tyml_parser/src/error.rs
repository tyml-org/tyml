use std::ops::Range;

use allocator_api2::vec::Vec;
use bumpalo::Bump;

use crate::lexer::{Lexer, Token, TokenKind};

#[derive(Debug)]
pub struct ParseError<'input, 'allocator> {
    pub kind: ParseErrorKind,
    pub scope: Scope,
    pub expected: Expected,
    pub error_tokens: Vec<Token<'input>, &'allocator Bump>,
    pub span: Range<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Scope {
    Defines,
    ElementDefine,
    StructDefine,
    EnumDefine,
    TypeAttribute,
    Property,
    Interface,
    Function,
    Json,
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
    BraceLeft,
    BraceRight,
    BracketLeft,
    BracketRight,
    StringLiteral,
    FromTo,
    NumericLiteral,
    SmallerNumericLiteral,
    Unnecessary,
    TypeAttribute,
    ParenthesisLeft,
    ParenthesisRight,
    PropertyName,
    Equal,
    FunctionName,
    Return,
    LineFeed,
    InterfaceName,
    Colon,
    ErrorType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseErrorKind {
    InvalidDefineElement,
    InvalidDefineSeparator,
    NotFoundElementTypeAndDefaultValue,
    InvalidElementTypeFormat,
    NonClosedBrace,
    UnknownDefaultValueFormat,
    NotFoundStructName,
    NotFoundStructBlock,
    NotFoundEnumName,
    NotFoundEnumBlock,
    InvalidEnumElement,
    InvalidEnumElementSeparator,
    InvalidOrTypeFormat,
    NotFoundArrayBaseType,
    NonClosedBracket,
    InvalidRegexAttributeFormat,
    NonFromTo,
    InvalidFromToFormat,
    NonNumeric,
    BiggerFrom,
    InvalidAndOrAttributeFormat,
    NonTypeAttribute,
    NonClosedParenthesis,
    InvalidPropertyFormat,
    InvalidInterfaceFormat,
    InvalidFunctionFormat,
    NonLineFeed,
    InvalidJsonArrayFormat,
    InvalidJsonObjectFormat,
    InvalidThrowsFormat,
}

pub(crate) fn recover_until<'input, 'allocator>(
    kind: ParseErrorKind,
    lexer: &mut Lexer<'input>,
    until: &[TokenKind],
    expected: Expected,
    scope: Scope,
    allocator: &'allocator Bump,
) -> ParseError<'input, 'allocator> {
    let anchor = lexer.cast_anchor();
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
        span: anchor.elapsed(lexer),
    }
}
