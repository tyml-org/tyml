use allocator_api2::vec::Vec;
use bumpalo::Bump;

use crate::lexer::{Lexer, Token, TokenKind};

#[derive(Debug)]
pub struct ParseError<'input, 'allocator> {
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
    StructElementBlock,
    EnumName,
    EnumElementBlock,
    EnumElement,
    EnumElementSeparator,
    BraceRight,
}

pub(crate) fn recover_until<'input, 'allocator>(
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
        scope,
        expected,
        error_tokens,
    }
}
