use allocator_api2::vec::Vec;
use bumpalo::Bump;

use crate::{
    IncompleteTyml, Tyml,
    ast::{Define, Defines},
    error::{Expected, ParseError, Scope, recover_until},
    lexer::{GetTokenKind, Lexer, TokenKind},
};

pub(crate) fn parse_tyml<'input, 'allocator>(
    source_code: &'input str,
) -> Result<Tyml<'input, 'allocator>, IncompleteTyml<'input, 'allocator>> {
    let holder = IncompleteTyml {
        tyml: Tyml {
            source_code,
            ast: None,
            allocator: Bump::new(),
        },
    };

    Ok(holder.tyml)
}

fn parse_defines<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>>,
    allocator: &'allocator Bump,
) -> Option<&'allocator Defines<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let mut defines = Vec::new_in(allocator);

    lexer.skip_line_feed();

    loop {
        let define = match parse_define(lexer, errors, allocator) {
            Some(define) => define,
            None => {
                let error = recover_until(
                    lexer,
                    &[TokenKind::LineFeed, TokenKind::Comma, TokenKind::BraceRight],
                    Expected::Define,
                    Scope::Defines,
                    allocator,
                );
                errors.push(error);

                if let TokenKind::BraceRight | TokenKind::None = lexer.current().get_kind() {
                    break;
                } else {
                    continue;
                }
            }
        };

        defines.push(define);

        if lexer.current().get_kind() == TokenKind::Comma {
            lexer.next();
        }
        lexer.skip_line_feed();
    }

    Some(allocator.alloc(Defines {
        defines,
        span: anchor.elapsed(lexer),
    }))
}

fn parse_define<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>>,
    allocator: &'allocator Bump,
) -> Option<Define<'input, 'allocator>> {
    None
}
