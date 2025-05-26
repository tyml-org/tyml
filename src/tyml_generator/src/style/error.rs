use std::{borrow::Cow, ops::Range};

use crate::lexer::{GeneratorLexer, GeneratorTokenKind};

use super::ParserPart;

#[derive(Debug)]
pub struct GeneratedParseError {
    pub span: Range<usize>,
    pub parse_error_code: usize,
    pub expected_format: Option<Cow<'static, str>>,
}

pub(crate) fn recover_until_or_lf<'input, P: ParserPart>(
    lexer: &mut GeneratorLexer,
    until: impl Iterator<Item = GeneratorTokenKind>,
    parser: &P,
) -> GeneratedParseError {
    let anchor = lexer.cast_anchor();

    let until = until.collect::<Vec<_>>();

    loop {
        if until.iter().any(|until| lexer.current_contains(*until)) {
            break;
        }

        if lexer.is_reached_eof() {
            break;
        };

        if lexer.is_current_lf() {
            break;
        }

        lexer.next();
    }

    GeneratedParseError {
        span: anchor.elapsed(lexer),
        parse_error_code: parser.parse_error_code(),
        expected_format: parser.expected_format(),
    }
}
