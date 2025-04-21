use std::{borrow::Cow, ops::Range};

use crate::lexer::{GeneratorLexer, GeneratorTokenKind};

use super::ParserPart;

#[derive(Debug)]
pub struct GeneratedParseError {
    pub span: Range<usize>,
    pub expected_message_key: Cow<'static, str>,
    pub expected_format: Option<Cow<'static, str>>,
}

pub(crate) fn recover_until_or_lf<'input, P: ParserPart>(
    lexer: &mut GeneratorLexer,
    until: &[GeneratorTokenKind],
    parser: &P,
) -> GeneratedParseError {
    let anchor = lexer.cast_anchor();

    loop {
        if until.iter().any(|until| lexer.current_contains(*until)) {
            break;
        }

        if lexer.current().is_none() {
            break;
        };

        if lexer.is_current_lf() {
            break;
        }

        lexer.next();
    }

    GeneratedParseError {
        span: anchor.elapsed(lexer),
        expected_message_key: parser.expected_message_key(),
        expected_format: parser.expected_format_key(),
    }
}
