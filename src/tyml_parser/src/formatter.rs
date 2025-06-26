use tyml_formatter::FormatterToken;

use crate::{ast::Defines, lexer::Lexer};

pub fn collect_format_token<'input>(ast: &Defines<'input, '_>) -> Vec<FormatterToken<'input>> {
    let mut tokens = Vec::new();

    tokens
}

