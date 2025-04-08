use std::{fmt::Debug, ops::Range};

use tyml_parser::ast::Spanned;

#[derive(Debug)]
pub enum TymlValueValidateError<Span: Debug> {
    NotTreeValue {
        found: Span,
        path: String,
        tyml_span: Range<usize>,
    },
    NoValueFound {
        required: Spanned<String>,
        required_in: Vec<Span>,
    },
    DuplicatedValue {
        exists: Vec<Span>,
        duplicated: Span,
        path: String,
    },
    UnknownValue {
        values: Vec<Span>,
        path: String,
    },
    InvalidValue {
        found: Vec<Span>,
        expected: Spanned<String>,
        path: String,
    },
    NotArrayValue {
        found: Vec<Span>,
        expected: Spanned<String>,
        path: String,
    },
}
