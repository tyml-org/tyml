use std::{fmt::Debug, ops::Range};

use tyml_parser::ast::Spanned;
use tyml_source::SourceCodeSpan;

#[derive(Debug)]
pub enum TymlValueValidateError {
    NotTreeValue {
        found: SourceCodeSpan,
        path: String,
        tyml_span: Range<usize>,
    },
    NoValueFound {
        required: Spanned<String>,
        required_in: Vec<SourceCodeSpan>,
    },
    DuplicatedValue {
        exists: Vec<SourceCodeSpan>,
        duplicated: SourceCodeSpan,
        path: String,
    },
    UnknownValue {
        values: Vec<SourceCodeSpan>,
        path: String,
    },
    InvalidValue {
        found: Vec<SourceCodeSpan>,
        expected: Spanned<String>,
        path: String,
    },
    NotArrayValue {
        found: Vec<SourceCodeSpan>,
        expected: Spanned<String>,
        path: String,
    },
}
