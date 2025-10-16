use std::{borrow::Cow, ops::Range};

use tyml_parser::ast::Spanned;

use crate::types::Type;

#[derive(Debug)]
pub struct TypeError<'input> {
    pub kind: TypeErrorKind<'input>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum TypeErrorKind<'input> {
    UnknownNamedType {
        name: Spanned<&'input str>,
    },
    IncompatibleValueType {
        value: Spanned<Cow<'input, str>>,
        value_type: Type,
        expected: Spanned<Type>,
    },
    IncompatibleValueForAttribute {
        value: Spanned<Cow<'input, str>>,
        expected: Spanned<Type>,
    },
    IncompatibleAttributeForType {
        ty: &'static str,
    },
    InvalidRegexAttribute,
    IncompatibleJsonValueType {
        json: Range<usize>,
        expected: Spanned<Type>,
    },
    NameAlreadyExists {
        exists: Spanned<Cow<'input, str>>,
    },
    BodyArgumentAlreadyExists {
        exists: Range<usize>,
    },
    ClaimArgumentAlreadyExists {
        exists: Range<usize>,
    },
    ClaimNotFound,
    AuthedNotFound,
}
