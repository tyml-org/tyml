use std::ops::Range;

use tyml_parser::ast::Spanned;

use crate::types::Type;

#[derive(Debug)]
pub struct TypeError<'input, 'ty> {
    pub kind: TypeErrorKind<'input, 'ty>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum TypeErrorKind<'input, 'ty> {
    UnknownNamedType {
        name: Spanned<&'input str>,
    },
    IncompatibleValueType {
        value: Spanned<&'input str>,
        value_type: Type<'ty>,
        expected: Type<'ty>,
    },
    IncompatibleValue {
        value: Spanned<&'input str>,
        expected: Type<'ty>,
    },
}
