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
        expected: Spanned<Type<'ty>>,
    },
    IncompatibleValueForAttribute {
        value: Spanned<&'input str>,
        expected: Spanned<Type<'ty>>,
    },
    IncompatibleAttributeForType {
        ty: &'static str,
    },
    InvalidRegexAttribute,
}
