use ariadne::Color;
use tyml_type::{
    error::{TypeError, TypeErrorKind},
    types::{NamedTypeMap, ToTypeName},
};

use crate::{
    AsUtf8ByteRange, Diagnostic, DiagnosticBuilder, MessageSection, TymlDiagnositcMessage,
};

impl<'input, 'ty> DiagnosticBuilder for TypeError<'input, 'ty> {
    fn build(&self, named_type_map: &NamedTypeMap) -> crate::Diagnostic {
        match &self.kind {
            TypeErrorKind::UnknownNamedType { name } => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::TypeError,
                    code: 0001,
                    arguments: vec![name.value.to_string()],
                },
                labels: vec![(name.span.as_utf8_byte_range(), Color::Red)],
            },
            TypeErrorKind::IncompatibleValueType {
                value,
                value_type,
                expected,
            } => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::TypeError,
                    code: 0002,
                    arguments: vec![
                        value_type.to_type_name(named_type_map),
                        expected.value.to_type_name(named_type_map),
                    ],
                },
                labels: vec![
                    (value.span.as_utf8_byte_range(), Color::Red),
                    (expected.span.as_utf8_byte_range(), Color::Yellow),
                ],
            },
            TypeErrorKind::IncompatibleValueForAttribute { value, expected } => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::TypeError,
                    code: 0003,
                    arguments: vec![expected.value.to_type_name(named_type_map)],
                },
                labels: vec![
                    (value.span.as_utf8_byte_range(), Color::Red),
                    (expected.span.as_utf8_byte_range(), Color::Yellow),
                ],
            },
        }
    }
}
