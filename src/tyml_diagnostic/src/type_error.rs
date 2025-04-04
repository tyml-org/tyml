use ariadne::Color;
use tyml_type::{
    error::{TypeError, TypeErrorKind},
    types::{NamedTypeMap, ToTypeName},
};

use crate::{
    AsUtf8ByteRange, Diagnostic, DiagnosticBuilder, DiagnosticLabel, MessageSection,
    SourceCodeKind, TymlDiagnositcMessage,
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
                labels: vec![DiagnosticLabel {
                    kind: SourceCodeKind::Tyml,
                    span: name.span.as_utf8_byte_range(),
                    color: Color::Red,
                    message_override: None,
                }],
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
                    DiagnosticLabel {
                        kind: SourceCodeKind::Tyml,
                        span: value.span.as_utf8_byte_range(),
                        color: Color::Red,
                        message_override: None,
                    },
                    DiagnosticLabel {
                        kind: SourceCodeKind::Tyml,
                        span: expected.span.as_utf8_byte_range(),
                        color: Color::Yellow,
                        message_override: None,
                    },
                ],
            },
            TypeErrorKind::IncompatibleValueForAttribute { value, expected } => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::TypeError,
                    code: 0003,
                    arguments: vec![expected.value.to_type_name(named_type_map)],
                },
                labels: vec![
                    DiagnosticLabel {
                        kind: SourceCodeKind::Tyml,
                        span: value.span.as_utf8_byte_range(),
                        color: Color::Red,
                        message_override: None,
                    },
                    DiagnosticLabel {
                        kind: SourceCodeKind::Tyml,
                        span: expected.span.as_utf8_byte_range(),
                        color: Color::Yellow,
                        message_override: None,
                    },
                ],
            },
        }
    }
}
