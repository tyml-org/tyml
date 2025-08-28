use ariadne::Color;
use tyml_source::AsUtf8ByteRange;
use tyml_type::{
    error::{TypeError, TypeErrorKind},
    types::{NamedTypeMap, ToTypeName},
};

use crate::{
    Diagnostic, DiagnosticBuilder, DiagnosticLabel, MessageSection, SourceCodeKind,
    TymlDiagnosticMessage,
};

impl<'input, 'ty> DiagnosticBuilder for TypeError<'input, 'ty> {
    fn build(&self, named_type_map: &NamedTypeMap) -> crate::Diagnostic {
        match &self.kind {
            TypeErrorKind::UnknownNamedType { name } => Diagnostic {
                message: TymlDiagnosticMessage {
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
                message: TymlDiagnosticMessage {
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
                message: TymlDiagnosticMessage {
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
            TypeErrorKind::IncompatibleAttributeForType { ty } => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::TypeError,
                    code: 0004,
                    arguments: vec![ty.to_string()],
                },
                labels: vec![DiagnosticLabel {
                    kind: SourceCodeKind::Tyml,
                    span: self.span.as_utf8_byte_range(),
                    color: Color::Red,
                    message_override: None,
                }],
            },
            TypeErrorKind::InvalidRegexAttribute => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::TypeError,
                    code: 0005,
                    arguments: vec![],
                },
                labels: vec![DiagnosticLabel {
                    kind: SourceCodeKind::Tyml,
                    span: self.span.as_utf8_byte_range(),
                    color: Color::Red,
                    message_override: None,
                }],
            },
            TypeErrorKind::IncompatibleJsonValueType { json, expected } => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::TypeError,
                    code: 0006,
                    arguments: vec![expected.value.to_type_name(named_type_map)],
                },
                labels: vec![
                    DiagnosticLabel {
                        kind: SourceCodeKind::Tyml,
                        span: json.as_utf8_byte_range(),
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
            TypeErrorKind::NameAlreadyExists { exists } => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::TypeError,
                    code: 0007,
                    arguments: vec![exists.value.to_string()],
                },
                labels: vec![
                    DiagnosticLabel {
                        kind: SourceCodeKind::Tyml,
                        span: self.span.as_utf8_byte_range(),
                        color: Color::Red,
                        message_override: None,
                    },
                    DiagnosticLabel {
                        kind: SourceCodeKind::Tyml,
                        span: exists.span.as_utf8_byte_range(),
                        color: Color::Yellow,
                        message_override: None,
                    },
                ],
            },
            TypeErrorKind::BodyArgumentAlreadyExists { exists } => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::TypeError,
                    code: 0008,
                    arguments: vec![],
                },
                labels: vec![
                    DiagnosticLabel {
                        kind: SourceCodeKind::Tyml,
                        span: self.span.as_utf8_byte_range(),
                        color: Color::Red,
                        message_override: None,
                    },
                    DiagnosticLabel {
                        kind: SourceCodeKind::Tyml,
                        span: exists.as_utf8_byte_range(),
                        color: Color::Yellow,
                        message_override: None,
                    },
                ],
            },
            TypeErrorKind::ClaimArgumentAlreadyExists { exists } => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::TypeError,
                    code: 0009,
                    arguments: vec![],
                },
                labels: vec![
                    DiagnosticLabel {
                        kind: SourceCodeKind::Tyml,
                        span: self.span.as_utf8_byte_range(),
                        color: Color::Red,
                        message_override: None,
                    },
                    DiagnosticLabel {
                        kind: SourceCodeKind::Tyml,
                        span: exists.as_utf8_byte_range(),
                        color: Color::Yellow,
                        message_override: None,
                    },
                ],
            },
            TypeErrorKind::ClaimNotFound => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::TypeError,
                    code: 0010,
                    arguments: vec![],
                },
                labels: vec![DiagnosticLabel {
                    kind: SourceCodeKind::Tyml,
                    span: self.span.as_utf8_byte_range(),
                    color: Color::Red,
                    message_override: None,
                }],
            },
            TypeErrorKind::AuthedNotFound => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::TypeError,
                    code: 0011,
                    arguments: vec![],
                },
                labels: vec![DiagnosticLabel {
                    kind: SourceCodeKind::Tyml,
                    span: self.span.as_utf8_byte_range(),
                    color: Color::Red,
                    message_override: None,
                }],
            },
        }
    }
}
