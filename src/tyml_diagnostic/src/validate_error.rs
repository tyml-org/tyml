use std::iter::once;

use ariadne::Color;
use tyml_validate::error::TymlValueValidateError;

use crate::{
    AsUtf8ByteRange, Diagnostic, DiagnosticBuilder, DiagnosticLabel, DiagnosticSpan,
    MessageSection, SourceCodeKind, TymlDiagnositcMessage,
};

impl DiagnosticBuilder for TymlValueValidateError<DiagnosticSpan> {
    fn build(&self, _: &tyml_type::types::NamedTypeMap) -> crate::Diagnostic {
        match self {
            TymlValueValidateError::NotTreeValue {
                found,
                path,
                tyml_span,
            } => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::ValidateError,
                    code: 0001,
                    arguments: vec![path.clone()],
                },
                labels: vec![
                    DiagnosticLabel {
                        kind: SourceCodeKind::ValidateTraget,
                        span: found.clone(),
                        color: Color::Red,
                        message_override: None,
                    },
                    DiagnosticLabel {
                        kind: SourceCodeKind::Tyml,
                        span: tyml_span.as_utf8_byte_range(),
                        color: Color::Yellow,
                        message_override: None,
                    },
                ],
            },
            TymlValueValidateError::NoValueFound {
                required,
                required_in,
            } => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::ValidateError,
                    code: 0002,
                    arguments: vec![required.value.clone()],
                },
                labels: required_in
                    .iter()
                    .map(|required_in| DiagnosticLabel {
                        kind: SourceCodeKind::ValidateTraget,
                        span: required_in.clone(),
                        color: Color::Red,
                        message_override: Some(0),
                    })
                    .chain(once(DiagnosticLabel {
                        kind: SourceCodeKind::Tyml,
                        span: required.span.as_utf8_byte_range(),
                        color: Color::Yellow,
                        message_override: Some(1),
                    }))
                    .collect(),
            },
            TymlValueValidateError::DuplicatedValue {
                exists,
                duplicated,
                path,
            } => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::ValidateError,
                    code: 0003,
                    arguments: vec![path.clone()],
                },
                labels: once(DiagnosticLabel {
                    kind: SourceCodeKind::ValidateTraget,
                    span: duplicated.clone(),
                    color: Color::Red,
                    message_override: Some(0),
                })
                .chain(exists.iter().map(|span| DiagnosticLabel {
                    kind: SourceCodeKind::ValidateTraget,
                    span: span.clone(),
                    color: Color::Yellow,
                    message_override: Some(1),
                }))
                .collect(),
            },
            TymlValueValidateError::UnknownValue { values, path } => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::ValidateError,
                    code: 0004,
                    arguments: vec![path.clone()],
                },
                labels: values
                    .iter()
                    .map(|span| DiagnosticLabel {
                        kind: SourceCodeKind::ValidateTraget,
                        span: span.clone(),
                        color: Color::Red,
                        message_override: Some(0),
                    })
                    .collect(),
            },
            TymlValueValidateError::InvalidValue { found, expected } => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::ValidateError,
                    code: 0005,
                    arguments: vec![expected.value.clone()],
                },
                labels: found
                    .iter()
                    .map(|span| DiagnosticLabel {
                        kind: SourceCodeKind::ValidateTraget,
                        span: span.clone(),
                        color: Color::Red,
                        message_override: Some(0),
                    })
                    .chain(once(DiagnosticLabel {
                        kind: SourceCodeKind::Tyml,
                        span: expected.span.as_utf8_byte_range(),
                        color: Color::Yellow,
                        message_override: Some(1),
                    }))
                    .collect(),
            },
            TymlValueValidateError::NotArrayValue { found, expected } => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::ValidateError,
                    code: 0006,
                    arguments: vec![expected.value.clone()],
                },
                labels: found
                    .iter()
                    .map(|span| DiagnosticLabel {
                        kind: SourceCodeKind::ValidateTraget,
                        span: span.clone(),
                        color: Color::Red,
                        message_override: Some(0),
                    })
                    .chain(once(DiagnosticLabel {
                        kind: SourceCodeKind::Tyml,
                        span: expected.span.as_utf8_byte_range(),
                        color: Color::Yellow,
                        message_override: Some(1),
                    }))
                    .collect(),
            },
        }
    }
}
