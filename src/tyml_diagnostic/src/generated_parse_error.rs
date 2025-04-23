use ariadne::Color;
use tyml_generator::style::error::GeneratedParseError;
use tyml_source::AsUtf8ByteRange;

use crate::{
    Diagnostic, DiagnosticBuilder, DiagnosticLabel, MessageSection, SourceCodeKind,
    TymlDiagnositcMessage,
};

impl DiagnosticBuilder for GeneratedParseError {
    fn build(&self, _: &tyml_type::types::NamedTypeMap) -> crate::Diagnostic {
        Diagnostic {
            message: TymlDiagnositcMessage {
                section: MessageSection::MLParseError,
                code: self.parse_error_code,
                arguments: vec![
                    self.expected_format
                        .as_ref()
                        .map(|format| format.to_string())
                        .unwrap_or_default(),
                ],
            },
            labels: vec![
                DiagnosticLabel {
                    kind: SourceCodeKind::ValidateTraget,
                    span: self.span.as_utf8_byte_range(),
                    color: Color::Red,
                    message_override: None,
                },
                DiagnosticLabel {
                    kind: SourceCodeKind::ValidateTraget,
                    span: self.span.as_utf8_byte_range(),
                    color: Color::Cyan,
                    message_override: None,
                },
            ],
        }
    }
}
