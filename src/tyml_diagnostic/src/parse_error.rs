use ariadne::Color;
use tyml_parser::error::{ParseError, ParseErrorKind};
use tyml_source::AsUtf8ByteRange;
use tyml_type::types::NamedTypeMap;

use crate::{
    Diagnostic, DiagnosticBuilder, DiagnosticLabel, MessageSection, SourceCodeKind,
    TymlDiagnosticMessage,
};

impl<'input, 'allocator> DiagnosticBuilder for ParseError<'input, 'allocator> {
    fn build(&self, _: &NamedTypeMap) -> Diagnostic {
        match self.kind {
            ParseErrorKind::InvalidDefineElement => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::ParseError,
                    code: 0001,
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
                        span: self.span.as_utf8_byte_range(),
                        color: Color::Cyan,
                        message_override: None,
                    },
                ],
            },
            ParseErrorKind::InvalidDefineSeparator => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::ParseError,
                    code: 0002,
                    arguments: vec![self.error_tokens[0].text.to_string()],
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
                        span: self.span.as_utf8_byte_range(),
                        color: Color::Cyan,
                        message_override: None,
                    },
                ],
            },
            ParseErrorKind::NotFoundElementTypeAndDefaultValue => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::ParseError,
                    code: 0003,
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
                        span: self.span.as_utf8_byte_range(),
                        color: Color::Cyan,
                        message_override: None,
                    },
                ],
            },
            ParseErrorKind::InvalidElementTypeFormat => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::ParseError,
                    code: 0004,
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
                        span: self.span.as_utf8_byte_range(),
                        color: Color::Cyan,
                        message_override: None,
                    },
                ],
            },
            ParseErrorKind::NonClosedBrace => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::ParseError,
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
            ParseErrorKind::UnknownDefaultValueFormat => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::ParseError,
                    code: 0006,
                    arguments: vec![],
                },
                labels: vec![DiagnosticLabel {
                    kind: SourceCodeKind::Tyml,
                    span: self.span.as_utf8_byte_range(),
                    color: Color::Red,
                    message_override: None,
                }],
            },
            ParseErrorKind::NotFoundStructName => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::ParseError,
                    code: 0007,
                    arguments: vec![],
                },
                labels: vec![DiagnosticLabel {
                    kind: SourceCodeKind::Tyml,
                    span: self.span.as_utf8_byte_range(),
                    color: Color::Red,
                    message_override: None,
                }],
            },
            ParseErrorKind::NotFoundStructBlock => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::ParseError,
                    code: 0008,
                    arguments: vec![],
                },
                labels: vec![DiagnosticLabel {
                    kind: SourceCodeKind::Tyml,
                    span: self.span.as_utf8_byte_range(),
                    color: Color::Red,
                    message_override: None,
                }],
            },
            ParseErrorKind::NotFoundEnumName => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::ParseError,
                    code: 0009,
                    arguments: vec![],
                },
                labels: vec![DiagnosticLabel {
                    kind: SourceCodeKind::Tyml,
                    span: self.span.as_utf8_byte_range(),
                    color: Color::Red,
                    message_override: None,
                }],
            },
            ParseErrorKind::NotFoundEnumBlock => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::ParseError,
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
            ParseErrorKind::InvalidEnumElement => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::ParseError,
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
            ParseErrorKind::InvalidEnumElementSeparator => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::ParseError,
                    code: 0012,
                    arguments: vec![self.error_tokens[0].text.to_string()],
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
                        span: self.span.as_utf8_byte_range(),
                        color: Color::Cyan,
                        message_override: None,
                    },
                ],
            },
            ParseErrorKind::InvalidOrTypeFormat => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::ParseError,
                    code: 0013,
                    arguments: vec![],
                },
                labels: vec![DiagnosticLabel {
                    kind: SourceCodeKind::Tyml,
                    span: self.span.as_utf8_byte_range(),
                    color: Color::Red,
                    message_override: None,
                }],
            },
            ParseErrorKind::NotFoundArrayBaseType => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::ParseError,
                    code: 0014,
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
                        span: self.span.as_utf8_byte_range(),
                        color: Color::Cyan,
                        message_override: None,
                    },
                ],
            },
            ParseErrorKind::NonClosedBracket => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::ParseError,
                    code: 0015,
                    arguments: vec![],
                },
                labels: vec![DiagnosticLabel {
                    kind: SourceCodeKind::Tyml,
                    span: self.span.as_utf8_byte_range(),
                    color: Color::Red,
                    message_override: None,
                }],
            },
            ParseErrorKind::InvalidRegexAttributeFormat => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::ParseError,
                    code: 0016,
                    arguments: vec![],
                },
                labels: vec![DiagnosticLabel {
                    kind: SourceCodeKind::Tyml,
                    span: self.span.as_utf8_byte_range(),
                    color: Color::Red,
                    message_override: None,
                }],
            },
            ParseErrorKind::NonFromTo => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::ParseError,
                    code: 0017,
                    arguments: vec![],
                },
                labels: vec![DiagnosticLabel {
                    kind: SourceCodeKind::Tyml,
                    span: self.span.as_utf8_byte_range(),
                    color: Color::Red,
                    message_override: None,
                }],
            },
            ParseErrorKind::InvalidFromToFormat => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::ParseError,
                    code: 0018,
                    arguments: vec![],
                },
                labels: vec![DiagnosticLabel {
                    kind: SourceCodeKind::Tyml,
                    span: self.span.as_utf8_byte_range(),
                    color: Color::Red,
                    message_override: None,
                }],
            },
            ParseErrorKind::NonNumeric => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::ParseError,
                    code: 0019,
                    arguments: vec![],
                },
                labels: vec![DiagnosticLabel {
                    kind: SourceCodeKind::Tyml,
                    span: self.span.as_utf8_byte_range(),
                    color: Color::Red,
                    message_override: None,
                }],
            },
            ParseErrorKind::BiggerFrom => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::ParseError,
                    code: 0020,
                    arguments: vec![],
                },
                labels: vec![DiagnosticLabel {
                    kind: SourceCodeKind::Tyml,
                    span: self.span.as_utf8_byte_range(),
                    color: Color::Red,
                    message_override: None,
                }],
            },
            ParseErrorKind::InvalidAndOrAttributeFormat => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::ParseError,
                    code: 0021,
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
