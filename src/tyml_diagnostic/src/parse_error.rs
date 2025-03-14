use ariadne::Color;
use tyml_parser::error::{ParseError, ParseErrorKind};
use tyml_type::types::NamedTypeMap;

use crate::{Diagnostic, DiagnosticBuilder, MessageSection, TymlDiagnositcMessage};

impl<'input, 'allocator> DiagnosticBuilder for ParseError<'input, 'allocator> {
    fn build(&self, _: &NamedTypeMap) -> Diagnostic {
        match self.kind {
            ParseErrorKind::InvalidDefineElement => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::ParseError,
                    code: 0001,
                    arguments: vec![],
                },
                labels: vec![
                    (self.span.clone(), Color::Red),
                    (self.span.clone(), Color::Cyan),
                ],
            },
            ParseErrorKind::InvalidDefineSeparator => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::ParseError,
                    code: 0002,
                    arguments: vec![self.error_tokens[0].text.to_string()],
                },
                labels: vec![
                    (self.span.clone(), Color::Red),
                    (self.span.clone(), Color::Cyan),
                ],
            },
            ParseErrorKind::NotFoundElementTypeAndDefaultValue => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::ParseError,
                    code: 0003,
                    arguments: vec![],
                },
                labels: vec![
                    (self.span.clone(), Color::Red),
                    (self.span.clone(), Color::Cyan),
                ],
            },
            ParseErrorKind::InvalidElementTypeFormat => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::ParseError,
                    code: 0004,
                    arguments: vec![],
                },
                labels: vec![
                    (self.span.clone(), Color::Red),
                    (self.span.clone(), Color::Cyan),
                ],
            },
            ParseErrorKind::NonClosedBrace => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::ParseError,
                    code: 0005,
                    arguments: vec![],
                },
                labels: vec![(self.span.clone(), Color::Red)],
            },
            ParseErrorKind::UnknownDefaultValueFormat => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::ParseError,
                    code: 0006,
                    arguments: vec![],
                },
                labels: vec![(self.span.clone(), Color::Red)],
            },
            ParseErrorKind::NotFoundStructName => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::ParseError,
                    code: 0007,
                    arguments: vec![],
                },
                labels: vec![(self.span.clone(), Color::Red)],
            },
            ParseErrorKind::NotFoundStructBlock => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::ParseError,
                    code: 0008,
                    arguments: vec![],
                },
                labels: vec![(self.span.clone(), Color::Red)],
            },
            ParseErrorKind::NotFoundEnumName => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::ParseError,
                    code: 0009,
                    arguments: vec![],
                },
                labels: vec![(self.span.clone(), Color::Red)],
            },
            ParseErrorKind::NotFoundEnumBlock => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::ParseError,
                    code: 0010,
                    arguments: vec![],
                },
                labels: vec![(self.span.clone(), Color::Red)],
            },
            ParseErrorKind::InvalidEnumElement => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::ParseError,
                    code: 0011,
                    arguments: vec![],
                },
                labels: vec![(self.span.clone(), Color::Red)],
            },
            ParseErrorKind::InvalidEnumElementSeparator => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::ParseError,
                    code: 0012,
                    arguments: vec![self.error_tokens[0].text.to_string()],
                },
                labels: vec![
                    (self.span.clone(), Color::Red),
                    (self.span.clone(), Color::Cyan),
                ],
            },
            ParseErrorKind::InvalidOrTypeFormat => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::ParseError,
                    code: 0013,
                    arguments: vec![],
                },
                labels: vec![(self.span.clone(), Color::Red)],
            },
            ParseErrorKind::NotFoundArrayBaseType => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::ParseError,
                    code: 0014,
                    arguments: vec![],
                },
                labels: vec![
                    (self.span.clone(), Color::Red),
                    (self.span.clone(), Color::Cyan),
                ],
            },
            ParseErrorKind::NonClosedBracket => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::ParseError,
                    code: 0015,
                    arguments: vec![],
                },
                labels: vec![(self.span.clone(), Color::Red)],
            },
        }
    }
}
