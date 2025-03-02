use ariadne::Color;
use tyml_parser::error::{ParseError, ParseErrorKind};

use crate::{Diagnostic, DiagnosticBuilder, MessageSection, TymlDiagnositcMessage};

impl<'input, 'allocator> DiagnosticBuilder for ParseError<'input, 'allocator> {
    fn build(&self) -> Diagnostic {
        match self.kind {
            ParseErrorKind::InvalidDefineElement => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::ParseError,
                    code: 0001,
                    arguments: vec![],
                },
                labels: vec![(self.span.clone(), Color::Red)],
            },
            ParseErrorKind::InvalidDefineSeparator => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::ParseError,
                    code: 0002,
                    arguments: vec![],
                },
                labels: vec![(self.span.clone(), Color::Red)],
            },
            ParseErrorKind::NotFoundElementTypeAndDefaultValue => todo!(),
            ParseErrorKind::InvalidElementTypeFormat => todo!(),
            ParseErrorKind::NonClosedBrace => todo!(),
            ParseErrorKind::UnknownDefaultValueFormat => todo!(),
            ParseErrorKind::NotFoundStructName => todo!(),
            ParseErrorKind::NotFoundStructBlock => todo!(),
            ParseErrorKind::NotFoundEnumName => todo!(),
            ParseErrorKind::NotFoundEnumBlock => todo!(),
            ParseErrorKind::NotFoundEnumElement => todo!(),
            ParseErrorKind::InvalidEnumElementSeparator => todo!(),
            ParseErrorKind::InvalidOrTypeFormat => todo!(),
            ParseErrorKind::NotFoundArrayBaseType => todo!(),
            ParseErrorKind::NonClosedBracket => todo!(),
        }
    }
}
