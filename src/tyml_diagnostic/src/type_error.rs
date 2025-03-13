use ariadne::Color;
use tyml_type::error::{TypeError, TypeErrorKind};

use crate::{Diagnostic, DiagnosticBuilder, MessageSection, TymlDiagnositcMessage};

impl<'input, 'ty> DiagnosticBuilder for TypeError<'input, 'ty> {
    fn build(&self) -> crate::Diagnostic {
        match &self.kind {
            TypeErrorKind::UnknownNamedType { name } => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::TypeError,
                    code: 0001,
                    arguments: vec![name.value.to_string()],
                },
                labels: vec![(name.span.clone(), Color::Red)],
            },
            TypeErrorKind::IncompatibleValueType {
                value,
                value_type,
                expected,
            } => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::TypeError,
                    code: 0002,
                    arguments: vec![],
                },
                labels: vec![],
            },
            TypeErrorKind::IncompatibleValue { value, expected } => todo!(),
        }
    }
}
