use ariadne::Color;
use tyml_type::{
    error::{TypeError, TypeErrorKind},
    types::{NamedTypeMap, ToTypeName},
};

use crate::{Diagnostic, DiagnosticBuilder, MessageSection, TymlDiagnositcMessage};

impl<'input, 'ty> DiagnosticBuilder for TypeError<'input, 'ty> {
    fn build(&self, named_type_map: &NamedTypeMap) -> crate::Diagnostic {
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
                    arguments: vec![
                        value_type.to_type_name(named_type_map),
                        expected.value.to_type_name(named_type_map),
                    ],
                },
                labels: vec![
                    (value.span.clone(), Color::Red),
                    (expected.span.clone(), Color::Yellow),
                ],
            },
            TypeErrorKind::IncompatibleValueForAttribute { value, expected } => Diagnostic {
                message: TymlDiagnositcMessage {
                    section: MessageSection::TypeError,
                    code: 0003,
                    arguments: vec![expected.value.to_type_name(named_type_map)],
                },
                labels: vec![
                    (value.span.clone(), Color::Red),
                    (expected.span.clone(), Color::Yellow),
                ],
            },
        }
    }
}
