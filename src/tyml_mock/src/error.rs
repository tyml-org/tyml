use std::ops::Range;

use ariadne::Color;
use tyml::{
    tyml_diagnostic::{
        Diagnostic, DiagnosticBuilder, DiagnosticLabel, MessageSection, SourceCodeKind, TymlDiagnosticMessage
    }, tyml_source::AsUtf8ByteRange, tyml_type::{resolver::camel_to_snake, types::InterfaceInfo}, Tyml
};

use crate::server::ServerSourceLocation;

pub enum TymlMockError {
    NoReturnDefaultValue { span: Range<usize> },
    NoArgumentDefaultValue { span: Range<usize> },
}

pub fn collect_serve_error(
    tyml: &Tyml,
    location: ServerSourceLocation,
    errors: &mut Vec<TymlMockError>,
) {
    match location {
        ServerSourceLocation::AllInterfaces => {
            for interface in tyml.interfaces().iter() {
                collect_interface_serve_error(interface, errors);
            }
        }
        ServerSourceLocation::Interface(interface_name) => {
            let interface_name = camel_to_snake(interface_name.as_str());

            let Some(interface) = tyml
                .interfaces()
                .iter()
                .find(|interface| interface.name.value.as_str() == interface_name.as_str())
            else {
                return;
            };

            collect_interface_serve_error(interface, errors);
        }
    }
}

fn collect_interface_serve_error(interface: &InterfaceInfo, errors: &mut Vec<TymlMockError>) {
    for function in interface.functions.iter() {
        if function
            .return_info
            .as_ref()
            .map(|info| info.default_value)
            .flatten()
            .is_none()
        {
            let error = TymlMockError::NoReturnDefaultValue {
                span: function.name.span(),
            };
            errors.push(error);
        }
    }
}

pub fn collect_send_error(
    tyml: &Tyml,
    interface_name: &str,
    function_name: &str,
    errors: &mut Vec<TymlMockError>,
) {
    let interface_name = camel_to_snake(interface_name);
    let function_name = camel_to_snake(function_name);

    let Some(interface) = tyml
        .interfaces()
        .iter()
        .find(|interface| interface.name.value.as_str() == interface_name.as_str())
    else {
        return;
    };

    let Some(function) = interface
        .functions
        .iter()
        .find(|function| function.name.value.as_str() == function_name.as_str())
    else {
        return;
    };

    for argument in function.arguments.iter() {
        if argument.default_value.is_none() {
            let error = TymlMockError::NoArgumentDefaultValue {
                span: argument.name.span(),
            };
            errors.push(error);
        }
    }
}

impl DiagnosticBuilder for TymlMockError {
    fn build(&self, _: &tyml::tyml_type::types::NamedTypeMap) -> tyml::tyml_diagnostic::Diagnostic {
        match self {
            TymlMockError::NoReturnDefaultValue { span } => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::MockError,
                    code: 0001,
                    arguments: vec![],
                },
                labels: vec![DiagnosticLabel {
                    kind: SourceCodeKind::Tyml,
                    span: span.as_utf8_byte_range(),
                    color: Color::Red,
                    message_override: None,
                }],
            },
            TymlMockError::NoArgumentDefaultValue { span } => Diagnostic {
                message: TymlDiagnosticMessage {
                    section: MessageSection::MockError,
                    code: 0002,
                    arguments: vec![],
                },
                labels: vec![DiagnosticLabel {
                    kind: SourceCodeKind::Tyml,
                    span: span.as_utf8_byte_range(),
                    color: Color::Red,
                    message_override: None,
                }],
            },
        }
    }
}
