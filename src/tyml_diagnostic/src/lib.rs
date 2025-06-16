use std::ops::Deref;

use ariadne::{Color, Label, Report, ReportKind, sources};
use message::{get_text, get_text_optional, replace_message};
use tyml_source::{SourceCode, SourceCodeSpan, ToUnicodeCharacterRange};
use tyml_type::types::NamedTypeMap;

pub mod generated_parse_error;
pub mod message;
pub mod parse_error;
pub mod type_error;
pub mod validate_error;

pub struct TymlDiagnosticMessage {
    pub section: MessageSection,
    pub code: usize,
    pub arguments: Vec<String>,
}

impl TymlDiagnosticMessage {
    pub fn section_name(&self, lang: &str, colored: bool) -> String {
        replace_message(
            get_text(format!("{}.name", self.section.section()).as_str(), lang),
            &vec![format!("{:>04}", self.code)],
            colored,
        )
    }

    pub fn message(&self, lang: &str, colored: bool) -> String {
        replace_message(
            get_text(
                format!("{}.{:>04}.message", &self.section.section(), self.code).as_str(),
                lang,
            ),
            &self.arguments,
            colored,
        )
    }

    pub fn label(&self, label_index: usize, lang: &str, colored: bool) -> Option<String> {
        get_text_optional(
            format!(
                "{}.{:>04}.label_{}",
                &self.section.section(),
                self.code,
                label_index
            )
            .as_str(),
            lang,
        )
        .map(|text| replace_message(text, &self.arguments, colored))
    }

    pub fn note(&self, lang: &str, colored: bool) -> Option<String> {
        get_text_optional(
            format!("{}.{:>04}.note", &self.section.section(), self.code).as_str(),
            lang,
        )
        .map(|text| replace_message(text, &self.arguments, colored))
    }

    pub fn help(&self, lang: &str, colored: bool) -> Option<String> {
        get_text_optional(
            format!("{}.{:>04}.help", &self.section.section(), self.code).as_str(),
            lang,
        )
        .map(|text| replace_message(text, &self.arguments, colored))
    }
}

pub enum MessageSection {
    ParseError,
    TypeError,
    ValidateError,
    MLParseError,
}

impl MessageSection {
    const fn section(&self) -> &'static str {
        match self {
            MessageSection::ParseError => "parse_error",
            MessageSection::TypeError => "type_error",
            MessageSection::ValidateError => "validate_error",
            MessageSection::MLParseError => "ml_parse_error",
        }
    }
}

pub trait DiagnosticBuilder {
    fn build(&self, named_type_map: &NamedTypeMap) -> Diagnostic;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticLabel {
    pub kind: SourceCodeKind,
    pub span: SourceCodeSpan,
    pub color: Color,
    pub message_override: Option<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SourceCodeKind {
    Tyml,
    ValidateTraget,
}

pub struct Diagnostic {
    pub message: TymlDiagnosticMessage,
    pub labels: Vec<DiagnosticLabel>,
}

impl Diagnostic {
    pub fn print(&self, lang: &str, tyml_source: &SourceCode, validate_target_source: &SourceCode) {
        let section_name = self.message.section_name(lang, true);

        let get_label_span = |label: &DiagnosticLabel| match label.kind {
            SourceCodeKind::Tyml => (
                tyml_source.name.deref().clone(),
                label.span.to_unicode_character_range(&tyml_source.code),
            ),
            SourceCodeKind::ValidateTraget => (
                validate_target_source.name.deref().clone(),
                label
                    .span
                    .to_unicode_character_range(&validate_target_source.code),
            ),
        };

        let mut builder = Report::build(
            ReportKind::Custom(section_name.as_str(), Color::White),
            self.labels
                .get(0)
                .map(get_label_span)
                .unwrap_or((tyml_source.name.deref().clone(), 0..0)),
        );

        builder.set_message(self.message.message(lang, true));

        for (default_label_index, label) in self.labels.iter().enumerate() {
            let label_message = self
                .message
                .label(
                    label.message_override.unwrap_or(default_label_index),
                    lang,
                    true,
                )
                .unwrap();

            builder.add_label(
                Label::new(get_label_span(label))
                    .with_message(label_message)
                    .with_color(label.color),
            );
        }

        if let Some(note) = self.message.note(lang, true) {
            builder.set_note(note);
        }

        if let Some(help) = self.message.help(lang, true) {
            builder.set_help(help);
        }

        builder
            .finish()
            .eprint(sources([
                (tyml_source.name.deref().clone(), tyml_source.code.as_str()),
                (
                    validate_target_source.name.deref().clone(),
                    validate_target_source.code.as_str(),
                ),
            ]))
            .unwrap();
    }
}
