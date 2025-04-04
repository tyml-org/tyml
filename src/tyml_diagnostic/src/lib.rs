use std::ops::Range;

use ariadne::{Color, Label, Report, ReportKind, Source};
use extension_fn::extension_fn;
use message::{get_text, get_text_optional, replace_message};
use tyml_type::types::NamedTypeMap;

mod message;
pub mod parse_error;
pub mod type_error;
pub mod validate_error;

pub struct TymlDiagnositcMessage {
    pub(crate) section: MessageSection,
    pub(crate) code: usize,
    pub(crate) arguments: Vec<String>,
}

impl TymlDiagnositcMessage {
    pub fn section_name(&self, lang: &str, colored: bool) -> String {
        replace_message(
            get_text(format!("{}.name", self.section.section()).as_str(), lang),
            &vec![self.code.to_string()],
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
}

impl MessageSection {
    const fn section(&self) -> &'static str {
        match self {
            MessageSection::ParseError => "parse_error",
            MessageSection::TypeError => "type_error",
            MessageSection::ValidateError => "validate_error",
        }
    }
}

pub trait ToUnicodeCharacterRange {
    fn to_unicode_character_range(&self, source_code: &str) -> Range<usize>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DiagnosticSpan {
    /// 0 indexed, UTF-8 byte index
    UTF8Byte(Range<usize>),
    /// 0 indexed, Unicode character index
    UnicodeCharacter(Range<usize>),
}

impl Default for DiagnosticSpan {
    fn default() -> Self {
        DiagnosticSpan::UnicodeCharacter(0..0)
    }
}

#[extension_fn(Range<usize>)]
pub fn as_utf8_byte_range(&self) -> DiagnosticSpan {
    DiagnosticSpan::UTF8Byte(self.clone())
}

impl ToUnicodeCharacterRange for DiagnosticSpan {
    fn to_unicode_character_range(&self, source_code: &str) -> Range<usize> {
        match self {
            DiagnosticSpan::UTF8Byte(range) => {
                let start = source_code
                    .char_indices()
                    .map(|(position, _)| position)
                    .position(|position| position >= range.start)
                    .unwrap_or(source_code.len());
                let end = source_code
                    .char_indices()
                    .map(|(position, _)| position)
                    .position(|position| position >= range.end)
                    .unwrap_or(source_code.len());

                start..end
            }
            DiagnosticSpan::UnicodeCharacter(range) => range.clone(),
        }
    }
}

pub trait DiagnosticBuilder {
    fn build(&self, named_type_map: &NamedTypeMap) -> Diagnostic;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticLabel {
    pub kind: SourceCodeKind,
    pub span: DiagnosticSpan,
    pub color: Color,
    pub message_override: Option<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SourceCodeKind {
    Tyml,
    ValidateTraget,
}

pub struct Diagnostic {
    pub message: TymlDiagnositcMessage,
    pub labels: Vec<DiagnosticLabel>,
}

impl Diagnostic {
    pub fn print(&self, lang: &str, tyml_source: &str, validate_target_source: &str) {
        let section_name = self.message.section_name(lang, true);

        let get_label_span = |label: &DiagnosticLabel| match label.kind {
            SourceCodeKind::Tyml => label.span.to_unicode_character_range(tyml_source),
            SourceCodeKind::ValidateTraget => label
                .span
                .to_unicode_character_range(validate_target_source),
        };

        let mut builder = Report::build(
            ReportKind::Custom(section_name.as_str(), Color::White),
            self.labels.get(0).map(get_label_span).unwrap_or(0..0),
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

        builder.finish().print(Source::from(tyml_source)).unwrap();
    }
}
