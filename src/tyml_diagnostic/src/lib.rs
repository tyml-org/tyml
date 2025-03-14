use std::ops::Range;

use ariadne::{Color, Label, Report, ReportKind, Source};
use message::{ToCharacterRange, get_text, get_text_optional, replace_message};
use tyml_type::types::NamedTypeMap;

mod message;
pub mod parse_error;
pub mod type_error;

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
}

impl MessageSection {
    const fn section(&self) -> &'static str {
        match self {
            MessageSection::ParseError => "parse_error",
            MessageSection::TypeError => "type_error",
        }
    }
}

pub trait DiagnosticBuilder {
    fn build(&self, named_type_map: &NamedTypeMap) -> Diagnostic;
}

pub struct Diagnostic {
    pub message: TymlDiagnositcMessage,
    pub labels: Vec<(Range<usize>, Color)>,
}

impl Diagnostic {
    pub fn print(&self, lang: &str, source: &str) {
        let section_name = self.message.section_name(lang, true);

        let mut builder = Report::build(
            ReportKind::Custom(section_name.as_str(), Color::White),
            self.labels
                .get(0)
                .map(|(range, _)| range.clone())
                .unwrap_or(0..0)
                .to_character_range(source),
        );

        builder.set_message(self.message.message(lang, true));

        for (label_index, (span, color)) in self.labels.iter().enumerate() {
            let label_message = self.message.label(label_index, lang, true).unwrap();

            builder.add_label(
                Label::new(span.to_character_range(source))
                    .with_message(label_message)
                    .with_color(*color),
            );
        }

        if let Some(note) = self.message.note(lang, true) {
            builder.set_note(note);
        }

        if let Some(help) = self.message.help(lang, true) {
            builder.set_help(help);
        }

        builder.finish().print(Source::from(source)).unwrap();
    }
}
