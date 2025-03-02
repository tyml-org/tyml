use std::ops::Range;

use ariadne::Color;
use message::{get_text, get_text_optional, replace_message};

mod message;
pub mod parse_error;

pub(crate) struct TymlDiagnositcMessage {
    pub section: MessageSection,
    pub code: usize,
    pub arguments: Vec<String>,
}

impl TymlDiagnositcMessage {
    pub fn message(&self, lang: &str, colored: bool) -> String {
        replace_message(
            get_text(
                format!("{}.{}.message", &self.section.section(), self.code).as_str(),
                lang,
            ),
            &self.arguments,
            colored,
        )
    }

    pub fn labels(&self, lang: &str, colored: bool) -> Vec<String> {
        let mut labels = Vec::new();
        for i in 0..usize::MAX {
            let Some(text) = get_text_optional(
                format!("{}.{}.label_{}", &self.section.section(), self.code, i).as_str(),
                lang,
            ) else {
                break;
            };
            labels.push(replace_message(text, &self.arguments, colored));
        }

        labels
    }

    pub fn note(&self, lang: &str, colored: bool) -> Option<String> {
        get_text_optional(
            format!("{}.{}.note", &self.section.section(), self.code).as_str(),
            lang,
        )
        .map(|text| replace_message(text, &self.arguments, colored))
    }

    pub fn help(&self, lang: &str, colored: bool) -> Option<String> {
        get_text_optional(
            format!("{}.{}.help", &self.section.section(), self.code).as_str(),
            lang,
        )
        .map(|text| replace_message(text, &self.arguments, colored))
    }
}

pub(crate) enum MessageSection {
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
    fn build(&self) -> Diagnostic;
}

pub struct Diagnostic {
    message: TymlDiagnositcMessage,
    labels: Vec<(Range<usize>, Color)>,
}
