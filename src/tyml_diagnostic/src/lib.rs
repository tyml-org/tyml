use message::{get_text, replace_message};

mod message;

pub(crate) struct TymlDiagnositcMessage {
    pub section: MessageSection,
    pub code: usize,
    pub arguments: Vec<String>,
    pub colored: bool,
    pub lang: String,
}

impl TymlDiagnositcMessage {
    pub fn message(&self) -> String {
        replace_message(
            get_text(
                format!("{}.{}.message", &self.section.section(), self.code).as_str(),
                self.lang.as_str(),
            ),
            &self.arguments,
            self.colored,
        )
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
