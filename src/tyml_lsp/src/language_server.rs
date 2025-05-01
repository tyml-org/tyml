use std::{
    fs::File,
    io::Read,
    ops::Range,
    sync::{Arc, LazyLock, Mutex},
};

use regex::Regex;
use tower_lsp::{
    Client,
    lsp_types::{Diagnostic, Position, Url},
};
use tyml::{
    TymlContext, Validated,
    tyml_generator::_ini_file_define,
    tyml_source::{SourceCode, SourceCodeSpan},
};

type LSPRange = tower_lsp::lsp_types::Range;

#[derive(Debug)]
pub struct GeneratedLanguageServer {
    pub url: Url,
    pub tyml: Mutex<Option<TymlContext<Validated>>>,
    pub tyml_file_error: Mutex<Option<Range<usize>>>,
}

impl GeneratedLanguageServer {
    pub fn new(url: Url) -> Self {
        Self {
            url,
            tyml: Mutex::new(None),
            tyml_file_error: Mutex::new(None),
        }
    }

    pub fn on_change(
        &self,
        source_code_name: Arc<String>,
        source_code: Arc<String>,
    ) -> Result<(), ()> {
        // !tyml is header of tyml
        static TYML_HEADER_REGEX: LazyLock<Regex> =
            LazyLock::new(|| Regex::new(r"\!tyml").unwrap());

        // header start must be until 64 bytes
        let Some(header_matched) = TYML_HEADER_REGEX.find_iter(&source_code[..64]).next() else {
            return Err(());
        };

        let header_source = source_code[header_matched.end()..].lines().next().unwrap();

        let header = TymlHeader::parse(header_source);

        let file = File::open(&header.tyml).ok();
        let file_open_result = file.is_some();

        let mut tyml_source = String::new();
        file.map(|mut file| file.read_to_string(&mut tyml_source));

        let mut file_error_warning = None;

        if !file_open_result {
            file_error_warning =
                Some(header_matched.start()..(header_matched.end() + header_source.len()));
            tyml_source = "*: any".to_string();
        }

        let tyml = TymlContext::new(SourceCode::new(header.tyml.clone(), tyml_source)).parse();

        let language = _ini_file_define();

        let tyml =
            tyml.ml_parse_and_validate(&language, &SourceCode::new(source_code_name, source_code));

        *self.tyml.lock().unwrap() = Some(tyml);

        *self.tyml_file_error.lock().unwrap() = file_error_warning;

        Ok(())
    }

    pub fn publish_analyzed_info(&self, client: &Client) {
        let tyml = self.tyml.lock().unwrap().clone().unwrap();

        let tyml_file_error = self.tyml_file_error.lock().unwrap().clone();

        if let Some(error_span) = tyml_file_error {
            client.publish_diagnostics(
                self.url.clone(),
                vec![Diagnostic {
                    range: LSPRange::new(start, end),
                    severity: todo!(),
                    code: todo!(),
                    message: todo!(),
                }],
                None,
            );
        } else {
        }
    }
}

trait ToLSPSpan {
    fn to_lsp_span(&self, code: &str) -> LSPRange;
}

impl ToLSPSpan for SourceCodeSpan {
    fn to_lsp_span(&self, code: &str) -> LSPRange {
        match self {
            SourceCodeSpan::UTF8Byte(range) => {
                todo!()
            }
            SourceCodeSpan::UnicodeCharacter(range) => todo!(),
        }
    }
}

fn to_line_column(code: &str, byte: usize) -> Position {
    static LINE_REGEX: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"[^\n\r]+(\n|\r|\r\n|$)").unwrap());

    let mut last_line_index = 0;
    let mut last_line = "";
    for (line, line_matched) in LINE_REGEX.find_iter(code).enumerate() {
        if (line_matched.start()..line_matched.end()).contains(&byte) {
            let byte_column = byte - line_matched.start();
            let column = code[line_matched.start()..byte_column]
                .chars()
                .filter(|char| !char.is_whitespace())
                .count();

            return Position::new((line + 1) as _, column as _);
        }

        last_line_index = line;
        last_line = &code[line_matched.start()..line_matched.end()];
    }

    Position::new(
        (last_line_index + 1) as _,
        last_line
            .chars()
            .filter(|char| !char.is_whitespace())
            .count() as _,
    )
}

pub struct TymlHeader {
    pub style: Option<String>,
    pub tyml: String,
}

impl TymlHeader {
    pub fn parse(source: &str) -> Self {
        let (first_literal, length) = Self::literal_tokenizer(source);

        let second_source = &source[length..];

        match second_source.is_empty() {
            true => {
                let second = Self::literal_tokenizer(second_source).0;

                if second.is_empty() {
                    Self {
                        style: None,
                        tyml: first_literal,
                    }
                } else {
                    Self {
                        style: Some(first_literal),
                        tyml: second,
                    }
                }
            }
            false => Self {
                style: None,
                tyml: first_literal,
            },
        }
    }

    fn literal_tokenizer(input: &str) -> (String, usize) {
        let mut parsed_literal = String::new();
        let mut literal_length = 0;
        let mut prev_char = '\0';
        let mut chars = input.chars().peekable();

        // skip whitespace
        loop {
            let Some(&char) = chars.peek() else { break };

            if char.is_whitespace() {
                literal_length += char.len_utf8();
                chars.next();
            } else {
                break;
            }
        }

        let string_literal = match input.chars().next() {
            Some('"') => true,
            _ => false,
        };

        // skip -> "
        if string_literal {
            chars.next();
            literal_length += '"'.len_utf8();
        }

        for char in chars {
            literal_length += char.len_utf8();

            if prev_char == '\\' {
                parsed_literal.push(char);

                if char == '\\' {
                    prev_char = '\0';
                    continue;
                }
            } else if char != '\\' {
                if string_literal {
                    if char == '"' {
                        break;
                    }
                } else {
                    if char == ';' || char == '\'' || char == '"' || char == ' ' || char == '　' {
                        break;
                    }
                }
                parsed_literal.push(char);
            }

            prev_char = char;
        }

        (parsed_literal, literal_length)
    }
}
