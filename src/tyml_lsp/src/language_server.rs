use std::{
    collections::BTreeMap,
    fs::File,
    io::Read,
    ops::Range,
    sync::{
        Arc, LazyLock, Mutex,
        atomic::{AtomicBool, Ordering},
    },
};

use regex::Regex;
use tower_lsp::{
    Client,
    lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString, Position, SemanticTokenType, Url},
};
use tyml::{
    TymlContext, Validated,
    tyml_diagnostic::{DiagnosticBuilder, message::get_text},
    tyml_generator::{_ini_file_define, style::ASTTokenKind},
    tyml_source::{AsUtf8ByteRange, SourceCode, SourceCodeSpan},
    tyml_type::types::NamedTypeMap,
};

type LSPRange = tower_lsp::lsp_types::Range;

#[derive(Debug)]
pub struct GeneratedLanguageServer {
    pub url: Url,
    pub lang: &'static str,
    pub tyml: Mutex<Option<TymlContext<Validated>>>,
    pub header_span: Mutex<Range<usize>>,
    pub has_tyml_file_error: AtomicBool,
    pub tyml_file_name: Mutex<String>,
    pub tokens: Mutex<Arc<Vec<(SemanticTokenType, LSPRange)>>>,
}

impl GeneratedLanguageServer {
    pub fn new(url: Url, lang: &'static str) -> Self {
        Self {
            url,
            lang,
            tyml: Mutex::new(None),
            header_span: Mutex::new(0..0),
            has_tyml_file_error: AtomicBool::new(false),
            tyml_file_name: Mutex::new(String::new()),
            tokens: Mutex::new(Arc::new(Vec::new())),
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
        let Some(header_matched) = TYML_HEADER_REGEX
            .find_iter(&source_code[..64.min(source_code.len())])
            .next()
        else {
            return Err(());
        };

        let header_source = source_code[header_matched.end()..]
            .lines()
            .next()
            .unwrap_or(&source_code[header_matched.end()..]);

        let header = TymlHeader::parse(header_source);

        let file = File::open(&header.tyml).ok();
        let file_open_result = file.is_some();

        let mut tyml_source = String::new();
        file.map(|mut file| file.read_to_string(&mut tyml_source));

        *self.header_span.lock().unwrap() =
            header_matched.start()..(header_matched.end() + header_source.len());
        *self.tyml_file_name.lock().unwrap() = header.tyml.clone();

        if !file_open_result {
            self.has_tyml_file_error.store(true, Ordering::Release);
            tyml_source = "*: any".to_string();
        }

        let tyml = TymlContext::new(SourceCode::new(header.tyml.clone(), tyml_source)).parse();

        let language = _ini_file_define();

        let mut tokens = BTreeMap::new();

        let tyml = tyml.ml_parse_and_validate(
            &language,
            &SourceCode::new(source_code_name, source_code.clone()),
            Some(&mut tokens),
        );

        let mut semantic_tokens = Vec::new();
        for (kind, span) in tokens.values() {
            let kind = match kind {
                ASTTokenKind::Section => SemanticTokenType::TYPE,
                ASTTokenKind::Key => SemanticTokenType::VARIABLE,
                ASTTokenKind::TreeKey => SemanticTokenType::TYPE,
                ASTTokenKind::NumericValue => SemanticTokenType::NUMBER,
                ASTTokenKind::InfNan => SemanticTokenType::KEYWORD,
                ASTTokenKind::StringValue => SemanticTokenType::STRING,
                ASTTokenKind::BoolValue => SemanticTokenType::KEYWORD,
                ASTTokenKind::Comment => SemanticTokenType::COMMENT,
            };
            semantic_tokens.push((kind, span.as_utf8_byte_range().to_lsp_span(&source_code)));
        }
        *self.tokens.lock().unwrap() = Arc::new(semantic_tokens);

        *self.tyml.lock().unwrap() = Some(tyml);

        Ok(())
    }

    pub async fn publish_analyzed_info(&self, client: &Client) {
        let tyml = self.tyml.lock().unwrap().clone().unwrap();
        let header_span = self.header_span.lock().unwrap().clone();

        if self.has_tyml_file_error.load(Ordering::Acquire) {
            let tyml_file_name = self.tyml_file_name.lock().unwrap().clone();

            client
                .publish_diagnostics(
                    self.url.clone(),
                    vec![Diagnostic {
                        range: header_span
                            .as_utf8_byte_range()
                            .to_lsp_span(&tyml.ml_source_code().code),
                        severity: Some(DiagnosticSeverity::WARNING),
                        message: get_text("lsp.message.tyml_file_error", self.lang)
                            .replace("%0", tyml_file_name.as_str()),
                        ..Default::default()
                    }],
                    None,
                )
                .await;
        } else {
            let mut diagnostics = Vec::new();
            for error in tyml.ml_parse_error().iter() {
                let diagnostic = error.build(&mut NamedTypeMap::new(&Default::default()));

                diagnostics.push(Diagnostic {
                    range: diagnostic.labels[0]
                        .span
                        .to_lsp_span(&tyml.ml_source_code().code),
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: Some(NumberOrString::Number(diagnostic.message.code as _)),
                    message: diagnostic.message.message(self.lang, false),
                    ..Default::default()
                });

                diagnostics.push(Diagnostic {
                    range: diagnostic.labels[1]
                        .span
                        .to_lsp_span(&tyml.ml_source_code().code),
                    severity: Some(DiagnosticSeverity::HINT),
                    code: Some(NumberOrString::Number(diagnostic.message.code as _)),
                    message: diagnostic.message.label(1, self.lang, false).unwrap(),
                    ..Default::default()
                });
            }

            if diagnostics.is_empty() {
                for error in tyml.ml_validate_error().iter() {
                    let diagnostic = error.build(&mut NamedTypeMap::new(&Default::default()));

                    diagnostics.push(Diagnostic {
                        range: diagnostic.labels[0]
                            .span
                            .to_lsp_span(&tyml.ml_source_code().code),
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: Some(NumberOrString::Number(diagnostic.message.code as _)),
                        message: diagnostic.message.message(self.lang, false),
                        ..Default::default()
                    });
                }
            }

            client
                .publish_diagnostics(self.url.clone(), diagnostics, None)
                .await;
        }
    }
}

trait ToLSPSpan {
    fn to_lsp_span(&self, code: &str) -> LSPRange;
}

impl ToLSPSpan for SourceCodeSpan {
    fn to_lsp_span(&self, code: &str) -> LSPRange {
        match self {
            SourceCodeSpan::UTF8Byte(range) => LSPRange::new(
                to_line_column(code, range.start),
                to_line_column(code, range.end),
            ),
            SourceCodeSpan::UnicodeCharacter(range) => LSPRange::new(
                to_line_column(code, charactor_to_byte(code, range.start)),
                to_line_column(code, charactor_to_byte(code, range.end)),
            ),
        }
    }
}

fn charactor_to_byte(code: &str, charactor: usize) -> usize {
    code.char_indices()
        .position(|(index, _)| index == charactor)
        .unwrap_or(code.len())
}

fn to_line_column(code: &str, byte: usize) -> Position {
    static LINE_REGEX: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"[^\n\r]*(\n|\r|\r\n|$)").unwrap());

    let mut last_line_index = 0;
    let mut last_line = "";
    for (line, line_matched) in LINE_REGEX.find_iter(code).enumerate() {
        if (line_matched.start()..line_matched.end()).contains(&byte) {
            let column = code[line_matched.start()..byte].chars().count();

            return Position::new((line + 1) as _, column as _);
        }

        last_line_index = line;
        last_line = &code[line_matched.start()..line_matched.end()];
    }

    Position::new(
        (last_line_index + 1) as _,
        last_line
            .chars()
            .filter(|&char| char != '\n' && char != '\r')
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
            true => Self {
                style: None,
                tyml: first_literal,
            },
            false => {
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
