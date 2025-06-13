use std::{env::VarError, ops::Range, sync::LazyLock};

use either::Either;
use regex::Regex;
use shellexpand::{full, LookupError};

use crate::cache::get_cached_file;

#[derive(Debug, Clone)]
pub struct TymlHeader {
    pub style: Option<Result<String, LookupError<VarError>>>,
    pub tyml: Result<String, Either<LookupError<VarError>, String>>,
    pub span: Range<usize>,
}

impl TymlHeader {
    pub async fn parse(source: &str) -> Option<Self> {
        // !tyml is header of tyml
        static TYML_HEADER_REGEX: LazyLock<Regex> =
            LazyLock::new(|| Regex::new(r"\!tyml").unwrap());

        // header start must be until 64 bytes
        let Some(header_matched) = TYML_HEADER_REGEX
            .find_iter(&source[..128.min(source.len())])
            .next()
        else {
            return None;
        };

        let source = source[header_matched.end()..]
            .lines()
            .next()
            .unwrap_or(&source[header_matched.end()..]);

        let (first_literal, length) = Self::literal_tokenizer(source);

        let span = header_matched.start()..(header_matched.end() + source.len());

        let second_source = &source[length..];

        let mut header = match second_source.is_empty() {
            true => Self {
                style: None,
                tyml: Ok(first_literal),
                span,
            },
            false => {
                let second = Self::literal_tokenizer(second_source).0;

                if second.is_empty() {
                    Self {
                        style: None,
                        tyml: Ok(first_literal),
                        span,
                    }
                } else {
                    Self {
                        style: Some(Ok(first_literal)),
                        tyml: Ok(second),
                        span,
                    }
                }
            }
        };

        header.style = header
            .style
            .as_ref()
            .map(|style| full(style.as_ref().unwrap()).map(|resolved| resolved.into()));

        let tyml_path = header.tyml.as_ref().unwrap();
        if tyml_path.starts_with("http://") || tyml_path.starts_with("https://") {
            header.tyml = get_cached_file(tyml_path.as_str())
                .await
                .map(|cache| cache.to_string_lossy().to_string())
                .map_err(|error| Either::Right(error.to_string()));
        } else if tyml_path.starts_with("@") {
            let url = format!(
                "https://raw.githubusercontent.com/tyml-org/registry/refs/heads/main/{}.tyml",
                tyml_path.replace("@", "")
            );
            header.tyml = get_cached_file(url.as_str())
                .await
                .map(|cache| cache.to_string_lossy().to_string())
                .map_err(|error| Either::Right(error.to_string()));
        } else {
            header.tyml = full(tyml_path)
                .map(|resolved| resolved.into())
                .map_err(|error| Either::Left(error));
        }

        Some(header)
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
