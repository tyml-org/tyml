use std::{
    fs::File,
    io::Read,
    sync::{Arc, LazyLock, Mutex},
};

use regex::Regex;
use tyml::{TymlContext, Validated, tyml_generator::_ini_file_define, tyml_source::SourceCode};

#[derive(Debug)]
pub struct GeneratedLanguageServer {
    pub tyml: Mutex<Option<TymlContext<Validated>>>,
}

#[derive(Debug)]
pub enum TymlLSError {
    NotTyml,
    TymlReadError(std::io::Error),
}

impl GeneratedLanguageServer {
    pub fn new() -> Self {
        Self {
            tyml: Mutex::new(None),
        }
    }

    pub fn on_change(
        &self,
        source_code_name: Arc<String>,
        source_code: Arc<String>,
    ) -> Result<(), TymlLSError> {
        // !tyml is header of tyml
        static TYML_HEADER_REGEX: LazyLock<Regex> =
            LazyLock::new(|| Regex::new(r"\!tyml").unwrap());

        // header start must be until 64 bytes
        let Some(header_matched) = TYML_HEADER_REGEX.find_iter(&source_code[..64]).next() else {
            return Err(TymlLSError::NotTyml);
        };

        let header_source = source_code[header_matched.end()..].lines().next().unwrap();

        let header = TymlHeader::parse(header_source);

        let mut file = File::open(&header.tyml).map_err(|err| TymlLSError::TymlReadError(err))?;
        let mut tyml_source = String::new();
        file.read_to_string(&mut tyml_source)
            .map_err(|err| TymlLSError::TymlReadError(err))?;

        let tyml = TymlContext::new(SourceCode::new(header.tyml.clone(), tyml_source)).parse();

        let language = _ini_file_define();

        let tyml =
            tyml.ml_parse_and_validate(&language, &SourceCode::new(source_code_name, source_code));

        *self.tyml.lock().unwrap() = Some(tyml);

        Ok(())
    }
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
                style: Some(first_literal),
                tyml: Self::literal_tokenizer(second_source).0,
            },
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
        let mut chars = input.chars();

        let string_literal = match input.chars().next() {
            Some('"') => true,
            _ => false,
        };

        // skip -> "
        if string_literal {
            chars.next();
        }

        for char in input.chars() {
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
                    if char == ';' || char == '\'' || char == '"' {
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
