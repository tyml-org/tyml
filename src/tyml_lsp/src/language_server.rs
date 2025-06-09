use std::{
    collections::BTreeMap,
    fs::File,
    io::Read,
    ops::{Range, RangeInclusive},
    path::Path,
    sync::{
        Arc, LazyLock, Mutex,
        atomic::{AtomicBool, Ordering},
    },
};

use either::Either;
use extension_fn::extension_fn;
use regex::Regex;
use tower_lsp::{
    Client,
    lsp_types::{
        CompletionItem, CompletionItemKind, Diagnostic, DiagnosticSeverity, Documentation,
        MarkupContent, MarkupKind, NumberOrString, Position, SemanticTokenType, Url,
    },
};
use tyml::{
    Parsed, TymlContext, Validated,
    header::TymlHeader,
    tyml_diagnostic::{DiagnosticBuilder, message::get_text},
    tyml_generator::{
        registry::STYLE_REGISTRY,
        style::{ASTTokenKind, language::LanguageStyle},
    },
    tyml_source::{AsUtf8ByteRange, SourceCode, SourceCodeSpan, ToByteSpan},
    tyml_type::types::{NamedTypeMap, NamedTypeTree, Type, TypeTree},
    tyml_validate::{
        error::TymlValueValidateError,
        validate::{MergedValueTree, ValidateValue, ValueTypeChecker},
    },
};

type LSPRange = tower_lsp::lsp_types::Range;

#[derive(Debug)]
pub struct GeneratedLanguageServer {
    pub url: Url,
    pub lang: &'static str,
    pub tyml: Mutex<Option<(TymlContext<Validated>, TymlHeader)>>,
    pub has_tyml_file_error: AtomicBool,
    pub style_not_found: AtomicBool,
    pub tyml_file_name: Mutex<String>,
    pub tokens: Mutex<Arc<Vec<(SemanticTokenType, (Position, usize))>>>,
    pub language_style: Mutex<Arc<LanguageStyle>>,
}

impl GeneratedLanguageServer {
    pub fn new(url: Url, lang: &'static str) -> Self {
        Self {
            url,
            lang,
            tyml: Mutex::new(None),
            has_tyml_file_error: AtomicBool::new(false),
            style_not_found: AtomicBool::new(false),
            tyml_file_name: Mutex::new(String::new()),
            tokens: Mutex::new(Arc::new(Vec::new())),
            language_style: Mutex::new(STYLE_REGISTRY.resolve("").unwrap()),
        }
    }

    pub async fn on_change(&self, source_code_name: Arc<String>, source_code: Arc<String>) {
        let header = TymlHeader::parse(&source_code).await;

        let mut other_file_name = None;

        match header {
            Some(mut header) => {
                let file = header
                    .tyml
                    .as_ref()
                    .map(|tyml| {
                        File::open(tyml)
                            .ok()
                            .or(Path::new(source_code_name.as_str())
                                .parent()
                                .map(|parent| {
                                    let other_file_name_temp = parent
                                        .join(tyml.as_str())
                                        .to_string_lossy()
                                        .to_string()
                                        .replacen("file://", "", 1);
                                    other_file_name = Some(other_file_name_temp.clone());

                                    File::open(other_file_name_temp).ok()
                                })
                                .flatten())
                    })
                    .ok()
                    .flatten()
                    .filter(|file| {
                        file.metadata()
                            .map(|metadata| metadata.is_file())
                            .unwrap_or(false)
                    });

                if let Some(other_file_name) = other_file_name {
                    if let Ok(tyml) = header.tyml.as_mut() {
                        *tyml = other_file_name;
                    }
                }

                let file_open_result = file.is_some();

                let mut tyml_source = String::new();
                file.map(|mut file| file.read_to_string(&mut tyml_source));

                if let Ok(tyml) = &header.tyml {
                    *self.tyml_file_name.lock().unwrap() = tyml.clone();
                }

                self.has_tyml_file_error
                    .store(!file_open_result, Ordering::Release);

                if !file_open_result {
                    tyml_source = "*: any".to_string();
                }

                let tyml = TymlContext::new(SourceCode::new(
                    header
                        .tyml
                        .as_ref()
                        .map(|tyml| tyml.as_str())
                        .clone()
                        .unwrap_or("error")
                        .to_string(),
                    tyml_source,
                ))
                .parse();

                let url = self.url.to_string();
                let style_fall_back = url.split(".").last().unwrap_or("");
                let style = header
                    .style
                    .as_ref()
                    .map(|style| style.as_ref().map(|style| style.as_str()).ok())
                    .flatten()
                    .unwrap_or(style_fall_back);

                let language = STYLE_REGISTRY
                    .resolve(style)
                    .unwrap_or(STYLE_REGISTRY.resolve("").unwrap());

                self.style_not_found
                    .store(STYLE_REGISTRY.resolve(style).is_none(), Ordering::Release);

                let mut tokens = BTreeMap::new();

                let tyml = tyml.ml_parse_and_validate(
                    &language,
                    &SourceCode::new(source_code_name, source_code.clone()),
                    Some(&mut tokens),
                );

                let mut semantic_tokens = Vec::new();
                for (kind, span) in tokens.values() {
                    let kind = match kind {
                        ASTTokenKind::Section => SemanticTokenType::STRUCT,
                        ASTTokenKind::Key => SemanticTokenType::PROPERTY,
                        ASTTokenKind::TreeKey => SemanticTokenType::TYPE,
                        ASTTokenKind::NumericValue => SemanticTokenType::NUMBER,
                        ASTTokenKind::InfNan => SemanticTokenType::KEYWORD,
                        ASTTokenKind::StringValue => SemanticTokenType::STRING,
                        ASTTokenKind::BoolValue => SemanticTokenType::KEYWORD,
                        ASTTokenKind::Comment => SemanticTokenType::COMMENT,
                    };
                    for token in span
                        .as_utf8_byte_range()
                        .to_lsp_semantic_token(&source_code)
                    {
                        semantic_tokens.push((kind.clone(), token));
                    }
                }
                *self.tokens.lock().unwrap() = Arc::new(semantic_tokens);

                *self.tyml.lock().unwrap() = Some((tyml, header));

                *self.language_style.lock().unwrap() = language;
            }
            None => {
                let url = self.url.to_string();
                let style = url.split(".").last().unwrap_or("");

                let language = STYLE_REGISTRY
                    .resolve(style)
                    .unwrap_or(STYLE_REGISTRY.resolve("").unwrap());

                // empty tyml
                let tyml =
                    TymlContext::new(SourceCode::new("".to_string(), "*: any".to_string())).parse();

                let mut tokens = BTreeMap::new();

                let tyml = tyml.ml_parse_and_validate(
                    &language,
                    &SourceCode::new(source_code_name, source_code.clone()),
                    Some(&mut tokens),
                );

                let dummy_header = TymlHeader::parse("!tyml").await.unwrap();

                let mut semantic_tokens = Vec::new();
                for (kind, span) in tokens.values() {
                    let kind = match kind {
                        ASTTokenKind::Section => SemanticTokenType::STRUCT,
                        ASTTokenKind::Key => SemanticTokenType::PROPERTY,
                        ASTTokenKind::TreeKey => SemanticTokenType::TYPE,
                        ASTTokenKind::NumericValue => SemanticTokenType::NUMBER,
                        ASTTokenKind::InfNan => SemanticTokenType::KEYWORD,
                        ASTTokenKind::StringValue => SemanticTokenType::STRING,
                        ASTTokenKind::BoolValue => SemanticTokenType::KEYWORD,
                        ASTTokenKind::Comment => SemanticTokenType::COMMENT,
                    };
                    for token in span
                        .as_utf8_byte_range()
                        .to_lsp_semantic_token(&source_code)
                    {
                        semantic_tokens.push((kind.clone(), token));
                    }
                }
                *self.tokens.lock().unwrap() = Arc::new(semantic_tokens);

                *self.tyml.lock().unwrap() = Some((tyml, dummy_header));

                *self.language_style.lock().unwrap() = language;
            }
        }
    }

    pub async fn publish_diagnostics(&self, client: &Client) {
        let (tyml, header) = self.tyml.lock().unwrap().clone().unwrap();

        if self.has_tyml_file_error.load(Ordering::Acquire)
            || self.style_not_found.load(Ordering::Acquire)
        {
            let tyml_file_name = self.tyml_file_name.lock().unwrap().clone();

            if let Some(Err(error)) = header.style {
                client
                    .publish_diagnostics(
                        self.url.clone(),
                        vec![Diagnostic {
                            range: header
                                .span
                                .as_utf8_byte_range()
                                .to_lsp_span(&tyml.ml_source_code().code),
                            severity: Some(DiagnosticSeverity::WARNING),
                            message: get_text("lsp.message.header_var_lookup_error", self.lang)
                                .replace("%0", &error.var_name),
                            ..Default::default()
                        }],
                        None,
                    )
                    .await;
            } else if let Err(error) = header.tyml {
                match error {
                    Either::Left(lookup_error) => {
                        client
                            .publish_diagnostics(
                                self.url.clone(),
                                vec![Diagnostic {
                                    range: header
                                        .span
                                        .as_utf8_byte_range()
                                        .to_lsp_span(&tyml.ml_source_code().code),
                                    severity: Some(DiagnosticSeverity::WARNING),
                                    message: get_text(
                                        "lsp.message.header_var_lookup_error",
                                        self.lang,
                                    )
                                    .replace("%0", &lookup_error.var_name),
                                    ..Default::default()
                                }],
                                None,
                            )
                            .await;
                    }
                    Either::Right(_) => todo!(),
                }
            } else if self.style_not_found.load(Ordering::Relaxed) {
                client
                    .publish_diagnostics(
                        self.url.clone(),
                        vec![Diagnostic {
                            range: header
                                .span
                                .as_utf8_byte_range()
                                .to_lsp_span(&tyml.ml_source_code().code),
                            severity: Some(DiagnosticSeverity::WARNING),
                            message: get_text("lsp.message.style_not_found", self.lang)
                                .replace("%0", header.style.unwrap().unwrap().as_str()),
                            ..Default::default()
                        }],
                        None,
                    )
                    .await;
            } else {
                client
                    .publish_diagnostics(
                        self.url.clone(),
                        vec![Diagnostic {
                            range: header
                                .span
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
            }
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
                    message: format!(
                        "{}: {}\n{}",
                        diagnostic.message.section_name(self.lang, false),
                        diagnostic.message.message(self.lang, false),
                        diagnostic.message.label(1, self.lang, false).unwrap(),
                    ),
                    ..Default::default()
                });
            }

            if diagnostics.is_empty() {
                for error in tyml.ml_validate_error().iter() {
                    let diagnostic = error.build(&mut NamedTypeMap::new(&Default::default()));

                    if let TymlValueValidateError::InvalidValue {
                        found,
                        expected: _,
                        path: _,
                        caused_by: _,
                    } = error
                    {
                        for found in found.iter() {
                            diagnostics.push(Diagnostic {
                                range: found.to_lsp_span(&tyml.ml_source_code().code),
                                severity: Some(DiagnosticSeverity::ERROR),
                                code: Some(NumberOrString::Number(diagnostic.message.code as _)),
                                message: format!(
                                    "{}: {}\n{}\n{}\n{}",
                                    diagnostic.message.section_name(self.lang, false),
                                    diagnostic.message.message(self.lang, false),
                                    diagnostic.message.label(0, self.lang, false).unwrap(),
                                    diagnostic
                                        .message
                                        .note(self.lang, false)
                                        .unwrap_or(String::new()),
                                    get_text("lsp.message.array_error_diagnostic", self.lang)
                                ),
                                ..Default::default()
                            });
                        }
                    } else {
                        diagnostics.push(Diagnostic {
                            range: diagnostic.labels[0]
                                .span
                                .to_lsp_span(&tyml.ml_source_code().code),
                            severity: Some(DiagnosticSeverity::ERROR),
                            code: Some(NumberOrString::Number(diagnostic.message.code as _)),
                            message: format!(
                                "{}: {}\n{}{}",
                                diagnostic.message.section_name(self.lang, false),
                                diagnostic.message.message(self.lang, false),
                                diagnostic.message.label(0, self.lang, false).unwrap(),
                                diagnostic
                                    .message
                                    .note(self.lang, false)
                                    .map(|note| format!("\n{}", note))
                                    .unwrap_or(String::new())
                            ),
                            ..Default::default()
                        });
                    }
                }
            }

            client
                .publish_diagnostics(self.url.clone(), diagnostics, None)
                .await;
        }
    }

    pub fn provide_completion(&self, position: Position) -> Option<Vec<CompletionItem>> {
        let Some(tyml) = self.tyml.lock().unwrap().as_ref().cloned() else {
            return None;
        };

        let byte_position = position.to_byte_position(&tyml.0.ml_source_code().code);

        let non_whitespace_byte_position = tyml.0.ml_source_code().code[..byte_position]
            .char_indices()
            .rev()
            .find_map(|(i, ch)| {
                if ch.is_whitespace() || ch == '.' {
                    None
                } else {
                    Some(i + ch.len_utf8())
                }
            })
            .unwrap_or(0);

        let mut completions = Vec::new();

        tyml.0.validator().provide_completion(
            &tyml.0.tyml_source.code,
            non_whitespace_byte_position,
            &mut completions,
        );

        Some(
            completions
                .into_iter()
                .map(|completion| {
                    let documents = match completion.documents.is_empty() {
                        true => create_hover_code_block(
                            &tyml.0.tyml_source.code[completion.span.clone()],
                        ),
                        false => format!(
                            "{}{}",
                            create_hover_code_block(
                                &tyml.0.tyml_source.code[completion.span.clone()]
                            ),
                            completion.documents
                        ),
                    };

                    CompletionItem {
                        label: completion.name.clone(),
                        kind: Some(completion.kind.to_completion_item_kind()),
                        documentation: Some(Documentation::MarkupContent(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: documents,
                        })),
                        ..Default::default()
                    }
                })
                .collect(),
        )
    }

    pub fn goto_define(&self, position: Position) -> (String, Vec<LSPRange>) {
        let Some((tyml, header)) = self.tyml.lock().unwrap().as_ref().cloned() else {
            return (String::new(), Vec::new());
        };

        if let Ok(tyml) = &header.tyml {
            if tyml.is_empty() {
                return (String::new(), Vec::new());
            }
        }

        let byte_position = position.to_byte_position(&tyml.ml_source_code().code);

        if header.span.to_inclusive().contains(&byte_position) {
            return (
                header.tyml.unwrap_or_default(),
                vec![
                    (0..tyml.tyml_source.code.len())
                        .as_utf8_byte_range()
                        .to_lsp_span(&tyml.tyml_source.code),
                ],
            );
        }

        let defines = tyml
            .validator()
            .goto_define_and_documents(byte_position, &tyml.ml_source_code().code);

        let defines = defines
            .into_iter()
            .map(|range| {
                range
                    .0
                    .as_utf8_byte_range()
                    .to_lsp_span(&tyml.tyml_source.code)
            })
            .collect();

        (header.tyml.unwrap_or_default(), defines)
    }

    pub fn hover(&self, position: Position) -> Option<String> {
        let Some((tyml, _)) = self.tyml.lock().unwrap().clone() else {
            return None;
        };

        let byte_position = position.to_byte_position(&tyml.ml_source_code().code);

        let documents = tyml
            .validator()
            .goto_define_and_documents(byte_position, &tyml.ml_source_code().code);

        if let Some((span, documents)) = documents.get(0) {
            let documents = documents
                .iter()
                .map(|line| line.to_string())
                .collect::<Vec<_>>()
                .join("");

            Some(format!(
                "{}{}",
                create_hover_code_block(&tyml.tyml_source.code[span.clone()]),
                documents
            ))
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct TymlLanguageServer {
    pub url: Url,
    pub lang: &'static str,
    pub tyml: Mutex<Option<TymlContext<Parsed>>>,
    pub tokens: Mutex<Arc<Vec<(SemanticTokenType, (Position, usize))>>>,
}

impl TymlLanguageServer {
    pub fn new(url: Url, lang: &'static str) -> Self {
        Self {
            url,
            lang,
            tyml: Mutex::new(None),
            tokens: Mutex::new(Arc::new(Vec::new())),
        }
    }

    pub fn on_change(&self, name: String, code: String) {
        let (tyml, changed) = match self.tyml.lock().unwrap().clone() {
            Some(old_tyml) => {
                if code.as_str() != old_tyml.tyml_source.code.as_str() {
                    (TymlContext::new(SourceCode::new(name, code)).parse(), true)
                } else {
                    (old_tyml, false)
                }
            }
            None => (TymlContext::new(SourceCode::new(name, code)).parse(), true),
        };

        if changed {
            let mut tokens = BTreeMap::new();
            tyml_semantic_tokens::collect_tokens_for_defines(&tyml.tyml().ast(), &mut tokens);

            for comment_range in tyml.tyml().comment_ranges().iter() {
                tokens.insert(
                    comment_range.start,
                    (SemanticTokenType::COMMENT, comment_range.clone()),
                );
            }

            let mut semantic_tokens = Vec::new();
            for (token_type, span) in tokens.values() {
                for token in span
                    .as_utf8_byte_range()
                    .to_lsp_semantic_token(&tyml.tyml_source.code)
                {
                    semantic_tokens.push((token_type.clone(), token));
                }
            }
            *self.tokens.lock().unwrap() = Arc::new(semantic_tokens);
        }

        *self.tyml.lock().unwrap() = Some(tyml);
    }

    pub async fn publish_diagnostics(&self, client: &Client) {
        let Some(tyml) = self.tyml.lock().unwrap().clone() else {
            return;
        };

        let mut diagnostics = Vec::new();
        for error in tyml.tyml().parse_errors() {
            let diagnostic = error.build(&mut NamedTypeMap::new(&Default::default()));

            diagnostics.push(Diagnostic {
                range: diagnostic.labels[0]
                    .span
                    .to_lsp_span(&tyml.tyml_source.code),
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::Number(diagnostic.message.code as _)),
                message: format!(
                    "{}: {}",
                    diagnostic.message.section_name(self.lang, false),
                    diagnostic.message.message(self.lang, false),
                ),
                ..Default::default()
            });
        }

        if diagnostics.is_empty() {
            for error in tyml.tyml().type_errors() {
                let diagnostic = error.build(&mut NamedTypeMap::new(&Default::default()));

                diagnostics.push(Diagnostic {
                    range: diagnostic.labels[0]
                        .span
                        .to_lsp_span(&tyml.tyml_source.code),
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: Some(NumberOrString::Number(diagnostic.message.code as _)),
                    message: format!(
                        "{}: {}",
                        diagnostic.message.section_name(self.lang, false),
                        diagnostic.message.message(self.lang, false),
                    ),
                    ..Default::default()
                });
            }
        }

        client
            .publish_diagnostics(self.url.clone(), diagnostics, None)
            .await;
    }

    pub fn provide_completion(&self, position: Position) -> Option<Vec<CompletionItem>> {
        let Some(tyml) = self.tyml.lock().unwrap().clone() else {
            return None;
        };

        let byte_position = position.to_byte_position(&tyml.tyml_source.code);

        let non_whitespace_byte_position = tyml.tyml_source.code[..byte_position]
            .char_indices()
            .rev()
            .find_map(|(i, ch)| {
                if ch.is_whitespace() {
                    None
                } else {
                    Some(i + ch.len_utf8())
                }
            })
            .unwrap_or(0);

        let ast = tyml.tyml().ast();

        let mut completions = Vec::new();
        if on_type_tag::is_position_on_type_tag_for_defines(
            ast,
            non_whitespace_byte_position,
            &mut Vec::new(),
            &mut completions,
        ) {
            Some(
                completions
                    .into_iter()
                    .map(|(name, kind)| CompletionItem {
                        label: name.to_string(),
                        kind: Some(kind),
                        ..Default::default()
                    })
                    .collect(),
            )
        } else {
            None
        }
    }

    pub fn goto_define(&self, position: Position) -> Option<LSPRange> {
        let Some(tyml) = self.tyml.lock().unwrap().clone() else {
            return None;
        };

        let byte_position = position.to_byte_position(&tyml.tyml_source.code);

        let named_type_map = tyml.tyml().named_type_map();

        let name_id = named_type_map
            .use_link_map
            .iter()
            .find(|(_, ranges)| {
                ranges
                    .iter()
                    .any(|range| range.to_inclusive().contains(&byte_position))
            })
            .map(|(&name_id, _)| name_id);

        name_id.map(|name_id| {
            named_type_map
                .get_define_span(name_id)
                .unwrap()
                .as_utf8_byte_range()
                .to_lsp_span(&tyml.tyml_source.code)
        })
    }

    pub fn get_references(&self, position: Position) -> Vec<LSPRange> {
        let Some(tyml) = self.tyml.lock().unwrap().clone() else {
            return Vec::new();
        };

        let byte_position = position.to_byte_position(&tyml.tyml_source.code);

        let named_type_map = tyml.tyml().named_type_map();

        let name_id = named_type_map
            .use_link_map
            .iter()
            .find(|(_, ranges)| {
                ranges
                    .iter()
                    .any(|range| range.to_inclusive().contains(&byte_position))
            })
            .map(|(&name_id, _)| name_id);

        let Some(name_id) = name_id else {
            return Vec::new();
        };

        let Some(users) = named_type_map.use_link_map.get(&name_id) else {
            return Vec::new();
        };

        users
            .iter()
            .map(|span| {
                span.as_utf8_byte_range()
                    .to_lsp_span(&tyml.tyml_source.code)
            })
            .collect()
    }

    pub fn hover(&self, position: Position) -> Option<String> {
        let Some(tyml) = self.tyml.lock().unwrap().clone() else {
            return None;
        };

        let byte_position = position.to_byte_position(&tyml.tyml_source.code);

        let mut result = None;
        tyml_documents_from_ast::get_documents_from_defines(
            tyml.tyml().ast(),
            byte_position,
            &mut result,
        );

        if let Some(lines) = result {
            return Some(
                lines
                    .into_iter()
                    .map(|line| line.to_string())
                    .collect::<Vec<_>>()
                    .join(""),
            );
        }

        let named_type_map = tyml.tyml().named_type_map();

        let name_id = named_type_map
            .use_link_map
            .iter()
            .find(|(_, ranges)| {
                ranges
                    .iter()
                    .any(|range| range.to_inclusive().contains(&byte_position))
            })
            .map(|(&name_id, _)| name_id);

        let Some(name_id) = name_id else { return None };

        let type_tree = named_type_map.get_type(name_id).unwrap();

        let documents = match type_tree {
            NamedTypeTree::Struct { tree } => &tree.documents(),
            NamedTypeTree::Enum {
                elements: _,
                documents,
            } => documents,
        };

        if documents.is_empty() {
            None
        } else {
            Some(
                documents
                    .iter()
                    .map(|line| line.to_string())
                    .collect::<Vec<_>>()
                    .join(""),
            )
        }
    }
}

fn create_hover_code_block(code: &str) -> String {
    // remove '///' documents
    static DOCUMENTS_REGEX: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"(^|\n|\r|\r\n)[ 　\t]*(///|###).*").unwrap());
    static INDENT_REGEX: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"^(\n|\r|\r\n| |　|\t)+").unwrap());

    let code = DOCUMENTS_REGEX.replace_all(code, "");
    let code = INDENT_REGEX.replace_all(code.as_ref(), "");

    static FOUND_INDENT_REGEX: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"(\n|\r|\r\n)([ \t]*)\}").unwrap());

    let indent = FOUND_INDENT_REGEX
        .captures_iter(code.as_ref())
        .map(|captured| captured[2].chars().count())
        .min()
        .unwrap_or(0);

    let indent = " ".repeat(indent);

    format!("```tyml\n{}{}\n```\n---\n", indent, code)
}

mod on_type_tag {
    use std::mem::swap;

    use tower_lsp::lsp_types::CompletionItemKind;
    use tyml::tyml_parser::ast::{AST, Define, Defines, ElementInlineType, TypeDefine};

    use super::ToInclusive;

    pub fn is_position_on_type_tag_for_defines<'input>(
        ast: &Defines<'input, '_>,
        position: usize,
        names: &mut Vec<(&'input str, CompletionItemKind)>,
        completions: &mut Vec<(&'input str, CompletionItemKind)>,
    ) -> bool {
        if !ast.span.to_inclusive().contains(&position) {
            return false;
        }

        // collect type names, first
        for define in ast.defines.iter() {
            if let Define::Type(type_define) = define {
                let (name, kind) = match type_define {
                    TypeDefine::Struct(struct_define) => {
                        (struct_define.name.value, CompletionItemKind::STRUCT)
                    }
                    TypeDefine::Enum(enum_define) => {
                        (enum_define.name.value, CompletionItemKind::ENUM)
                    }
                };
                names.push((name, kind));
            }
        }

        for define in ast.defines.iter() {
            if is_position_on_type_tag_for_define(define, position, names, completions) {
                return true;
            }
        }

        false
    }

    fn is_position_on_type_tag_for_define<'input>(
        ast: &Define<'input, '_>,
        position: usize,
        names: &mut Vec<(&'input str, CompletionItemKind)>,
        completions: &mut Vec<(&'input str, CompletionItemKind)>,
    ) -> bool {
        if !ast.span().to_inclusive().contains(&position) {
            return false;
        }

        match ast {
            Define::Element(element_define) => {
                if let Some(_) = &element_define.ty {
                    swap(names, completions);
                    true
                } else if let Some(inline_type) = &element_define.inline_type {
                    is_position_on_type_tag_for_element_inline_type(
                        inline_type,
                        position,
                        names,
                        completions,
                    )
                } else {
                    swap(names, completions);
                    true
                }
            }
            Define::Type(type_define) => match type_define {
                TypeDefine::Struct(struct_define) => is_position_on_type_tag_for_defines(
                    &struct_define.defines,
                    position,
                    names,
                    completions,
                ),
                TypeDefine::Enum(_) => false,
            },
        }
    }

    fn is_position_on_type_tag_for_element_inline_type<'input>(
        ast: &ElementInlineType<'input, '_>,
        position: usize,
        names: &mut Vec<(&'input str, CompletionItemKind)>,
        completions: &mut Vec<(&'input str, CompletionItemKind)>,
    ) -> bool {
        if !ast.span.to_inclusive().contains(&position) {
            return false;
        }

        is_position_on_type_tag_for_defines(&ast.defines, position, names, completions)
    }
}

mod tyml_semantic_tokens {
    use std::{collections::BTreeMap, ops::Range};

    use tower_lsp::lsp_types::SemanticTokenType;
    use tyml::tyml_parser::ast::{
        AST, Define, Defines, ElementType, OrType, TypeDefine, ValueLiteral, either::Either,
    };

    pub fn collect_tokens_for_defines(
        ast: &Defines,
        tokens: &mut BTreeMap<usize, (SemanticTokenType, Range<usize>)>,
    ) {
        for define in ast.defines.iter() {
            match define {
                Define::Element(element_define) => {
                    let span = element_define.documents.span.clone();
                    tokens.insert(span.start, (SemanticTokenType::COMMENT, span));

                    let span = element_define.node.span();
                    tokens.insert(span.start, (SemanticTokenType::PROPERTY, span));

                    if let Some(ty) = &element_define.ty {
                        collect_tokens_for_element_type(ty, tokens);
                    }
                    if let Some(inline_type) = &element_define.inline_type {
                        collect_tokens_for_defines(&inline_type.defines, tokens);
                    }
                    if let Some(default_value) = &element_define.default {
                        let token = match &default_value.value {
                            ValueLiteral::String(literal) => {
                                (SemanticTokenType::STRING, literal.span.clone())
                            }
                            ValueLiteral::Float(literal) => {
                                (SemanticTokenType::NUMBER, literal.span())
                            }
                            ValueLiteral::Binary(literal) => {
                                (SemanticTokenType::NUMBER, literal.span())
                            }
                            ValueLiteral::Null(literal) => {
                                (SemanticTokenType::KEYWORD, literal.span.clone())
                            }
                        };
                        tokens.insert(token.1.start, token);
                    }
                }
                Define::Type(type_define) => {
                    collect_tokens_for_type_define(type_define, tokens);
                }
            }
        }
    }

    pub fn collect_tokens_for_element_type(
        ast: &ElementType,
        tokens: &mut BTreeMap<usize, (SemanticTokenType, Range<usize>)>,
    ) {
        collect_tokens_for_or_type(&ast.type_info, tokens);
    }

    pub fn collect_tokens_for_or_type(
        ast: &OrType,
        tokens: &mut BTreeMap<usize, (SemanticTokenType, Range<usize>)>,
    ) {
        for ty in ast.or_types.iter() {
            match &ty.ty {
                Either::Left(named_type) => {
                    let span = named_type.name.span.clone();
                    tokens.insert(span.start, (SemanticTokenType::TYPE, span));
                }
                Either::Right(array_type) => {
                    collect_tokens_for_or_type(&array_type.base, tokens);
                }
            }
        }
    }

    pub fn collect_tokens_for_type_define(
        ast: &TypeDefine,
        tokens: &mut BTreeMap<usize, (SemanticTokenType, Range<usize>)>,
    ) {
        match ast {
            TypeDefine::Struct(struct_define) => {
                let span = struct_define.documents.span.clone();
                tokens.insert(span.start, (SemanticTokenType::COMMENT, span));

                let span = struct_define.name.span.clone();
                tokens.insert(span.start, (SemanticTokenType::TYPE, span));

                let span = struct_define.keyword_span.clone();
                tokens.insert(span.start, (SemanticTokenType::KEYWORD, span));

                collect_tokens_for_defines(&struct_define.defines, tokens);
            }
            TypeDefine::Enum(enum_define) => {
                let span = enum_define.documents.span.clone();
                tokens.insert(span.start, (SemanticTokenType::COMMENT, span));

                let span = enum_define.name.span.clone();
                tokens.insert(span.start, (SemanticTokenType::TYPE, span));

                let span = enum_define.keyword_span.clone();
                tokens.insert(span.start, (SemanticTokenType::KEYWORD, span));

                for element in enum_define.elements.iter() {
                    let span = element.documents.span.clone();
                    tokens.insert(span.start, (SemanticTokenType::COMMENT, span));

                    let span = element.literal.span.clone();
                    tokens.insert(span.start, (SemanticTokenType::STRING, span));
                }
            }
        }
    }
}

mod tyml_documents_from_ast {
    use tyml::tyml_parser::ast::{AST, Define, Defines, TypeDefine};

    use super::ToInclusive;

    pub fn get_documents_from_defines<'input>(
        defines: &Defines<'input, '_>,
        position: usize,
        result: &mut Option<Vec<&'input str>>,
    ) {
        for define in defines.defines.iter() {
            match define {
                Define::Element(element_define) => {
                    if element_define
                        .node
                        .span()
                        .to_inclusive()
                        .contains(&position)
                    {
                        *result = Some(element_define.documents.lines.iter().cloned().collect());
                        return;
                    }

                    if let Some(inline_type) = &element_define.inline_type {
                        get_documents_from_defines(&inline_type.defines, position, result);
                    }
                }
                Define::Type(type_define) => match type_define {
                    TypeDefine::Struct(struct_define) => {
                        get_documents_from_defines(&struct_define.defines, position, result);
                    }
                    TypeDefine::Enum(enum_define) => {
                        for element in enum_define.elements.iter() {
                            if element.literal.span.to_inclusive().contains(&position) {
                                *result = Some(element.documents.lines.iter().cloned().collect());
                                return;
                            }
                        }
                    }
                },
            }
        }
    }
}

#[extension_fn(SourceCodeSpan)]
fn to_lsp_span(&self, code: &str) -> LSPRange {
    match self {
        SourceCodeSpan::UTF8Byte(range) => LSPRange::new(
            to_line_column(code, range.start),
            to_line_column(code, range.end),
        ),
        SourceCodeSpan::UnicodeCharacter(range) => LSPRange::new(
            to_line_column(code, character_to_byte(code, range.start)),
            to_line_column(code, character_to_byte(code, range.end)),
        ),
    }
}

#[extension_fn(SourceCodeSpan)]
fn to_lsp_semantic_token(&self, code: &str) -> impl Iterator<Item = (Position, usize)> {
    let range = self.to_byte_span(code);

    LINE_REGEX
        .find_iter(&code[range.clone()])
        .map(move |matched| {
            (
                to_line_column(code, range.start + matched.start()),
                matched.as_str().chars().count(),
            )
        })
}

static LINE_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"[^\n\r]*(\n|\r|\r\n|$)").unwrap());

fn to_line_column(code: &str, byte: usize) -> Position {
    let mut last_line_index = 0;
    let mut last_line = "";
    for (line, line_matched) in LINE_REGEX.find_iter(code).enumerate() {
        if (line_matched.start()..line_matched.end()).contains(&byte) {
            let column = code[line_matched.start()..byte].chars().count();

            return Position::new(line as _, column as _);
        }

        last_line_index = line;
        last_line = &code[line_matched.start()..line_matched.end()];
    }

    Position::new(
        last_line_index as _,
        last_line
            .chars()
            .filter(|&char| char != '\n' && char != '\r')
            .count() as _,
    )
}

pub trait ToBytePosition {
    fn to_byte_position(&self, code: &str) -> usize;
}

impl ToBytePosition for Position {
    fn to_byte_position(&self, code: &str) -> usize {
        for (line, line_matched) in LINE_REGEX.find_iter(code).enumerate() {
            if line as u32 == self.line {
                let column_byte = line_matched
                    .as_str()
                    .char_indices()
                    .nth(self.character as _)
                    .map(|(position, _)| position)
                    .unwrap_or(line_matched.len());

                return line_matched.start() + column_byte;
            }
        }

        code.len()
    }
}

fn character_to_byte(code: &str, character: usize) -> usize {
    code.char_indices()
        .nth(character)
        .map(|(position, _)| position)
        .unwrap_or(code.len())
}

#[derive(Debug, Clone)]
pub struct Completion {
    pub kind: CompletionKind,
    pub documents: String,
    pub span: Range<usize>,
    pub name: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompletionKind {
    EnumValue,
    SectionName,
    Section,
}

impl CompletionKind {
    pub fn to_completion_item_kind(&self) -> CompletionItemKind {
        match self {
            CompletionKind::EnumValue => CompletionItemKind::ENUM_MEMBER,
            CompletionKind::SectionName => CompletionItemKind::FIELD,
            CompletionKind::Section => CompletionItemKind::STRUCT,
        }
    }
}

#[extension_fn(<'a> ValueTypeChecker<'a, 'a, 'a, 'a, 'a, 'a>)]
pub fn provide_completion(
    &self,
    code: &str,
    byte_position: usize,
    completions: &mut Vec<Completion>,
) {
    if let Some(MergedValueTree::Section {
        elements,
        name_spans: _,
        define_spans: _,
    }) = &self.merged_value_tree
    {
        match elements.get("root") {
            Some(element) => self.provide_completion_recursive_for_type_tree(
                &self.type_tree,
                element,
                code,
                byte_position,
                completions,
            ),
            None => self.provide_completion_recursive_for_type_tree(
                &self.type_tree,
                self.merged_value_tree.as_ref().unwrap(),
                code,
                byte_position,
                completions,
            ),
        }
    }
}

#[extension_fn(<'a> ValueTypeChecker<'a, 'a, 'a, 'a, 'a, 'a>)]
fn provide_completion_recursive_for_type(
    &self,
    ty: &Type,
    merged_value_tree: &MergedValueTree,
    code: &str,
    byte_position: usize,
    completions: &mut Vec<Completion>,
) {
    match ty {
        Type::Named(name_id) => match self.named_type_map.get_type(*name_id).unwrap() {
            NamedTypeTree::Struct { tree } => {
                return self.provide_completion_recursive_for_type_tree(
                    tree,
                    merged_value_tree,
                    code,
                    byte_position,
                    completions,
                );
            }
            NamedTypeTree::Enum {
                elements,
                documents: _,
            } => {
                completions.extend(elements.iter().map(|(element, documents)| {
                    Completion {
                        kind: CompletionKind::EnumValue,
                        documents: documents
                            .iter()
                            .map(|line| line.to_string())
                            .collect::<Vec<_>>()
                            .join(""),
                        span: element.span.clone(),
                        name: element.value.to_string(),
                    }
                }));
            }
        },
        Type::Or(types) => {
            for ty in types.iter() {
                self.provide_completion_recursive_for_type(
                    ty,
                    merged_value_tree,
                    code,
                    byte_position,
                    completions,
                );
            }
        }
        Type::Optional(ty) => {
            self.provide_completion_recursive_for_type(
                ty,
                merged_value_tree,
                code,
                byte_position,
                completions,
            );
        }
        Type::Array(ty) => {
            self.provide_completion_recursive_for_type(
                ty,
                merged_value_tree,
                code,
                byte_position,
                completions,
            );
        }
        _ => {}
    }
}

#[extension_fn(Range<usize>)]
fn to_inclusive(&self) -> RangeInclusive<usize> {
    self.start..=self.end
}

#[extension_fn(<'a> ValueTypeChecker<'a, 'a, 'a, 'a, 'a, 'a>)]
fn provide_completion_recursive_for_type_tree(
    &self,
    type_tree: &TypeTree,
    merged_value_tree: &MergedValueTree,
    code: &str,
    byte_position: usize,
    completions: &mut Vec<Completion>,
) {
    match type_tree {
        TypeTree::Node {
            node,
            any_node,
            documents: _,
            span: _,
        } => {
            if let MergedValueTree::Array {
                elements,
                key_span: _,
                span: _,
            } = merged_value_tree
            {
                for element in elements.iter() {
                    self.provide_completion_recursive_for_type_tree(
                        type_tree,
                        element,
                        code,
                        byte_position,
                        completions,
                    );
                }
            }

            let MergedValueTree::Section {
                elements,
                name_spans: _,
                define_spans,
            } = merged_value_tree
            else {
                return;
            };

            if !define_spans.iter().any(|span| {
                span.to_byte_span(code)
                    .to_inclusive()
                    .contains(&byte_position)
            }) {
                return;
            }

            for (element_name, element) in elements.iter() {
                let Some(type_tree) = node
                    .get(element_name.as_ref())
                    .or(any_node.as_ref().map(|boxed| boxed.as_ref()))
                else {
                    continue;
                };

                match element {
                    MergedValueTree::Section {
                        elements: _,
                        name_spans: _,
                        define_spans,
                    } => {
                        let cursor_in_spans = define_spans.iter().any(|span| {
                            span.to_byte_span(code)
                                .to_inclusive()
                                .contains(&byte_position)
                        });

                        if cursor_in_spans {
                            return self.provide_completion_recursive_for_type_tree(
                                type_tree,
                                element,
                                code,
                                byte_position,
                                completions,
                            );
                        }
                    }
                    MergedValueTree::Array {
                        elements,
                        key_span: _,
                        span: _,
                    } => {
                        for element in elements.iter() {
                            if !element.spans().any(|span| {
                                span.to_byte_span(code)
                                    .to_inclusive()
                                    .contains(&byte_position)
                            }) {
                                continue;
                            }

                            self.provide_completion_recursive_for_type_tree(
                                type_tree,
                                element,
                                code,
                                byte_position,
                                completions,
                            );
                        }
                    }
                    MergedValueTree::Value {
                        value: _,
                        key_span,
                        span,
                    } => {
                        if let TypeTree::Node {
                            node,
                            any_node: _,
                            documents: _,
                            span: _,
                        } = type_tree
                        {
                            if key_span
                                .to_byte_span(code)
                                .to_inclusive()
                                .contains(&byte_position)
                            {
                                // pattern of 'test.'
                                for (element_name, element) in node.iter() {
                                    completions.push(Completion {
                                        kind: CompletionKind::SectionName,
                                        documents: element
                                            .documents()
                                            .iter()
                                            .map(|line| line.to_string())
                                            .collect::<Vec<_>>()
                                            .join(""),
                                        span: element.span(),
                                        name: element_name.to_string(),
                                    });
                                }
                                return;
                            }
                        }

                        if span
                            .to_byte_span(code)
                            .to_inclusive()
                            .contains(&byte_position)
                        {
                            return self.provide_completion_recursive_for_type_tree(
                                type_tree,
                                element,
                                code,
                                byte_position,
                                completions,
                            );
                        }
                    }
                }
            }

            for (element_name, element) in node.iter() {
                completions.push(Completion {
                    kind: CompletionKind::SectionName,
                    documents: element
                        .documents()
                        .iter()
                        .map(|line| line.to_string())
                        .collect::<Vec<_>>()
                        .join(""),
                    span: element.span(),
                    name: element_name.to_string(),
                });
            }
        }
        TypeTree::Leaf {
            ty,
            documents: _,
            span: _,
        } => {
            self.provide_completion_recursive_for_type(
                ty,
                merged_value_tree,
                code,
                byte_position,
                completions,
            );
        }
    }
}

#[extension_fn(<'a> ValueTypeChecker<'a, 'a, 'a, 'a, 'a, 'a>)]
fn goto_define_and_documents(
    &self,
    position: usize,
    code: &str,
) -> Vec<(Range<usize>, &'a [&'a str])> {
    let mut result = Vec::new();

    if let Some(merged_value_tree) = &self.merged_value_tree {
        if let MergedValueTree::Section {
            elements,
            name_spans: _,
            define_spans: _,
        } = merged_value_tree
        {
            if let Some(root_tree) = elements.get("root") {
                goto_define_and_documents_recursive(
                    &self.type_tree,
                    &self.named_type_map,
                    root_tree,
                    position,
                    code,
                    &mut result,
                    true,
                );
            }
        }
    }

    result
}

fn goto_define_and_documents_recursive<'a>(
    type_tree: &'a TypeTree<'a, 'a>,
    named_type_map: &'a NamedTypeMap<'a, 'a>,
    merged_value_tree: &MergedValueTree,
    position: usize,
    code: &str,
    result: &mut Vec<(Range<usize>, &'a [&'a str])>,
    is_root: bool,
) {
    match type_tree {
        TypeTree::Node {
            node,
            any_node,
            documents,
            span,
        } => {
            let MergedValueTree::Section {
                elements,
                name_spans,
                define_spans,
            } = merged_value_tree
            else {
                return;
            };

            if !is_root {
                if name_spans
                    .iter()
                    .any(|span| span.to_byte_span(code).to_inclusive().contains(&position))
                {
                    result.push((span.clone(), documents.as_slice()));
                    return;
                }
            }

            if !define_spans
                .iter()
                .any(|span| span.to_byte_span(code).to_inclusive().contains(&position))
            {
                return;
            }

            for (element_name, element) in elements.iter() {
                let Some(type_tree) = node.get(element_name.as_ref()).or(any_node.as_deref())
                else {
                    continue;
                };

                goto_define_and_documents_recursive(
                    type_tree,
                    named_type_map,
                    element,
                    position,
                    code,
                    result,
                    false,
                );
            }
        }
        TypeTree::Leaf {
            ty,
            documents,
            span: define_span,
        } => {
            match merged_value_tree {
                MergedValueTree::Section {
                    elements,
                    name_spans,
                    define_spans,
                } => {
                    if !is_root {
                        if name_spans
                            .iter()
                            .any(|span| span.to_byte_span(code).to_inclusive().contains(&position))
                        {
                            result.push((define_span.clone(), documents.as_slice()));
                            return;
                        }
                    }

                    if !define_spans
                        .iter()
                        .any(|span| span.to_byte_span(code).to_inclusive().contains(&position))
                    {
                        return;
                    }

                    let mut trees = Vec::new();
                    collect_type_tree(ty, named_type_map, &mut trees);

                    for tree in trees {
                        let TypeTree::Node {
                            node,
                            any_node,
                            documents: _,
                            span: _,
                        } = tree
                        else {
                            continue;
                        };

                        for (element_name, element) in elements.iter() {
                            let Some(type_tree) =
                                node.get(element_name.as_ref()).or(any_node.as_deref())
                            else {
                                continue;
                            };

                            goto_define_and_documents_recursive(
                                type_tree,
                                named_type_map,
                                element,
                                position,
                                code,
                                result,
                                false,
                            );
                        }
                    }
                }
                MergedValueTree::Array {
                    elements,
                    key_span,
                    span: _,
                } => {
                    if key_span
                        .to_byte_span(code)
                        .to_inclusive()
                        .contains(&position)
                    {
                        result.push((define_span.clone(), documents.as_slice()));
                        return;
                    }

                    for element in elements.iter() {
                        if let MergedValueTree::Section {
                            elements: _,
                            name_spans,
                            define_spans: _,
                        } = element
                        {
                            if name_spans.iter().any(|span| {
                                span.to_byte_span(code).to_inclusive().contains(&position)
                            }) {
                                result.push((define_span.clone(), documents));
                                return;
                            }
                        }
                    }

                    for element in elements.iter() {
                        if let MergedValueTree::Value {
                            value,
                            key_span: _,
                            span,
                        } = element
                        {
                            if !span.to_byte_span(code).to_inclusive().contains(&position) {
                                continue;
                            }

                            // goto enum define
                            let ValidateValue::String(value) = value else {
                                continue;
                            };

                            return find_enum_define_and_documents(
                                ty,
                                named_type_map,
                                value.as_ref(),
                                result,
                            );
                        }
                    }

                    let TypeTree::Leaf {
                        ty,
                        documents: _,
                        span: _,
                    } = type_tree
                    else {
                        return;
                    };

                    let mut trees = Vec::new();
                    collect_type_tree(&ty, named_type_map, &mut trees);

                    for tree in trees {
                        for element in elements.iter() {
                            goto_define_and_documents_recursive(
                                tree,
                                named_type_map,
                                element,
                                position,
                                code,
                                result,
                                false,
                            );
                        }
                    }
                }
                MergedValueTree::Value {
                    value,
                    key_span,
                    span,
                } => {
                    if key_span
                        .to_byte_span(code)
                        .to_inclusive()
                        .contains(&position)
                    {
                        result.push((define_span.clone(), documents.as_slice()));
                        return;
                    }

                    // TODO : better check logic?
                    if !(key_span.to_byte_span(code).start..span.to_byte_span(code).end)
                        .to_inclusive()
                        .contains(&position)
                    {
                        return;
                    }

                    // goto enum define
                    let ValidateValue::String(value) = value else {
                        return;
                    };

                    find_enum_define_and_documents(ty, named_type_map, value.as_ref(), result);
                }
            }
        }
    }
}

fn collect_type_tree<'a>(
    ty: &Type<'a>,
    named_type_map: &'a NamedTypeMap<'a, 'a>,
    trees: &mut Vec<&'a TypeTree<'a, 'a>>,
) {
    match ty {
        Type::Named(name_id) => {
            if let NamedTypeTree::Struct { tree } = named_type_map.get_type(*name_id).unwrap() {
                trees.push(tree);
            }
        }
        Type::Or(or_types) => {
            for ty in or_types.iter() {
                collect_type_tree(ty, named_type_map, trees);
            }
        }
        Type::Array(base_type) => collect_type_tree(&base_type, named_type_map, trees),
        Type::Optional(ty) => collect_type_tree(&ty, named_type_map, trees),
        _ => {}
    }
}

fn find_enum_define_and_documents<'a>(
    ty: &Type,
    named_type_map: &'a NamedTypeMap<'a, 'a>,
    value: &str,
    result: &mut Vec<(Range<usize>, &'a [&'a str])>,
) {
    match ty {
        Type::Named(name_id) => {
            if let NamedTypeTree::Enum {
                elements,
                documents: _,
            } = named_type_map.get_type(*name_id).unwrap()
            {
                for (element, documents) in elements.iter() {
                    if element.value == value {
                        result.push((element.span.clone(), documents.as_slice()));
                    }
                }
            }
        }
        Type::Or(types) => {
            for ty in types.iter() {
                find_enum_define_and_documents(ty, named_type_map, value, result);
            }
        }
        Type::Array(base) => find_enum_define_and_documents(base, named_type_map, value, result),
        Type::Optional(base) => find_enum_define_and_documents(base, named_type_map, value, result),
        _ => return,
    }
}
