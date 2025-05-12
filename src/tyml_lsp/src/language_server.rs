use std::{
    collections::BTreeMap,
    fs::File,
    io::Read,
    ops::{Range, RangeInclusive},
    sync::{
        Arc, LazyLock, Mutex,
        atomic::{AtomicBool, Ordering},
    },
};

use extension_fn::extension_fn;
use regex::Regex;
use tower_lsp::{
    Client,
    lsp_types::{
        CompletionItem, CompletionItemKind, Diagnostic, DiagnosticSeverity, NumberOrString,
        Position, SemanticTokenType, Url,
    },
};
use tyml::{
    TymlContext, Validated,
    header::TymlHeader,
    tyml_diagnostic::{DiagnosticBuilder, message::get_text},
    tyml_generator::{registry::STYLE_REGISTRY, style::ASTTokenKind},
    tyml_source::{AsUtf8ByteRange, SourceCode, SourceCodeSpan, ToByteSpan},
    tyml_type::types::{NamedTypeMap, NamedTypeTree, Type, TypeTree},
    tyml_validate::validate::{MergedValueTree, ValueTypeChecker},
};

type LSPRange = tower_lsp::lsp_types::Range;

#[derive(Debug)]
pub struct GeneratedLanguageServer {
    pub url: Url,
    pub lang: &'static str,
    pub tyml: Mutex<Option<(TymlContext<Validated>, TymlHeader)>>,
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
        let header = TymlHeader::parse(&source_code).ok_or(())?;

        let file = header
            .tyml
            .as_ref()
            .map(|tyml| File::open(tyml).ok())
            .ok()
            .flatten()
            .filter(|file| {
                file.metadata()
                    .map(|metadata| metadata.is_file())
                    .unwrap_or(false)
            });
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

        let language = STYLE_REGISTRY.resolve("ini").unwrap();

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

        *self.tyml.lock().unwrap() = Some((tyml, header));

        Ok(())
    }

    pub async fn publish_analyzed_info(&self, client: &Client) {
        let (tyml, header) = self.tyml.lock().unwrap().clone().unwrap();

        if self.has_tyml_file_error.load(Ordering::Acquire) {
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
                            message: get_text("lsp.message.tyml_var_lookup_error", self.lang)
                                .replace("%0", &error.var_name),
                            ..Default::default()
                        }],
                        None,
                    )
                    .await;
            } else if let Err(error) = header.tyml {
                client
                    .publish_diagnostics(
                        self.url.clone(),
                        vec![Diagnostic {
                            range: header
                                .span
                                .as_utf8_byte_range()
                                .to_lsp_span(&tyml.ml_source_code().code),
                            severity: Some(DiagnosticSeverity::WARNING),
                            message: get_text("lsp.message.tyml_var_lookup_error", self.lang)
                                .replace("%0", &error.var_name),
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
                        "{}\n{}",
                        diagnostic.message.message(self.lang, false),
                        diagnostic.message.label(1, self.lang, false).unwrap(),
                    ),
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
                        message: format!(
                            "{}\n{}",
                            diagnostic.message.message(self.lang, false),
                            diagnostic.message.label(0, self.lang, false).unwrap()
                        ),
                        ..Default::default()
                    });
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

        let mut completions = Vec::new();

        let byte_position = position.to_byte_position(&tyml.0.ml_source_code().code);

        let non_whitespace_byte_position = tyml.0.ml_source_code().code[..byte_position]
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

        tyml.0.validator().provide_completion(
            &tyml.0.tyml_source.code,
            non_whitespace_byte_position,
            &mut completions,
        );

        Some(
            completions
                .iter()
                .map(|completion| CompletionItem {
                    label: completion.name.clone(),
                    kind: Some(completion.kind.to_completion_item_kind()),
                    ..Default::default()
                })
                .collect(),
        )
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
                to_line_column(code, character_to_byte(code, range.start)),
                to_line_column(code, character_to_byte(code, range.end)),
            ),
        }
    }
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
            NamedTypeTree::Enum { elements } => {
                completions.extend(elements.iter().map(|element| Completion {
                    kind: CompletionKind::EnumValue,
                    name: element.value.to_string(),
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
            span: _,
        } => {
            let MergedValueTree::Section {
                elements,
                name_spans: _,
                define_spans: _,
            } = merged_value_tree
            else {
                return;
            };

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
                    MergedValueTree::Array { elements: _, span } => {
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
                    MergedValueTree::Value { value: _, span } => {
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

            for (element_name, _) in node.iter() {
                if !elements.contains_key(*element_name) {
                    completions.push(Completion {
                        kind: CompletionKind::SectionName,
                        name: element_name.to_string(),
                    });
                }
            }
        }
        TypeTree::Leaf { ty, span: _ } => {
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
