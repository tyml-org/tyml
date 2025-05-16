use std::{
    collections::BTreeMap,
    fs::File,
    io::Read,
    ops::{Deref, Range, RangeInclusive},
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
    Parsed, TymlContext, Validated,
    header::TymlHeader,
    tyml_diagnostic::{DiagnosticBuilder, message::get_text},
    tyml_generator::{registry::STYLE_REGISTRY, style::ASTTokenKind},
    tyml_source::{AsUtf8ByteRange, SourceCode, SourceCodeSpan, ToByteSpan},
    tyml_type::types::{NamedTypeMap, NamedTypeTree, Type, TypeTree},
    tyml_validate::validate::{MergedValueTree, ValidateValue, ValueTypeChecker},
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
    pub tokens: Mutex<Arc<Vec<(SemanticTokenType, LSPRange)>>>,
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
            semantic_tokens.push((kind, span.as_utf8_byte_range().to_lsp_span(&source_code)));
        }
        *self.tokens.lock().unwrap() = Arc::new(semantic_tokens);

        *self.tyml.lock().unwrap() = Some((tyml, header));

        Ok(())
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

    pub fn goto_define(&self, position: Position) -> (String, Vec<LSPRange>) {
        let Some((tyml, header)) = self.tyml.lock().unwrap().as_ref().cloned() else {
            return (String::new(), Vec::new());
        };

        let byte_position = position.to_byte_position(&tyml.ml_source_code().code);

        let defines = tyml
            .validator()
            .goto_define(byte_position, &tyml.ml_source_code().code);

        let defines = defines
            .into_iter()
            .map(|range| {
                range
                    .as_utf8_byte_range()
                    .to_lsp_span(&tyml.ml_source_code().code)
            })
            .collect();

        (header.tyml.unwrap_or_default(), defines)
    }
}

#[derive(Debug)]
pub struct TymlLanguageServer {
    pub url: Url,
    pub lang: &'static str,
    pub tyml: Mutex<Option<TymlContext<Parsed>>>,
    pub tokens: Mutex<Arc<Vec<(SemanticTokenType, LSPRange)>>>,
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

            let tokens = tokens
                .into_values()
                .map(|(token_type, span)| {
                    (
                        token_type,
                        span.as_utf8_byte_range()
                            .to_lsp_span(&tyml.tyml_source.code),
                    )
                })
                .collect();
            *self.tokens.lock().unwrap() = Arc::new(tokens);
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
                let span = struct_define.name.span.clone();
                tokens.insert(span.start, (SemanticTokenType::TYPE, span));

                let span = struct_define.keyword_span.clone();
                tokens.insert(span.start, (SemanticTokenType::KEYWORD, span));

                collect_tokens_for_defines(&struct_define.defines, tokens);
            }
            TypeDefine::Enum(enum_define) => {
                let span = enum_define.name.span.clone();
                tokens.insert(span.start, (SemanticTokenType::TYPE, span));

                let span = enum_define.keyword_span.clone();
                tokens.insert(span.start, (SemanticTokenType::KEYWORD, span));

                for element in enum_define.elements.iter() {
                    let span = element.span.clone();
                    tokens.insert(span.start, (SemanticTokenType::STRING, span));
                }
            }
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
                    MergedValueTree::Array {
                        elements: _,
                        key_span: _,
                        span,
                    } => {
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
                    MergedValueTree::Value {
                        value: _,
                        key_span: _,
                        span,
                    } => {
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

#[extension_fn(<'a> ValueTypeChecker<'a, 'a, 'a, 'a, 'a, 'a>)]
fn goto_define(&self, position: usize, code: &str) -> Vec<Range<usize>> {
    let mut result = Vec::new();

    if let Some(merged_value_tree) = &self.merged_value_tree {
        goto_define_recursive(
            &self.type_tree,
            &self.named_type_map,
            merged_value_tree,
            position,
            code,
            &mut result,
        );
    }

    result
}

fn goto_define_recursive(
    type_tree: &TypeTree,
    named_type_map: &NamedTypeMap,
    merged_value_tree: &MergedValueTree,
    position: usize,
    code: &str,
    result: &mut Vec<Range<usize>>,
) {
    match type_tree {
        TypeTree::Node {
            node,
            any_node,
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

            if name_spans
                .iter()
                .any(|span| span.to_byte_span(code).to_inclusive().contains(&position))
            {
                result.push(span.clone());
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

                goto_define_recursive(type_tree, named_type_map, element, position, code, result);
            }
        }
        TypeTree::Leaf {
            ty,
            span: define_span,
        } => {
            if let MergedValueTree::Value {
                value,
                key_span,
                span,
            } = merged_value_tree
            {
                if !span.to_byte_span(code).to_inclusive().contains(&position) {
                    return;
                }

                if key_span
                    .to_byte_span(code)
                    .to_inclusive()
                    .contains(&position)
                {
                    result.push(define_span.clone());
                    return;
                }

                // goto enum define
                let ValidateValue::String(value) = value else {
                    return;
                };

                find_enum_define(ty, named_type_map, value.as_ref(), result);
            } else if let MergedValueTree::Array {
                elements,
                key_span,
                span,
            } = merged_value_tree
            {
                if !span.to_byte_span(code).to_inclusive().contains(&position) {
                    return;
                }

                if key_span
                    .to_byte_span(code)
                    .to_inclusive()
                    .contains(&position)
                {
                    result.push(define_span.clone());
                    return;
                }

                for element in elements.iter() {
                    goto_define_recursive(
                        type_tree,
                        named_type_map,
                        element,
                        position,
                        code,
                        result,
                    );
                }
            }
        }
    }
}

fn find_enum_define(
    ty: &Type,
    named_type_map: &NamedTypeMap,
    value: &str,
    result: &mut Vec<Range<usize>>,
) {
    match ty {
        Type::Named(name_id) => {
            if let NamedTypeTree::Enum { elements } = named_type_map.get_type(*name_id).unwrap() {
                for element in elements.iter() {
                    if element.value == value {
                        result.push(element.span.clone());
                    }
                }
            }
        }
        Type::Or(types) => {
            for ty in types.iter() {
                find_enum_define(ty, named_type_map, value, result);
            }
        }
        Type::Array(base) => find_enum_define(base, named_type_map, value, result),
        Type::Optional(base) => find_enum_define(base, named_type_map, value, result),
        _ => return,
    }
}
