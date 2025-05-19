pub mod language_server;

use std::collections::HashMap;
use std::ops::Deref;
use std::sync::{Arc, LazyLock, RwLock};

use either::Either;
use language_server::{GeneratedLanguageServer, TymlLanguageServer};
use tokio::runtime::Runtime;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use tyml::tyml_diagnostic::message::Lang;

pub static RUNTIME: LazyLock<Runtime> = LazyLock::new(|| Runtime::new().unwrap());

#[derive(Debug)]
pub struct LSPBackend {
    pub client: Client,
    pub generated_language_servers: RwLock<HashMap<Url, Arc<GeneratedLanguageServer>>>,
    pub tyml_language_servers: RwLock<HashMap<Url, Arc<TymlLanguageServer>>>,
}

impl LSPBackend {
    fn get_server(
        &self,
        url: Url,
    ) -> Either<Arc<GeneratedLanguageServer>, Arc<TymlLanguageServer>> {
        if let Some("tyml") = url.as_str().split(".").last() {
            Either::Right(
                self.tyml_language_servers
                    .write()
                    .unwrap()
                    .entry(url.clone())
                    .or_insert_with(|| Arc::new(TymlLanguageServer::new(url, Lang::system())))
                    .clone(),
            )
        } else {
            Either::Left(
                self.generated_language_servers
                    .write()
                    .unwrap()
                    .entry(url.clone())
                    .or_insert_with(|| Arc::new(GeneratedLanguageServer::new(url, Lang::system())))
                    .clone(),
            )
        }
    }
}

static TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::KEYWORD,
    SemanticTokenType::FUNCTION,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::PROPERTY,
    SemanticTokenType::STRING,
    SemanticTokenType::NUMBER,
    SemanticTokenType::STRUCT,
    SemanticTokenType::TYPE,
    SemanticTokenType::COMMENT,
];

#[tower_lsp::async_trait]
impl LanguageServer for LSPBackend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: SemanticTokensLegend {
                                token_types: TOKEN_TYPES.to_vec(),
                                token_modifiers: vec![],
                            },
                            range: Some(false),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            ..Default::default()
                        },
                    ),
                ),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".to_string(), "[".to_string()]),
                    resolve_provider: Some(true),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    ..Default::default()
                }),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "TYML LSP server initialized!")
            .await;
        *CLIENT_COPY.write().unwrap() = Some(self.client.clone());
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let server = self.get_server(params.text_document.uri.clone());

        self.client
            .log_message(MessageType::LOG, "Starting analyze...")
            .await;

        match server {
            Either::Left(server) => {
                let result = server.on_change(
                    Arc::new(params.text_document.uri.to_string()),
                    Arc::new(params.text_document.text),
                );

                if result.is_err() {
                    return;
                }

                server.publish_diagnostics(&self.client).await;
            }
            Either::Right(server) => {
                server.on_change(
                    params.text_document.uri.to_string(),
                    params.text_document.text,
                );

                server.publish_diagnostics(&self.client).await;
            }
        }

        self.client.log_message(MessageType::LOG, "Analyzed!").await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        if let Some(change) = params.content_changes.into_iter().last() {
            let server = self.get_server(params.text_document.uri.clone());

            self.client
                .log_message(MessageType::LOG, "Starting analyze...")
                .await;

            match server {
                Either::Left(server) => {
                    let result = server.on_change(
                        Arc::new(params.text_document.uri.to_string()),
                        Arc::new(change.text),
                    );

                    if result.is_err() {
                        return;
                    }

                    server.publish_diagnostics(&self.client).await;
                }
                Either::Right(server) => {
                    server.on_change(params.text_document.uri.to_string(), change.text);

                    server.publish_diagnostics(&self.client).await;
                }
            }

            self.client.log_message(MessageType::LOG, "Analyzed!").await;
        }
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let server = self.get_server(params.text_document.uri.clone());

        let semantic_tokens = match server {
            Either::Left(server) => server.tokens.lock().unwrap().clone(),
            Either::Right(server) => server.tokens.lock().unwrap().clone(),
        };

        if semantic_tokens.is_empty() {
            return Ok(None);
        }

        let mut tokens = Vec::new();
        let mut prev_line = 0;
        let mut prev_column = 0;
        for (kind, (start, length)) in semantic_tokens.iter() {
            if start.line != prev_line {
                prev_column = 0;
            }

            tokens.push(SemanticToken {
                delta_line: start.line - prev_line,
                delta_start: start.character - prev_column,
                length: *length as _,
                token_type: TOKEN_TYPES
                    .iter()
                    .position(|token_type| token_type == kind)
                    .unwrap_or_default() as _,
                token_modifiers_bitset: 0,
            });

            prev_line = start.line;
            prev_column = start.character;
        }

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: tokens,
        })))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let server = self.get_server(params.text_document_position.text_document.uri);

        match server {
            Either::Left(server) => Ok(server
                .provide_completion(params.text_document_position.position)
                .map(|completions| CompletionResponse::Array(completions))),
            Either::Right(server) => Ok(server
                .provide_completion(params.text_document_position.position)
                .map(|completions| CompletionResponse::Array(completions))),
        }
    }

    async fn completion_resolve(&self, params: CompletionItem) -> Result<CompletionItem> {
        Ok(params)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let server = self.get_server(params.text_document_position_params.text_document.uri);

        match server {
            Either::Left(server) => {
                let (defined_url, defines) =
                    server.goto_define(params.text_document_position_params.position);

                let defined_url = Url::parse(format!("file://{}", defined_url).as_str()).unwrap();

                match defines.len() {
                    0 => Ok(None),
                    1 => Ok(Some(GotoDefinitionResponse::Scalar(Location {
                        uri: defined_url,
                        range: defines[0],
                    }))),
                    _ => Ok(Some(GotoDefinitionResponse::Array(
                        defines
                            .into_iter()
                            .map(|range| Location {
                                uri: defined_url.clone(),
                                range,
                            })
                            .collect(),
                    ))),
                }
            }
            Either::Right(server) => Ok(server
                .goto_define(params.text_document_position_params.position)
                .map(|range| {
                    GotoDefinitionResponse::Scalar(Location {
                        uri: server.url.clone(),
                        range,
                    })
                })),
        }
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let server = self.get_server(params.text_document_position.text_document.uri);

        match server {
            Either::Left(_) => Ok(None),
            Either::Right(server) => {
                let users = server.get_references(params.text_document_position.position);

                if users.is_empty() {
                    Ok(None)
                } else {
                    Ok(Some(
                        users
                            .into_iter()
                            .map(|range| Location {
                                uri: server.url.clone(),
                                range,
                            })
                            .collect(),
                    ))
                }
            }
        }
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let server = self.get_server(params.text_document_position.text_document.uri);

        match server {
            Either::Left(_) => Ok(None),
            Either::Right(server) => {
                let references = server.get_references(params.text_document_position.position);

                let mut changes = HashMap::new();
                changes.insert(
                    server.url.clone(),
                    references
                        .into_iter()
                        .map(|range| TextEdit {
                            range,
                            new_text: params.new_name.clone(),
                        })
                        .collect(),
                );

                Ok(Some(WorkspaceEdit {
                    changes: Some(changes),
                    document_changes: None,
                    change_annotations: None,
                }))
            }
        }
    }
}

static CLIENT_COPY: LazyLock<RwLock<Option<Client>>> = LazyLock::new(|| RwLock::new(None));

#[allow(unused)]
pub(crate) fn debug_log<T: ToString + Send + 'static>(log: T) {
    RUNTIME.spawn(async move {
        let client = {
            CLIENT_COPY
                .read()
                .as_ref()
                .unwrap()
                .deref()
                .clone()
                .unwrap()
        };
        client.log_message(MessageType::LOG, log.to_string()).await
    });
}
