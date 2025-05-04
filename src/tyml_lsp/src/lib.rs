pub mod language_server;

use std::collections::HashMap;
use std::sync::{Arc, RwLock};

use language_server::GeneratedLanguageServer;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use tyml::Tyml;
use tyml::tyml_diagnostic::message::Lang;

#[derive(Debug)]
pub struct LSPBackend {
    pub client: Client,
    pub tyml_language_servers: RwLock<HashMap<Url, Arc<GeneratedLanguageServer>>>,
}

impl LSPBackend {
    fn get_server(&self, url: Url) -> Arc<GeneratedLanguageServer> {
        self.tyml_language_servers
            .write()
            .unwrap()
            .entry(url.clone())
            .or_insert_with(|| Arc::new(GeneratedLanguageServer::new(url, Lang::ja_JP)))
            .clone()
    }
}

static TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::KEYWORD,
    SemanticTokenType::FUNCTION,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::STRING,
    SemanticTokenType::NUMBER,
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
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "TYML LSP server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let server = self.get_server(params.text_document.uri.clone());

        let result = server.on_change(
            Arc::new(params.text_document.uri.to_string()),
            Arc::new(params.text_document.text),
        );

        if result.is_err() {
            return;
        }

        server.publish_analyzed_info(&self.client).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        if let Some(change) = params.content_changes.into_iter().last() {
            let server = self.get_server(params.text_document.uri.clone());

            self.client.log_message(MessageType::LOG, "1").await;

            Tyml::parse(change.text.clone());

            self.client.log_message(MessageType::LOG, "2").await;

            let result = server.on_change(
                Arc::new(params.text_document.uri.to_string()),
                Arc::new(change.text),
            );

            self.client.log_message(MessageType::LOG, "3").await;

            if result.is_err() {
                return;
            }

            server.publish_analyzed_info(&self.client).await;
        }
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let server = self.get_server(params.text_document.uri.clone());

        let semantic_tokens = server.tokens.lock().unwrap().clone();

        if semantic_tokens.is_empty() {
            return Ok(None);
        }

        let mut tokens = Vec::new();
        let mut prev_line = 1;
        let mut prev_column = 0;
        for (kind, span) in semantic_tokens.iter() {
            if span.start.line != prev_line {
                prev_column = 0;
            }

            tokens.push(SemanticToken {
                delta_line: span.start.line - prev_line,
                delta_start: span.start.character - prev_column,
                length: span.end.character - span.start.character,
                token_type: TOKEN_TYPES
                    .iter()
                    .position(|token_type| token_type == kind)
                    .unwrap_or_default() as _,
                token_modifiers_bitset: 0,
            });

            prev_line = span.start.line;
            prev_column = span.start.character;
        }

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: tokens,
        })))
    }
}
