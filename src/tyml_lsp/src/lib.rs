pub mod language_server;

use std::collections::HashMap;
use std::sync::{Arc, RwLock};

use language_server::GeneratedLanguageServer;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use tyml::tyml_diagnostic::message::Lang;
use tyml::Tyml;

#[derive(Debug)]
pub struct LSPBackend {
    pub client: Client,
    pub tyml_language_servers: RwLock<HashMap<Url, Arc<GeneratedLanguageServer>>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for LSPBackend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
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
        let server = self
            .tyml_language_servers
            .write()
            .unwrap()
            .entry(params.text_document.uri.clone())
            .or_insert_with(|| {
                Arc::new(GeneratedLanguageServer::new(
                    params.text_document.uri.clone(),
                    Lang::ja_JP,
                ))
            })
            .clone();

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
            let server = self
                .tyml_language_servers
                .write()
                .unwrap()
                .entry(params.text_document.uri.clone())
                .or_insert_with(|| {
                    Arc::new(GeneratedLanguageServer::new(
                        params.text_document.uri.clone(),
                        Lang::ja_JP,
                    ))
                })
                .clone();

            self.client
                .log_message(MessageType::LOG, "1")
                .await;

            Tyml::parse(change.text.clone());

            self.client
                .log_message(MessageType::LOG, "2")
                .await;

            let result = server.on_change(
                Arc::new(params.text_document.uri.to_string()),
                Arc::new(change.text),
            );

            self.client
                .log_message(MessageType::LOG, "3")
                .await;

            if result.is_err() {
                return;
            }

            server.publish_analyzed_info(&self.client).await;
        }
    }
}
