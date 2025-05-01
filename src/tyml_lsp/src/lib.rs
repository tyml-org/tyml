pub mod language_server;

use std::collections::HashMap;
use std::sync::{Arc, RwLock};

use language_server::GeneratedLanguageServer;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

#[derive(Debug)]
pub struct LSPBackend {
    pub client: Client,
    pub tyml_language_servers: RwLock<HashMap<Url, GeneratedLanguageServer>>,
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
        let mut servers = self.tyml_language_servers.write().unwrap();
        let server = servers
            .entry(params.text_document.uri.clone())
            .or_insert_with(|| GeneratedLanguageServer::new());

        server
            .on_change(
                Arc::new(params.text_document.uri.to_string()),
                Arc::new(params.text_document.text),
            )
            .unwrap();
    }

    async fn did_change(&self, p: DidChangeTextDocumentParams) {
        if let Some(change) = p.content_changes.into_iter().last() {}
    }
}
