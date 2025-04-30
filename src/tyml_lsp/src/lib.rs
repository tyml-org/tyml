pub mod language_server;

use language_server::GeneratedLanguageServer;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

#[derive(Debug)]
pub struct LSPBackend {
    pub client: Client,
    pub tyml_language_server: GeneratedLanguageServer,
}

#[tower_lsp::async_trait]
impl LanguageServer for LSPBackend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult::default())
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

