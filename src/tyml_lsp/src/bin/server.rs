use std::{collections::HashMap, sync::RwLock};

use tower_lsp::{LspService, Server};
use tyml_lsp::LSPBackend;

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| LSPBackend {
        client,
        tyml_language_servers: RwLock::new(HashMap::new()),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
