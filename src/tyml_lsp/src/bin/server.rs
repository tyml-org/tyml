use std::{collections::HashMap, sync::RwLock};

use tower_lsp::{LspService, Server};
use tyml_lsp::{LSPBackend, RUNTIME};

fn main() {
    RUNTIME.block_on(async {
        let stdin = tokio::io::stdin();
        let stdout = tokio::io::stdout();

        let (service, socket) = LspService::new(|client| LSPBackend {
            client,
            generated_language_servers: RwLock::new(HashMap::new()),
            tyml_language_servers: RwLock::new(HashMap::new()),
        });
        Server::new(stdin, stdout, socket).serve(service).await;
    });
}
