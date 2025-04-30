use tower_lsp::{LspService, Server};
use tyml_lsp::{LSPBackend, language_server::GeneratedLanguageServer};

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| LSPBackend {
        client,
        tyml_language_server: GeneratedLanguageServer::new(),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
