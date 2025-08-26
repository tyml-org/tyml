pub mod client;
pub mod error;
pub(crate) mod json;
pub mod server;

use tyml::Tyml;

use crate::{client::TymlMockClient, server::TymlMockServer};

pub struct TymlMock {
    pub server: TymlMockServer,
    pub client: TymlMockClient,
}

impl TymlMock {
    pub fn new(tyml: Tyml) -> Self {
        Self {
            server: TymlMockServer::new(tyml.clone()),
            client: TymlMockClient::new(tyml),
        }
    }
}

#[cfg(test)]
mod test {
    use std::{thread, time::Duration};

    use tokio::runtime::Runtime;
    use tyml::Tyml;

    use crate::{TymlMock, server::ServerSourceLocation};

    static SOURCE: &str = r#"
type Hello {
    id: int
    name: string
}

interface API {
    function hello()
}
"#;

    #[test]
    fn mock() {
        let tyml = Tyml::parse(SOURCE.to_string());

        let mock = TymlMock::new(tyml);

        let runtime = Runtime::new().unwrap();

        runtime.spawn(async move {
            mock.server
                .serve(ServerSourceLocation::AllInterfaces)
                .await
                .unwrap();
        });

        thread::sleep(Duration::from_millis(50));

        runtime.block_on(async {
            mock.client.send("api", "hello").await.unwrap();
        });
    }
}
