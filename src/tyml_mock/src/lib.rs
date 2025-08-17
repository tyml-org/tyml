pub mod client;
pub mod server;

use tyml::Tyml;

use crate::{client::TymlMockClient, server::TymlMockServer};

pub struct TymlMock {
    tyml: Tyml,
    pub server: TymlMockServer,
    pub client: TymlMockClient,
}

impl TymlMock {
    pub fn new(tyml: Tyml) -> Self {
        Self {
            tyml: tyml.clone(),
            server: TymlMockServer::new(tyml.clone()),
            client: TymlMockClient::new(tyml),
        }
    }
}

