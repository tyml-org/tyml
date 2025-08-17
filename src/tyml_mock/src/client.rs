use tyml::Tyml;

pub struct TymlMockClient {
    tyml: Tyml,
}

impl TymlMockClient {
    pub fn new(tyml: Tyml) -> Self {
        Self { tyml }
    }
}
