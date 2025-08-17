use tyml::Tyml;

pub struct TymlMockServer {
    tyml: Tyml,
}

impl TymlMockServer {
    pub fn new(tyml: Tyml) -> Self {
        Self { tyml }
    }
}
