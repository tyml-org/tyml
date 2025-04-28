use tyml::{Parsed, TymlContext};

#[derive(Debug)]
pub struct TymlLanguageServer {
    pub tyml: TymlContext<Parsed>,
}
