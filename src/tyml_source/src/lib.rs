use std::sync::Arc;

#[derive(Debug, Clone)]
pub struct SourceCode {
    pub name: Arc<String>,
    pub code: Arc<String>,
}

impl SourceCode {
    pub fn new<N, C>(name: N, code: C) -> Self
    where
        N: Into<Arc<String>>,
        C: Into<Arc<String>>,
    {
        Self {
            name: name.into(),
            code: code.into(),
        }
    }
}
