use regex::Regex;
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub enum Comment {
    Hash,
}

impl Comment {
    pub fn regex(&self) -> Regex {
        match self {
            Comment::Hash => Regex::new(r"#[^\n\r]+").unwrap(),
        }
    }
}
