use regex::Regex;
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub enum Comment {
    Line { start: String },
}

impl Comment {
    pub fn regex(&self) -> Regex {
        match self {
            Comment::Line { start } => Regex::new(format!(r"^{}[^\n\r]*", start).as_str()).unwrap(),
        }
    }
}
