use std::{ops::Range, sync::Arc};

use extension_fn::extension_fn;

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

pub trait ToUnicodeCharacterRange {
    fn to_unicode_character_range(&self, source_code: &str) -> Range<usize>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SourceCodeSpan {
    /// 0 indexed, UTF-8 byte index
    UTF8Byte(Range<usize>),
    /// 0 indexed, Unicode character index
    UnicodeCharacter(Range<usize>),
}

impl Default for SourceCodeSpan {
    fn default() -> Self {
        SourceCodeSpan::UnicodeCharacter(0..0)
    }
}

#[extension_fn(Range<usize>)]
pub fn as_utf8_byte_range(&self) -> SourceCodeSpan {
    SourceCodeSpan::UTF8Byte(self.clone())
}

impl ToUnicodeCharacterRange for SourceCodeSpan {
    fn to_unicode_character_range(&self, source_code: &str) -> Range<usize> {
        match self {
            SourceCodeSpan::UTF8Byte(range) => {
                let start = source_code
                    .char_indices()
                    .map(|(position, _)| position)
                    .position(|position| position >= range.start)
                    .unwrap_or(source_code.len());
                let end = source_code
                    .char_indices()
                    .map(|(position, _)| position)
                    .position(|position| position >= range.end)
                    .unwrap_or(source_code.len());

                start..end
            }
            SourceCodeSpan::UnicodeCharacter(range) => range.clone(),
        }
    }
}

pub trait ToByteSpan {
    fn to_byte_span(&self, code: &str) -> Range<usize>;
}

impl ToByteSpan for SourceCodeSpan {
    fn to_byte_span(&self, code: &str) -> Range<usize> {
        match self {
            SourceCodeSpan::UTF8Byte(range) => range.clone(),
            SourceCodeSpan::UnicodeCharacter(range) => {
                character_to_byte(code, range.start)..character_to_byte(code, range.end)
            }
        }
    }
}

fn character_to_byte(code: &str, character: usize) -> usize {
    code.char_indices()
        .nth(character)
        .map(|(position, _)| position)
        .unwrap_or(code.len())
}
