use std::{mem::swap, ops::Range, sync::Arc};

use either::Either;
use extension_fn::extension_fn;
use regex::Regex;

pub enum GeneratorTokenizer {
    Keyword(String),
    Regex(Regex),
    Function(Box<dyn Fn(&str) -> usize>),
}

impl GeneratorTokenizer {
    pub fn regex(regex_str: &str) -> Self {
        Self::Regex(Regex::new(format!("^{}", regex_str).as_str()).unwrap())
    }

    pub fn tokenize(&self, input: &str) -> usize {
        match self {
            GeneratorTokenizer::Keyword(keyword) => {
                if input.starts_with(keyword) {
                    keyword.len()
                } else {
                    0
                }
            }
            GeneratorTokenizer::Regex(regex) => {
                regex.find(input).map(|matched| matched.end()).unwrap_or(0)
            }
            GeneratorTokenizer::Function(function) => function(input),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GeneratorTokenKind(usize);

#[allow(non_upper_case_globals)]
impl GeneratorTokenKind {
    pub const None: Self = GeneratorTokenKind(0);
    pub const Whitespace: Self = GeneratorTokenKind(1);
    pub const UnexpectedCharacter: Self = GeneratorTokenKind(usize::MAX);
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GeneratorToken<'input> {
    pub kind: GeneratorTokenKind,
    pub text: &'input str,
    pub span: Range<usize>,
}

#[extension_fn(Option<GeneratorToken<'_>>)]
pub fn get_kind(&self) -> GeneratorTokenKind {
    self.as_ref()
        .map(|token| token.kind)
        .unwrap_or(GeneratorTokenKind::None)
}

pub struct TokenizerRegistry {
    registry: Either<Vec<GeneratorTokenizer>, Arc<Vec<GeneratorTokenizer>>>,
}

impl TokenizerRegistry {
    pub fn new() -> Self {
        Self {
            registry: Either::Left(vec![
                GeneratorTokenizer::Function(Box::new(|_| 0)), // None
                GeneratorTokenizer::regex(r"[ 　\t]+"),        // Whitespace
            ]),
        }
    }

    pub fn register(&mut self, tokenizer: GeneratorTokenizer) -> GeneratorTokenKind {
        match &mut self.registry {
            Either::Left(registry) => {
                registry.push(tokenizer);

                GeneratorTokenKind(registry.len() - 1)
            }
            Either::Right(_) => panic!("Already freezed!"),
        }
    }

    pub fn freeze(&mut self) {
        if let Either::Left(registry) = &mut self.registry {
            let mut registry_temp = Vec::new();
            swap(registry, &mut registry_temp);

            self.registry = Either::Right(Arc::new(registry_temp));
        }
    }

    pub fn registry(&self) -> Arc<Vec<GeneratorTokenizer>> {
        match &self.registry {
            Either::Right(registry) => registry.clone(),
            _ => panic!("Dereference before freeze!"),
        }
    }
}

impl Clone for TokenizerRegistry {
    fn clone(&self) -> Self {
        match &self.registry {
            Either::Right(registry) => Self {
                registry: Either::Right(registry.clone()),
            },
            _ => panic!("Clone before freeze!"),
        }
    }
}

pub struct GeneratorLexer<'input> {
    source: &'input str,
    tokenizers: Arc<Vec<GeneratorTokenizer>>,
    current_byte_position: usize,
    current_token_cache: Option<GeneratorToken<'input>>,
    pub ignore_whitespace: bool,
}

impl<'input> GeneratorLexer<'input> {
    pub fn new(source: &'input str, registry: &TokenizerRegistry) -> Self {
        Self {
            source,
            tokenizers: registry.registry(),
            current_byte_position: 0,
            current_token_cache: None,
            ignore_whitespace: true,
        }
    }

    pub fn current(&mut self) -> Option<GeneratorToken<'input>> {
        let anchor = self.cast_anchor();

        // move to next temporarily
        self.current_token_cache = self.next();

        // back to anchor position
        self.current_byte_position = anchor.byte_position;

        self.current_token_cache.clone()
    }

    pub fn cast_anchor(&self) -> GeneratorAnchor {
        GeneratorAnchor {
            byte_position: self.current_byte_position,
        }
    }

    pub fn skip(&mut self, kind: &[GeneratorTokenKind]) {
        loop {
            if kind.contains(&self.current().get_kind()) {
                self.next();
                continue;
            } else {
                return;
            }
        }
    }

    pub fn back_to_anchor(&mut self, anchor: GeneratorAnchor) {
        self.current_byte_position = anchor.byte_position;
        self.current_token_cache = None;
    }
}

impl<'input> Iterator for GeneratorLexer<'input> {
    type Item = GeneratorToken<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        // take cache
        if let Some(token) = self.current_token_cache.take() {
            self.current_byte_position = token.span.end;
            return Some(token);
        }

        loop {
            if self.current_byte_position == self.source.len() {
                return None;
            }

            let current_input = &self.source[self.current_byte_position..self.source.len()];

            let mut current_max_length = 0;
            let mut current_token_kind = GeneratorTokenKind::Whitespace;

            for (index, tokenizer) in self.tokenizers.iter().enumerate() {
                let byte_length = tokenizer.tokenize(current_input);

                if byte_length > current_max_length {
                    current_max_length = byte_length;
                    current_token_kind = GeneratorTokenKind(index);
                }
            }

            let start_position = self.current_byte_position;

            let token = if current_max_length == 0 {
                let char_length = self.source[start_position..]
                    .chars()
                    .next()
                    .unwrap()
                    .len_utf8();

                self.current_byte_position += char_length;
                let end_position = start_position + char_length;

                GeneratorToken {
                    kind: GeneratorTokenKind::UnexpectedCharacter,
                    text: &self.source[start_position..end_position],
                    span: start_position..end_position,
                }
            } else {
                self.current_byte_position += current_max_length;

                if self.ignore_whitespace && current_token_kind == GeneratorTokenKind::Whitespace {
                    continue;
                }

                let end_position = self.current_byte_position;

                GeneratorToken {
                    kind: current_token_kind,
                    text: &self.source[start_position..end_position],
                    span: start_position..end_position,
                }
            };

            return Some(token);
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct GeneratorAnchor {
    byte_position: usize,
}

impl GeneratorAnchor {
    pub fn elapsed(&self, lexer: &GeneratorLexer) -> Range<usize> {
        // skip until not whitespace
        let floor = lexer.source[self.byte_position..]
            .chars()
            .take_while(|char| char.is_whitespace())
            .map(|char| char.len_utf8())
            .sum::<usize>();

        let start = self.byte_position + floor;
        let end = lexer.current_byte_position.max(start);

        start..end
    }
}
