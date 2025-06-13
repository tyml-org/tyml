use std::{collections::HashSet, fmt::Debug, mem::swap, ops::Range, sync::Arc};

use allocator_api2::vec::Vec;
use bumpalo::Bump;
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
        Self::Regex(Regex::new(format!("^({})", regex_str).as_str()).unwrap())
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

impl Debug for GeneratorTokenizer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GeneratorTokenizer::Keyword(keyword) => {
                write!(f, "GeneratorTokenizer::Keyword({})", keyword)
            }
            GeneratorTokenizer::Regex(regex) => write!(f, "GeneratorTokenizer::Regex({:?})", regex),
            GeneratorTokenizer::Function(_) => write!(f, "GeneratorTokenizer::Function(...)"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GeneratorTokenKind(usize);

#[allow(non_upper_case_globals)]
impl GeneratorTokenKind {
    pub const None: Self = GeneratorTokenKind(0);
    pub const Whitespace: Self = GeneratorTokenKind(1);
    pub const LineFeed: Self = GeneratorTokenKind(2);
    pub const UnexpectedCharacter: Self = GeneratorTokenKind(usize::MAX);
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GeneratorToken<'input, 'parse> {
    pub kinds: allocator_api2::vec::Vec<GeneratorTokenKind, &'parse Bump>,
    pub text: &'input str,
    pub span: Range<usize>,
}

impl<'input> GeneratorToken<'input, '_> {
    pub fn into_spanned(&self) -> SpannedText<'input> {
        SpannedText::new(self.text, self.span.clone())
    }
}

#[extension_fn(<'parse> Option<GeneratorToken<'_, 'parse>>)]
pub fn get_kinds(&self) -> Option<&allocator_api2::vec::Vec<GeneratorTokenKind, &'parse Bump>> {
    self.as_ref().map(|token| &token.kinds)
}

#[derive(Debug)]
pub struct SpannedText<'input> {
    pub text: &'input str,
    pub span: Range<usize>,
}

impl<'input> SpannedText<'input> {
    pub fn new(text: &'input str, span: Range<usize>) -> Self {
        Self { text, span }
    }
}

pub struct TokenizerRegistry {
    registry: Either<Vec<GeneratorTokenizer>, Arc<Vec<GeneratorTokenizer>>>,
}

impl TokenizerRegistry {
    pub fn new() -> Self {
        let mut default = Vec::new();
        default.push(GeneratorTokenizer::Function(Box::new(|_| 0))); // None
        default.push(GeneratorTokenizer::regex(r"[ 　\t]+")); // Whitespace
        default.push(GeneratorTokenizer::regex(r"(\n|\r)"));

        Self {
            registry: Either::Left(default),
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

    pub fn get_registry(&self) -> Arc<Vec<GeneratorTokenizer>> {
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

pub struct GeneratorLexer<'input, 'parse> {
    source: &'input str,
    tokenizers: Arc<Vec<GeneratorTokenizer>>,
    pub comments: HashSet<GeneratorTokenKind>,
    pub comment_spans: Vec<Range<usize>>,
    current_byte_position: usize,
    current_token_cache: Option<GeneratorToken<'input, 'parse>>,
    pub ignore_whitespace: bool,
    allocator: &'parse Bump,
}

impl<'input, 'parse> GeneratorLexer<'input, 'parse> {
    pub fn new(source: &'input str, registry: &TokenizerRegistry, allocator: &'parse Bump) -> Self {
        Self {
            source,
            tokenizers: registry.get_registry(),
            comments: HashSet::new(),
            comment_spans: Vec::new(),
            current_byte_position: 0,
            current_token_cache: None,
            ignore_whitespace: true,
            allocator,
        }
    }

    pub fn current(&mut self) -> Option<GeneratorToken<'input, 'parse>> {
        let anchor = self.cast_anchor();

        // move to next temporarily
        self.current_token_cache = self.next();

        // back to anchor position
        self.current_byte_position = anchor.byte_position;

        self.current_token_cache.clone()
    }

    pub fn current_contains(&mut self, kind: GeneratorTokenKind) -> bool {
        match self.current() {
            Some(token) => token.kinds.contains(&kind),
            None => false,
        }
    }

    pub fn cast_anchor(&self) -> GeneratorAnchor {
        GeneratorAnchor {
            byte_position: self.current_byte_position,
        }
    }

    pub fn skip(&mut self, kind: &[GeneratorTokenKind]) {
        loop {
            let current = self.current();

            let Some(current_kinds) = current.get_kinds() else {
                break;
            };

            if current_kinds
                .iter()
                .any(|current_kind| kind.contains(current_kind))
            {
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

    pub fn is_reached_eof(&mut self) -> bool {
        self.current().is_none()
    }

    pub fn is_current_lf(&mut self) -> bool {
        self.current_contains(GeneratorTokenKind::LineFeed)
    }

    pub fn skip_lf(&mut self) {
        loop {
            let Some(current) = self.current() else { break };

            if !current.kinds.contains(&GeneratorTokenKind::LineFeed) {
                break;
            }

            self.next();
        }
    }
}

impl<'input, 'parse> Iterator for GeneratorLexer<'input, 'parse> {
    type Item = GeneratorToken<'input, 'parse>;

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
            let mut current_token_kinds = Vec::new_in(self.allocator);

            for (index, tokenizer) in self.tokenizers.iter().enumerate() {
                let byte_length = tokenizer.tokenize(current_input);

                if byte_length == 0 {
                    continue;
                }

                if byte_length >= current_max_length {
                    if byte_length > current_max_length {
                        current_token_kinds.clear();
                    }
                    current_max_length = byte_length;
                    current_token_kinds.push(GeneratorTokenKind(index));
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

                current_token_kinds.push(GeneratorTokenKind::UnexpectedCharacter);

                GeneratorToken {
                    kinds: current_token_kinds,
                    text: &self.source[start_position..end_position],
                    span: start_position..end_position,
                }
            } else {
                self.current_byte_position += current_max_length;

                if self.ignore_whitespace
                    && current_token_kinds.contains(&GeneratorTokenKind::Whitespace)
                {
                    continue;
                }

                // ignore comment token
                if current_token_kinds
                    .iter()
                    .any(|kind| self.comments.contains(kind))
                {
                    self.comment_spans
                        .push(start_position..self.current_byte_position);

                    continue;
                }

                let end_position = self.current_byte_position;

                GeneratorToken {
                    kinds: current_token_kinds,
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
    pub byte_position: usize,
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
