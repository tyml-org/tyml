use std::ops::Range;

use extension_fn::extension_fn;
use regex::Regex;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    /// ,
    Comma,
    /// .
    Period,
    /// *
    Asterisk,
    /// :
    Colon,
    /// ?
    QuestionMark,
    /// =
    Equal,
    /// null
    Null,
    /// inf
    Inf,
    /// nan
    Nan,
    /// type
    Type,
    /// enum
    Enum,
    /// {
    BraceLeft,
    /// }
    BraceRight,
    /// e.g. -6.3, +5E+2
    FloatNumeric,
    /// e.g. 0xFF, 0b0101, 0o1234567
    BinaryNumeric,
    /// e.g. literal
    Literal,
    /// e.g. "literal"
    StringLiteral,
    /// e.g. '\n'
    LineFeed,
    /// ' '
    Whitespace,
    UnexpectedCharacter,
    None,
}

static TOKENIZERS: &[Tokenizer] = &[
    Tokenizer::Keyword(TokenKind::Comma, ","),
    Tokenizer::Keyword(TokenKind::Period, "."),
    Tokenizer::Keyword(TokenKind::Asterisk, "*"),
    Tokenizer::Keyword(TokenKind::Colon, ":"),
    Tokenizer::Keyword(TokenKind::QuestionMark, "?"),
    Tokenizer::Keyword(TokenKind::Equal, "="),
    Tokenizer::Keyword(TokenKind::Null, "null"),
    Tokenizer::Keyword(TokenKind::Inf, "inf"),
    Tokenizer::Keyword(TokenKind::Nan, "nan"),
    Tokenizer::Keyword(TokenKind::Type, "type"),
    Tokenizer::Keyword(TokenKind::Enum, "enum"),
    Tokenizer::Keyword(TokenKind::BraceLeft, "{"),
    Tokenizer::Keyword(TokenKind::BraceRight, "}"),
    Tokenizer::Regex(
        TokenKind::FloatNumeric,
        r"[+-]?[\d_]+(\.[\d_]+)?([eE][+-][\d_]+)?",
    ),
    Tokenizer::Regex(TokenKind::BinaryNumeric, r"0x[a-f|A-F|0-9|_]+"),
    Tokenizer::Regex(TokenKind::BinaryNumeric, r"0o[0-7|_]+"),
    Tokenizer::Regex(TokenKind::BinaryNumeric, r"0b[01_]+"),
    Tokenizer::Regex(TokenKind::Literal, r"\w+"),
    Tokenizer::Regex(TokenKind::StringLiteral, r#""([^"\\]|\\.)*""#),
    Tokenizer::Regex(TokenKind::StringLiteral, r"'([^'\\]|\\.)*'"),
    Tokenizer::Regex(TokenKind::LineFeed, r"\n|\r"),
    Tokenizer::Regex(TokenKind::Whitespace, r"[ 　\t]+"),
];

enum Tokenizer {
    Keyword(TokenKind, &'static str),
    Regex(TokenKind, &'static str),
}

impl Tokenizer {
    fn tokenize(
        &self,
        current_input: &str,
        index: usize,
        regex_cache: &mut [Option<Regex>],
    ) -> (TokenKind, usize) {
        return match self {
            Tokenizer::Keyword(kind, keyword) => {
                let mut input_chars = current_input.chars();
                let mut keyword_chars = keyword.chars();
                let mut current_byte_length = 0;
                loop {
                    let keyword_char = match keyword_chars.next() {
                        Some(c) => c,
                        _ => break,
                    };
                    let current_char = match input_chars.next() {
                        Some(c) => c,
                        _ => return (kind.clone(), 0), // reject
                    };
                    if current_char != keyword_char {
                        return (kind.clone(), 0); // reject
                    }

                    current_byte_length += current_char.len_utf8();
                }
                (kind.clone(), current_byte_length) // accept
            }
            Tokenizer::Regex(kind, regex) => {
                let regex = (&mut regex_cache[index])
                    .get_or_insert_with(|| Regex::new(format!("^{}", regex).as_str()).unwrap());

                let length = match regex.find(current_input) {
                    Some(matched) => matched.end(),
                    None => 0,
                };

                (kind.clone(), length)
            }
        };
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token<'input> {
    pub kind: TokenKind,
    pub text: &'input str,
    pub span: Range<usize>,
}

#[extension_fn(Option<Token<'_>>)]
pub fn get_kind(&self) -> TokenKind {
    self.as_ref()
        .map(|token| token.kind)
        .unwrap_or(TokenKind::None)
}

pub struct Lexer<'input> {
    source: &'input str,
    current_byte_position: usize,
    regex_cache: Box<[Option<Regex>]>,
    current_token_cache: Option<Token<'input>>,
}

impl<'input> Lexer<'input> {
    pub fn new(source: &'input str) -> Self {
        Self {
            source,
            current_byte_position: 0,
            regex_cache: vec![None; TOKENIZERS.len()].into_boxed_slice(),
            current_token_cache: None,
        }
    }

    pub fn current(&mut self) -> Option<Token<'input>> {
        let anchor = self.cast_anchor();

        // move to next temporarily
        self.current_token_cache = self.next();

        // back to anchor position
        self.current_byte_position = anchor.byte_position;

        self.current_token_cache.clone()
    }

    pub fn cast_anchor(&self) -> Anchor {
        Anchor {
            byte_position: self.current_byte_position,
        }
    }

    pub fn skip_line_feed(&mut self) {
        loop {
            if let TokenKind::LineFeed = self.current().get_kind() {
                self.next();
                continue;
            } else {
                return;
            }
        }
    }

    pub fn back_to_anchor(&mut self, anchor: Anchor) {
        self.current_byte_position = anchor.byte_position;
        self.current_token_cache = None;
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Token<'input>;

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
            let mut current_token_kind = TokenKind::Whitespace;

            for (index, tokenizer) in TOKENIZERS.iter().enumerate() {
                let result = tokenizer.tokenize(current_input, index, &mut self.regex_cache);
                let token_kind = result.0;
                let byte_length = result.1;

                if byte_length > current_max_length {
                    current_max_length = byte_length;
                    current_token_kind = token_kind;
                }
            }

            let start_position = self.current_byte_position;

            let token = if current_max_length == 0 {
                // TODO : get char's utf-8 length
                self.current_byte_position += 1;
                let end_position = start_position + 1;

                Token {
                    kind: TokenKind::UnexpectedCharacter,
                    text: &self.source[start_position..end_position],
                    span: start_position..end_position,
                }
            } else {
                self.current_byte_position += current_max_length;

                if current_token_kind == TokenKind::Whitespace {
                    continue;
                }

                let end_position = self.current_byte_position;

                Token {
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
pub struct Anchor {
    byte_position: usize,
}

impl Anchor {
    pub fn elapsed(&self, lexer: &Lexer) -> Range<usize> {
        assert!(self.byte_position <= lexer.current_byte_position);

        self.byte_position..lexer.current_byte_position
    }
}
