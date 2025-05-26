use std::{borrow::Cow, iter::once, ops::Range, sync::LazyLock};

use auto_enums::auto_enum;
use regex::Regex;
use serde::{Deserialize, Serialize};

use crate::lexer::{GeneratorTokenKind, GeneratorTokenizer, TokenizerRegistry};

use super::{AST, Parser, ParserGenerator, ParserPart};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Literal {
    Normal(NormalLiteral),
    String(StringLiteral),
    Float(FloatLiteral),
    Binary(BinaryLiteral),
    Bool(BoolLiteral),
    Custom(CustomRegexLiteral),
}

impl Literal {
    pub fn register(
        &self,
        registry: &mut TokenizerRegistry,
    ) -> (GeneratorTokenKind, Option<CustomLiteralOption>) {
        match self {
            Literal::Normal(normal_literal) => (normal_literal.register(registry), None),
            Literal::String(string_literal) => (string_literal.register(registry), None),
            Literal::Float(float_literal) => (float_literal.register(registry), None),
            Literal::Binary(binary_literal) => (binary_literal.register(registry), None),
            Literal::Bool(bool_literal) => (bool_literal.register(registry), None),
            Literal::Custom(custom_regex_literal) => (
                custom_regex_literal.regiter(registry),
                Some(custom_regex_literal.option.clone()),
            ),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NormalLiteral {
    pub allow_line: bool,
    pub symbol_regex: Option<String>,
}

impl NormalLiteral {
    pub fn build_regex(&self) -> String {
        let mut regex = r"(\w".to_string();

        if self.allow_line {
            regex += r"|\\-";
        }
        if let Some(symbol) = &self.symbol_regex {
            regex += format!("|({})", symbol).as_str();
        }
        regex += ")+";

        regex
    }

    pub fn register(&self, registry: &mut TokenizerRegistry) -> GeneratorTokenKind {
        registry.register(GeneratorTokenizer::regex(&self.build_regex()))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StringLiteral {
    pub quotes_kind: QuotesKind,
    pub escape: EscapeOption,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct EscapeOption {
    pub allow_escape: bool,
    pub unicode: UnicodeFormatKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum UnicodeFormatKind {
    /// No unicode escape
    #[default]
    None,
    /// \uFFFF
    Normal,
    /// \u{FFFF}
    WithBrace,
}

impl EscapeOption {
    pub fn resolve_escape<'input>(&self, input: &'input str) -> Cow<'input, str> {
        match self.allow_escape {
            true => {
                let mut new_string = String::new();

                let mut prev = '\0';
                let mut chars = input.chars();
                loop {
                    let Some(char) = chars.next() else { break };

                    if prev == '\\' {
                        if char == 'u' || char == 'U' {
                            let mut unicode = String::new();

                            for _ in 0..4 {
                                let Some(char) = chars.next() else { break };

                                unicode.push(char);
                            }

                            match u32::from_str_radix(unicode.as_str(), 16)
                                .ok()
                                .map(|code| char::from_u32(code))
                                .flatten()
                            {
                                Some(char) => new_string.push(char),
                                None => {
                                    new_string.push('\\');
                                    new_string.push(char);
                                    new_string += unicode.as_str();
                                }
                            }
                            continue;
                        }

                        let replace = match char {
                            'b' => '\x08',
                            't' => '\t',
                            'n' => '\n',
                            'r' => '\r',
                            '0' => '\0',
                            '\\' => '\\',
                            _ => char,
                        };
                        new_string.push(replace);

                        continue;
                    }

                    new_string.push(char);
                    prev = char;
                }

                input.into()
            }
            false => input.into(),
        }
    }
}

impl StringLiteral {
    pub fn build_regex(&self) -> String {
        let allow_escape = self.escape.allow_escape;
        let unicode_format = match self.escape.unicode {
            UnicodeFormatKind::None => "",
            UnicodeFormatKind::Normal => r"|\\[uU][0-9A-Fa-f]{1,6}",
            UnicodeFormatKind::WithBrace => r"|\\[uU]\{[0-9A-Fa-f]{1,6}\}",
        };

        match self.quotes_kind {
            QuotesKind::DoubleQuotes => match allow_escape {
                true => format!(
                    r#""(?:[^"\\\r\n]|\\([btnr0'"\\\n\r]|\r\n){})*""#,
                    unicode_format
                )
                .into(),
                false => r#"".*""#.into(),
            },
            QuotesKind::TripleDoubleQuotes => match allow_escape {
                true => format!(
                    r#""""(?:[^"\\\r\n]|\\([btnr0'"\\\n\r]|\r\n){}|"[^"]|""[^"])*""""#,
                    unicode_format
                )
                .into(),
                false => r#""""([^"]|("|"")[^"])*""""#.into(),
            },
            QuotesKind::SingleQuote => match allow_escape {
                true => format!(
                    r#"'(?:[^'\\\r\n]|\\([btnr0'"\\\n\r]|\r\n){})*'"#,
                    unicode_format
                )
                .into(),
                false => r"'.*'".into(),
            },
            QuotesKind::TripleQuotes => match allow_escape {
                true => format!(
                    r#"'''(?:[^'\\\r\n]|\\([btnr0'"\\\n\r]|\r\n){}|'[^']|''[^'])*'''"#,
                    unicode_format
                )
                .into(),
                false => r"'''([^']|('|'')[^'])*'''".into(),
            },
        }
    }

    pub fn register(&self, registry: &mut TokenizerRegistry) -> GeneratorTokenKind {
        registry.register(GeneratorTokenizer::regex(&self.build_regex()))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum QuotesKind {
    DoubleQuotes,
    TripleDoubleQuotes,
    SingleQuote,
    TripleQuotes,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FloatLiteral {
    pub allow_e: bool,
    pub inf_nan_kind: InfNanKind,
    pub allow_plus_minus: bool,
    pub allow_under_line: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum InfNanKind {
    None,
    CamelCase,
    UpperCase,
    LowerCase,
    Insensitive,
}

impl FloatLiteral {
    pub fn build_regex(&self) -> String {
        let digit = match self.allow_under_line {
            true => r"[\d_]",
            false => r"[\d]",
        };

        let e_suffix = match self.allow_e {
            true => format!("[eE][+-]{}+", digit),
            false => "".to_string(),
        };

        let plus_minus = match self.allow_plus_minus {
            true => "[+-]?",
            false => "",
        };

        let inf_nan = match self.inf_nan_kind {
            InfNanKind::None => "",
            InfNanKind::CamelCase => "|Inf|Nan",
            InfNanKind::UpperCase => "|INF|NAN",
            InfNanKind::LowerCase => "|inf|nan",
            InfNanKind::Insensitive => "|[Ii][Nn][Ff]|[Nn][Aa][Nn]",
        };

        format!(
            r"{}{}+(\.{}+)?({})?{}",
            plus_minus, digit, digit, e_suffix, inf_nan
        )
    }

    pub fn register(&self, registry: &mut TokenizerRegistry) -> GeneratorTokenKind {
        registry.register(GeneratorTokenizer::regex(&self.build_regex()))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BinaryLiteral {
    pub allow_bit: bool,
    pub allow_oct: bool,
    pub allow_hex: bool,
    pub allow_plus_minus: bool,
    pub allow_under_line: bool,
}

impl BinaryLiteral {
    pub fn build_regex(&self) -> String {
        let plus_minus = match self.allow_plus_minus {
            true => "[+-]?",
            false => "",
        };

        let under_line = match self.allow_under_line {
            true => "_",
            false => "",
        };

        let bit = match self.allow_hex {
            true => format!("0b[01{}]+", under_line),
            false => "".to_string(),
        };
        let oct = match self.allow_hex {
            true => format!("0x[0-7|{}]+", under_line),
            false => "".to_string(),
        };
        let hex = match self.allow_hex {
            true => format!("0x[a-f|A-F|0-9|{}]+", under_line),
            false => "".to_string(),
        };

        format!("{}({}|{}|{})", plus_minus, bit, oct, hex)
    }

    pub fn register(&self, registry: &mut TokenizerRegistry) -> GeneratorTokenKind {
        registry.register(GeneratorTokenizer::regex(&self.build_regex()))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BoolLiteral {
    pub kind: BoolKind,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub enum BoolKind {
    CamelCase,
    UpperCase,
    LowerCase,
    Insensitive,
}

impl BoolLiteral {
    pub fn build_regx(&self) -> String {
        match self.kind {
            BoolKind::CamelCase => "True|False",
            BoolKind::UpperCase => "TRUE|FALSE",
            BoolKind::LowerCase => "true|false",
            BoolKind::Insensitive => "[Tt][Rr][Uu][Ee]|[Ff][Aa][Ll][Ss][Ee]",
        }
        .into()
    }

    pub fn register(&self, registry: &mut TokenizerRegistry) -> GeneratorTokenKind {
        registry.register(GeneratorTokenizer::regex(&self.build_regx()))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CustomRegexLiteral {
    pub regex: String,
    pub option: CustomLiteralOption,
}

impl CustomRegexLiteral {
    pub fn build_regex(&self) -> String {
        self.regex.clone()
    }

    pub fn regiter(&self, registry: &mut TokenizerRegistry) -> GeneratorTokenKind {
        registry.register(GeneratorTokenizer::regex(self.regex.as_str()))
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CustomLiteralOption {
    pub trim_space: bool,
    pub escape: EscapeOption,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct LiteralSet {
    pub normal: Option<NormalLiteral>,
    pub strings: Vec<StringLiteral>,
    pub custom: Option<CustomRegexLiteral>,
}

pub struct LiteralSetParser {
    pub normal: Option<GeneratorTokenKind>,
    pub strings: Vec<(GeneratorTokenKind, QuotesKind, EscapeOption)>,
    pub custom: Option<(GeneratorTokenKind, CustomLiteralOption)>,
}

#[derive(Debug)]
pub enum LiteralSetAST<'input> {
    Normal {
        text: &'input str,
        span: Range<usize>,
    },
    String {
        text: &'input str,
        kind: QuotesKind,
        option: EscapeOption,
        span: Range<usize>,
    },
    Custom {
        text: &'input str,
        option: CustomLiteralOption,
        span: Range<usize>,
    },
}

impl<'input> ParserGenerator<'input, LiteralSetAST<'input>, LiteralSetParser> for LiteralSet {
    fn generate(&self, registry: &mut TokenizerRegistry) -> LiteralSetParser {
        LiteralSetParser {
            normal: self.normal.as_ref().map(|normal| normal.register(registry)),
            strings: self
                .strings
                .iter()
                .map(|string| {
                    (
                        string.register(registry),
                        string.quotes_kind,
                        string.escape.clone(),
                    )
                })
                .collect(),
            custom: self
                .custom
                .as_ref()
                .map(|custom| (custom.regiter(registry), custom.option.clone())),
        }
    }
}

impl<'input> Parser<'input, LiteralSetAST<'input>> for LiteralSetParser {
    fn parse(
        &self,
        lexer: &mut crate::lexer::GeneratorLexer<'input, '_>,
        _: &mut allocator_api2::vec::Vec<super::error::GeneratedParseError>,
    ) -> Option<LiteralSetAST<'input>> {
        if let Some(normal) = &self.normal {
            if lexer.current_contains(*normal) {
                let token = lexer.next().unwrap();

                return Some(LiteralSetAST::Normal {
                    text: token.text,
                    span: token.span,
                });
            }
        }

        for (token_kind, quotes_kind, escape_option) in self.strings.iter() {
            if lexer.current_contains(*token_kind) {
                let token = lexer.next().unwrap();

                return Some(LiteralSetAST::String {
                    text: token.text,
                    kind: *quotes_kind,
                    option: escape_option.clone(),
                    span: token.span,
                });
            }
        }

        if let Some((token_kind, custom_option)) = &self.custom {
            if lexer.current_contains(*token_kind) {
                let token = lexer.next().unwrap();

                return Some(LiteralSetAST::Custom {
                    text: token.text,
                    option: custom_option.clone(),
                    span: token.span,
                });
            }
        }

        None
    }

    fn first_token_kinds(&self) -> impl Iterator<Item = GeneratorTokenKind> {
        [self.normal, self.custom.as_ref().map(|(kind, _)| *kind)]
            .into_iter()
            .flatten()
            .chain(self.strings.iter().map(|(kind, _, _)| *kind))
    }
}

impl<'input> ParserPart for LiteralSetParser {
    fn parse_error_code(&self) -> usize {
        unreachable!()
    }

    fn expected_format(&self) -> Option<Cow<'static, str>> {
        unreachable!()
    }
}

impl<'input> AST<'input> for LiteralSetAST<'input> {
    fn span(&self) -> Range<usize> {
        match self {
            LiteralSetAST::Normal { text: _, span } => span.clone(),
            LiteralSetAST::String {
                text: _,
                kind: _,
                option: _,
                span,
            } => span.clone(),
            LiteralSetAST::Custom {
                text: _,
                option: _,
                span,
            } => span.clone(),
        }
    }

    fn take_value(
        &self,
        _: &mut allocator_api2::vec::Vec<
            (Cow<'input, str>, Range<usize>, Range<usize>),
            &bumpalo::Bump,
        >,
        _: &mut tyml_validate::validate::ValueTypeChecker<'_, '_, '_, '_, 'input, 'input>,
    ) {
        unreachable!()
    }

    fn take_token(
        &self,
        _: &mut std::collections::BTreeMap<usize, (super::ASTTokenKind, Range<usize>)>,
    ) {
        unreachable!()
    }
}

impl<'input> LiteralSetAST<'input> {
    pub fn to_literal(&self) -> Cow<'input, str> {
        match self {
            LiteralSetAST::Normal { text, span: _ } => (*text).into(),
            LiteralSetAST::String {
                text,
                kind,
                option,
                span: _,
            } => {
                let text = match kind {
                    QuotesKind::DoubleQuotes => &text[1..(text.len() - 1)],
                    QuotesKind::TripleDoubleQuotes => &text[3..(text.len() - 3)],
                    QuotesKind::SingleQuote => &text[1..(text.len() - 1)],
                    QuotesKind::TripleQuotes => &text[3..(text.len() - 3)],
                };
                option.resolve_escape(text)
            }
            LiteralSetAST::Custom {
                text,
                option,
                span: _,
            } => {
                let text = match option.trim_space {
                    true => text.trim(),
                    false => text,
                };
                option.escape.resolve_escape(text)
            }
        }
    }

    #[auto_enum(Iterator)]
    pub fn to_section_name(
        &self,
        define_span: Range<usize>,
    ) -> impl Iterator<Item = (Cow<'input, str>, Range<usize>, Range<usize>)> {
        match self {
            LiteralSetAST::Normal { text, span } => text
                .split(".")
                .map(move |text| (text.into(), span.clone(), define_span.clone())),
            LiteralSetAST::String {
                text: _,
                kind: _,
                option: _,
                span,
            } => once((self.to_literal(), span.clone(), define_span)),
            LiteralSetAST::Custom { text, option, span } => {
                #[auto_enum(Iterator)]
                fn split<'input>(
                    text: &'input str,
                    option: &CustomLiteralOption,
                    span: &Range<usize>,
                    define_span: Range<usize>,
                ) -> impl Iterator<Item = (Cow<'input, str>, Range<usize>, Range<usize>)>
                {
                    match option.escape.allow_escape {
                        true => {
                            static SPLIT: LazyLock<Regex> =
                                LazyLock::new(|| Regex::new(r"([^\.\\]|\\.)+").unwrap());

                            SPLIT.find_iter(text).map(move |matched| {
                                let text = match option.trim_space {
                                    true => matched.as_str().trim(),
                                    false => matched.as_str(),
                                };
                                (
                                    option.escape.resolve_escape(text),
                                    span.clone(),
                                    define_span.clone(),
                                )
                            })
                        }
                        false => text.split(".").map(move |text| {
                            let text = match option.trim_space {
                                true => text.trim(),
                                false => text,
                            };
                            (text.into(), span.clone(), define_span.clone())
                        }),
                    }
                }

                split(*text, option, span, define_span)
            }
        }
    }
}
