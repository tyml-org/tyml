use std::borrow::Cow;

use crate::lexer::{GeneratorTokenKind, GeneratorTokenizer, TokenizerRegistry};

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub quotes_kind: QuotesKind,
    pub escape: EscapeOption,
}

#[derive(Debug, Clone, Default)]
pub struct EscapeOption {
    pub allow_escape: bool,
    pub unicode: UnicodeFormatKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum UnicodeFormatKind {
    /// No unicode escape
    #[default]
    None,
    /// \uFFFF
    Normal,
    /// \u{FFFF}
    WithBrace,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QuotesKind {
    DoubleQuotes,
    TripleDoubleQuotes,
    SingleQuote,
    TripleQuotes,
}

#[derive(Debug, Clone)]
pub struct FloatLiteral {
    pub allow_e: bool,
    pub inf_nan_kind: InfNanKind,
    pub allow_plus_minus: bool,
    pub allow_under_line: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct BoolLiteral {
    pub kind: BoolKind,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone, Default)]
pub struct CustomLiteralOption {
    pub trim_space: bool,
    pub escape: EscapeOption,
}

impl CustomLiteralOption {
    pub fn resolve_escape<'input>(&self, input: &'input str) -> Cow<'input, str> {
        match self.escape.allow_escape {
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
