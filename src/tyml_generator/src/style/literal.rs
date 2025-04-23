use std::{borrow::Cow, sync::LazyLock};

use regex::Regex;

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
            Literal::Normal(normal_literal) => {
                let custom_option = match normal_literal.allow_escape {
                    true => Some(CustomLiteralOption {
                        trim_space: false,
                        allow_escape: true,
                    }),
                    false => None,
                };
                (normal_literal.register(registry), custom_option)
            }
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
    pub allow_escape: bool,
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
        if self.allow_escape {
            regex = format!(r"({})|[^\\]|\\.)+", regex);
        } else {
            regex += ")+";
        }

        regex
    }

    pub fn register(&self, registry: &mut TokenizerRegistry) -> GeneratorTokenKind {
        registry.register(GeneratorTokenizer::regex(&self.build_regex()))
    }
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub quotes_kind: QuotesKind,
    pub allow_escape: bool,
}

impl StringLiteral {
    pub fn build_regex(&self) -> String {
        match self.quotes_kind {
            QuotesKind::DoubleQuotes => match self.allow_escape {
                true => r#""([^"\\]|\\.)*""#,
                false => r#"".*""#,
            },
            QuotesKind::TripleDoubleQuotes => match self.allow_escape {
                true => r#""""([^"\\]|\\.|("|"")[^"])*""""#,
                false => r#""""([^"]|("|"")[^"])*""""#,
            },
            QuotesKind::SingleQuote => match self.allow_escape {
                true => r"'([^'\\]|\\.)*'",
                false => r"'.*'",
            },
            QuotesKind::TripleQuotes => match self.allow_escape {
                true => r"'''([^'\\]|\\.|('|'')[^'])*'''",
                false => r"'''([^']|('|'')[^'])*'''",
            },
        }
        .into()
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
    pub allow_escape: bool,
}

impl CustomLiteralOption {
    pub fn resolve_escape<'input>(&self, input: &'input str) -> Cow<'input, str> {
        match self.allow_escape {
            true => {
                static NULL: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(\\\\|[^\\]|^)\\0").unwrap());
                static TAB: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(\\\\|[^\\]|^)\\t").unwrap());
                static CR: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(\\\\|[^\\]|^)\\r").unwrap());
                static LF: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(\\\\|[^\\]|^)\\n").unwrap());
                static ESCAPE: LazyLock<Regex> =
                    LazyLock::new(|| Regex::new(r"(\\\\|[^\\]|^)\\(.)").unwrap());

                let input = NULL.replace_all(input, "\0");
                let input = TAB.replace_all(input.as_ref(), "\t");
                let input = CR.replace_all(input.as_ref(), "\r");
                let mut input = LF.replace_all(input.as_ref(), "\n");

                for capture in ESCAPE.captures_iter(input.to_string().as_str()) {
                    let regex = Regex::new(format!(r"(?<!\\){}", &capture[0]).as_str()).unwrap();

                    input = regex.replace(&capture[0], &capture[1]).to_string().into();
                }

                input.to_string().into()
            }
            false => input.into(),
        }
    }
}
