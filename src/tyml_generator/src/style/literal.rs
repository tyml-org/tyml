use extension_fn::extension_fn;

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
    pub fn register(&self, registry: &mut TokenizerRegistry) -> GeneratorTokenKind {
        match self {
            Literal::Normal(normal_literal) => normal_literal.register(registry),
            Literal::String(string_literal) => string_literal.register(registry),
            Literal::Float(float_literal) => float_literal.register(registry),
            Literal::Binary(binary_literal) => binary_literal.register(registry),
            Literal::Bool(bool_literal) => bool_literal.register(registry),
            Literal::Custom(custom_regex_literal) => custom_regex_literal.regiter(registry),
        }
    }
}

#[extension_fn(<'item, I: Iterator<Item = &'item Literal>> I)]
pub fn register(self, registry: &mut TokenizerRegistry) -> GeneratorTokenKind {
    let mut all_regex = Vec::new();

    for literal in self {
        let regex = match literal {
            Literal::Normal(normal_literal) => normal_literal.build_regex(),
            Literal::String(string_literal) => string_literal.build_regex(),
            Literal::Float(float_literal) => float_literal.build_regex(),
            Literal::Binary(binary_literal) => binary_literal.build_regex(),
            Literal::Bool(bool_literal) => bool_literal.build_regx(),
            Literal::Custom(custom_regex_literal) => custom_regex_literal.build_regex(),
        };

        all_regex.push(regex);
    }

    registry.register(GeneratorTokenizer::regex(
        all_regex
            .iter()
            .map(|str| format!("({})", str))
            .collect::<Vec<_>>()
            .join("|")
            .as_str(),
    ))
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
            r"{}{}(\.{})?{}{}",
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
}

impl CustomRegexLiteral {
    pub fn build_regex(&self) -> String {
        self.regex.clone()
    }

    pub fn regiter(&self, registry: &mut TokenizerRegistry) -> GeneratorTokenKind {
        registry.register(GeneratorTokenizer::regex(self.regex.as_str()))
    }
}
