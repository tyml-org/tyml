use crate::lexer::{GeneratorTokenKind, GeneratorTokenizer, TokenizerRegistry};

#[derive(Debug)]
pub enum Literal {
    Normal(NormalLiteral),
    String(StringLiteral),
    Float(FloatLiteral),
    Binary(BinaryLiteral),
    Bool(BoolLiteral),
}

impl Literal {
    pub fn register(&self, registry: &mut TokenizerRegistry) -> GeneratorTokenKind {
        match self {
            Literal::Normal(normal_literal) => normal_literal.register(registry),
            Literal::String(string_literal) => string_literal.register(registry),
            Literal::Float(float_literal) => float_literal.register(registry),
            Literal::Binary(binary_literal) => binary_literal.register(registry),
            Literal::Bool(bool_literal) => bool_literal.register(registry),
        }
    }
}

#[derive(Debug, Clone)]
pub struct NormalLiteral {
    pub allow_line: bool,
    pub allow_symbol: bool,
    pub allow_escape: bool,
}

impl NormalLiteral {
    pub fn register(&self, registry: &mut TokenizerRegistry) -> GeneratorTokenKind {
        let mut regex = r"[\w".to_string();

        if self.allow_line {
            regex += r"-";
        }
        if self.allow_symbol {
            regex += r"\S";
        }
        if self.allow_escape {
            regex = format!(r"({}]|[^\\]|\\.)+", regex);
        } else {
            regex += "]+";
        }

        registry.register(GeneratorTokenizer::regex(regex.as_str()))
    }
}

#[derive(Debug)]
pub struct StringLiteral {
    pub quotes_kind: QuotesKind,
    pub allow_escape: bool,
}

impl StringLiteral {
    pub fn register(&self, registry: &mut TokenizerRegistry) -> GeneratorTokenKind {
        let regex_str = match self.quotes_kind {
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
        };

        registry.register(GeneratorTokenizer::regex(regex_str))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QuotesKind {
    DoubleQuotes,
    TripleDoubleQuotes,
    SingleQuote,
    TripleQuotes,
}

#[derive(Debug)]
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
    pub fn register(&self, registry: &mut TokenizerRegistry) -> GeneratorTokenKind {
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

        let regex = format!(
            r"{}{}(\.{})?{}{}",
            plus_minus, digit, digit, e_suffix, inf_nan
        );

        registry.register(GeneratorTokenizer::regex(regex.as_str()))
    }
}

#[derive(Debug)]
pub struct BinaryLiteral {
    pub allow_bit: bool,
    pub allow_oct: bool,
    pub allow_hex: bool,
    pub allow_plus_minus: bool,
    pub allow_under_line: bool,
}

impl BinaryLiteral {
    pub fn register(&self, registry: &mut TokenizerRegistry) -> GeneratorTokenKind {
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

        let regex = format!("{}({}|{}|{})", plus_minus, bit, oct, hex);

        registry.register(GeneratorTokenizer::regex(regex.as_str()))
    }
}

#[derive(Debug)]
pub struct BoolLiteral {
    pub kind: BoolKind,
}

#[derive(Debug)]
pub enum BoolKind {
    CamelCase,
    UpperCase,
    LowerCase,
    Insensitive,
}

impl BoolLiteral {
    pub fn register(&self, registry: &mut TokenizerRegistry) -> GeneratorTokenKind {
        let regex = match self.kind {
            BoolKind::CamelCase => "True|False",
            BoolKind::UpperCase => "TRUE|FALSE",
            BoolKind::LowerCase => "true|false",
            BoolKind::Insensitive => "[Tt][Rr][Uu][Ee]|[Ff][Aa][Ll][Ss][Ee]",
        };

        registry.register(GeneratorTokenizer::regex(regex))
    }
}
