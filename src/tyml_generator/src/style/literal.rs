#[derive(Debug)]
pub enum Literal {
    Normal(NormalLiteral),
    String(StringLiteral),
    Float(FloatLiteral),
    Binary(BinaryLiteral),
    Bool(BoolLiteral),
}

#[derive(Debug)]
pub struct NormalLiteral {
    pub allow_line: bool,
    pub allow_under_line: bool,
    pub allow_symbol: bool,
    pub allow_escape: bool,
}

#[derive(Debug)]
pub struct StringLiteral {
    pub double_quotes: bool,
    pub triple_double_quotes: bool,
    pub single_quote: bool,
    pub triple_quotes: bool,
    pub allow_escape: bool,
}

#[derive(Debug)]
pub struct FloatLiteral {
    pub allow_dot: bool,
    pub allow_e: bool,
    pub allow_inf: bool,
    pub allow_nan: bool,
    pub allow_plus: bool,
    pub allow_minus: bool,
    pub allow_under_line: bool,
}

#[derive(Debug)]
pub struct BinaryLiteral {
    pub allow_bit: bool,
    pub allow_oct: bool,
    pub allow_hex: bool,
    pub allow_plus: bool,
    pub allow_minus: bool,
    pub allow_under_line: bool,
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
