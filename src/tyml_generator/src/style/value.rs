use super::literal::{BinaryLiteral, BoolLiteral, FloatLiteral, Literal, StringLiteral};

#[derive(Debug)]
pub struct Value {
    pub string: Option<StringLiteral>,
    pub float: Option<FloatLiteral>,
    pub binary: Option<BinaryLiteral>,
    pub bool: Option<BoolLiteral>,

    pub any_string: Option<Vec<Literal>>,
}
