use std::ops::Range;

use allocator_api2::vec::Vec;
use bumpalo::Bump;

pub trait AST {
    fn span(&self) -> Range<usize>;
}

pub struct Spanned<T> {
    pub value: T,
    pub span: Range<usize>,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Range<usize>) -> Self {
        Self { value, span }
    }
}

pub type Literal<'input> = Spanned<&'input str>;

pub struct Defines<'input, 'allocator> {
    pub defines: Vec<Define<'input, 'allocator>, &'allocator Bump>,
    pub span: Range<usize>,
}

pub enum Define<'input, 'allocator> {
    Element(ElementDefine<'input, 'allocator>),
    Type(TypeDefine<'input, 'allocator>),
}

pub struct ElementDefine<'input, 'allocator> {
    pub node: Vec<NodeLiteral<'input>, &'allocator Bump>,
    pub ty: Option<ElementType<'input>>,
    pub default: Option<DefaultValue<'input>>,
    pub span: Range<usize>,
}

pub enum NodeLiteral<'input> {
    Literal(Literal<'input>),
    Asterisk(Literal<'input>),
}

pub struct ElementType<'input> {
    pub name: Literal<'input>,
    pub optional: Option<Range<usize>>,
    pub span: Range<usize>,
}

pub struct DefaultValue<'input> {
    pub value: ValueLiteral<'input>,
    pub span: Range<usize>,
}

pub enum ValueLiteral<'input> {
    String(Literal<'input>),
    Numeric(NumericLiteral<'input>),
    Null(Literal<'input>),
}

pub enum NumericLiteral<'input> {
    Float(FloatLiteral<'input>),
    Binary(BinaryLiteral<'input>),
}

pub enum FloatLiteral<'input> {
    Float(Literal<'input>),
    Inf(Literal<'input>),
    Nan(Literal<'input>),
}

pub enum BinaryLiteral<'input> {
    Hex(Literal<'input>),
    Oct(Literal<'input>),
    Bin(Literal<'input>),
}

pub enum TypeDefine<'input, 'allocator> {
    Struct(StructDefine<'input, 'allocator>),
    Enum(EnumDefine<'input, 'allocator>),
}

pub struct StructDefine<'input, 'allocator> {
    pub name: Literal<'input>,
    pub defines: &'allocator Defines<'input, 'allocator>,
    pub span: Range<usize>,
}

pub struct EnumDefine<'input, 'allocator> {
    pub name: Literal<'input>,
    pub elements: Vec<Literal<'input>, &'allocator Bump>,
    pub span: Range<usize>,
}
