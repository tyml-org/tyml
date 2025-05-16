use std::{fmt::Debug, ops::Range};

use allocator_api2::vec::Vec;
use bumpalo::Bump;
use either::Either;
use extension_fn::extension_fn;

use crate::lexer::Token;

pub extern crate either;

pub trait AST: Debug {
    fn span(&self) -> Range<usize>;
}

macro_rules! impl_ast {
    ($ty:ty, span = self.span) => {
        impl AST for $ty {
            fn span(&self) -> Range<usize> {
                self.span.clone()
            }
        }
    };
    ($ty:ty, enum: $( $variant:ident ),*) => {
        impl AST for $ty {
            fn span(&self) -> Range<usize> {
                match self {
                    $( Self::$variant(ast) => ast.span(), )*
                }
            }
        }
    };
}

impl_ast!(Literal<'_>, span = self.span);
impl_ast!(Defines<'_, '_>, span = self.span);
impl_ast!(Define<'_, '_>, enum: Element, Type);
impl_ast!(ElementDefine<'_, '_>, span = self.span);
impl_ast!(NodeLiteral<'_>, enum: Literal, Asterisk);
impl_ast!(ElementType<'_, '_>, span = self.span);
impl_ast!(OrType<'_, '_>, span = self.span);
impl_ast!(ArrayType<'_, '_>, span = self.span);
impl_ast!(NamedType<'_>, span = self.span);
impl_ast!(ElementInlineType<'_, '_>, span = self.span);
impl_ast!(DefaultValue<'_>, span = self.span);
impl_ast!(ValueLiteral<'_>, enum: String, Float, Binary, Null);
impl_ast!(FloatLiteral<'_>, enum: Float, Inf, Nan);
impl_ast!(BinaryLiteral<'_>, enum: Hex, Oct, Bin);
impl_ast!(TypeDefine<'_, '_>, enum: Struct, Enum);
impl_ast!(StructDefine<'_, '_>, span = self.span);
impl_ast!(EnumDefine<'_, '_>, span = self.span);

#[derive(Debug, Clone)]
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

#[extension_fn(<'input> Token<'input>)]
pub fn into_literal(self) -> Literal<'input> {
    Literal::new(self.text, self.span)
}

#[derive(Debug)]
pub struct Defines<'input, 'allocator> {
    pub defines: Vec<Define<'input, 'allocator>, &'allocator Bump>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum Define<'input, 'allocator> {
    Element(ElementDefine<'input, 'allocator>),
    Type(TypeDefine<'input, 'allocator>),
}

#[derive(Debug)]
pub struct ElementDefine<'input, 'allocator> {
    pub node: NodeLiteral<'input>,
    pub ty: Option<ElementType<'input, 'allocator>>,
    pub inline_type: Option<ElementInlineType<'input, 'allocator>>,
    pub default: Option<DefaultValue<'input>>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum NodeLiteral<'input> {
    Literal(Literal<'input>),
    Asterisk(Literal<'input>),
}

#[derive(Debug)]
pub struct ElementType<'input, 'allocator> {
    pub type_info: OrType<'input, 'allocator>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct OrType<'input, 'allocator> {
    pub or_types: Vec<BaseType<'input, 'allocator>, &'allocator Bump>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct BaseType<'input, 'allocator> {
    pub ty: Either<NamedType<'input>, ArrayType<'input, 'allocator>>,
    pub optional: Option<Range<usize>>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct ArrayType<'input, 'allocator> {
    pub base: OrType<'input, 'allocator>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct NamedType<'input> {
    pub name: Literal<'input>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct ElementInlineType<'input, 'allocator> {
    pub defines: &'allocator Defines<'input, 'allocator>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct DefaultValue<'input> {
    pub value: ValueLiteral<'input>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum ValueLiteral<'input> {
    String(Literal<'input>),
    Float(FloatLiteral<'input>),
    Binary(BinaryLiteral<'input>),
    Null(Literal<'input>),
}

#[derive(Debug)]
pub enum FloatLiteral<'input> {
    Float(Literal<'input>),
    Inf(Literal<'input>),
    Nan(Literal<'input>),
}

#[derive(Debug)]
pub enum BinaryLiteral<'input> {
    Hex(Literal<'input>),
    Oct(Literal<'input>),
    Bin(Literal<'input>),
}

#[derive(Debug)]
pub enum TypeDefine<'input, 'allocator> {
    Struct(StructDefine<'input, 'allocator>),
    Enum(EnumDefine<'input, 'allocator>),
}

#[derive(Debug)]
pub struct StructDefine<'input, 'allocator> {
    pub name: Literal<'input>,
    pub defines: &'allocator Defines<'input, 'allocator>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct EnumDefine<'input, 'allocator> {
    pub name: Literal<'input>,
    pub elements: Vec<Literal<'input>, &'allocator Bump>,
    pub span: Range<usize>,
}
