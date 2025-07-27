use std::{borrow::Cow, fmt::Debug, ops::Range};

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
impl_ast!(Define<'_, '_>, enum: Element, Type, Interface);
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
impl_ast!(AttributeOr<'_, '_>, span = self.span);
impl_ast!(AttributeAnd<'_, '_>, span = self.span);
impl_ast!(TypeAttribute<'_, '_>, enum: NumericAttribute, RegexAttribute, AttributeTree);
impl_ast!(NumericAttribute, span = self.span);
impl_ast!(RegexAttribute<'_>, span = self.span);
impl_ast!(Interface<'_, '_>, span = self.span);
impl_ast!(Function<'_, '_>, span = self.span);
impl_ast!(FunctionArgument<'_, '_>, span = self.span);
impl_ast!(Properties<'_, '_>, span = self.span);
impl_ast!(Property<'_, '_>, span = self.span);
impl_ast!(ReturnBlock<'_, '_>, span = self.span);
impl_ast!(ReturnExpression<'_, '_>, span = self.span);
impl_ast!(ReturnType<'_, '_>, span = self.span);
impl_ast!(JsonValue<'_, '_>, enum: Value, Array, Object);
impl_ast!(JsonArray<'_, '_>, span = self.span);
impl_ast!(JsonObject<'_, '_>, span = self.span);

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Range<usize>,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Range<usize>) -> Self {
        Self { value, span }
    }

    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }

    pub fn map<N>(self, f: impl FnOnce(T) -> N) -> Spanned<N> {
        Spanned::new(f(self.value), self.span)
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
    Interface(Interface<'input, 'allocator>),
}

#[derive(Debug)]
pub struct ElementDefine<'input, 'allocator> {
    pub documents: Documents<'input, 'allocator>,
    pub node: NodeLiteral<'input>,
    pub ty: Option<ElementType<'input, 'allocator>>,
    pub inline_type: Option<ElementInlineType<'input, 'allocator>>,
    pub default: Option<DefaultValue<'input>>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct Documents<'input, 'allocator> {
    pub lines: Vec<&'input str, &'allocator Bump>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum NodeLiteral<'input> {
    Literal(EscapedLiteral<'input>),
    Asterisk(Literal<'input>),
}

pub type EscapedLiteral<'input> = Spanned<Cow<'input, str>>;

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
    pub attribute: Option<AttributeOr<'input, 'allocator>>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct AttributeOr<'input, 'allocator> {
    pub attributes: Vec<AttributeAnd<'input, 'allocator>, &'allocator Bump>,
    pub or_spans: Vec<Range<usize>, &'allocator Bump>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct AttributeAnd<'input, 'allocator> {
    pub attributes: Vec<TypeAttribute<'input, 'allocator>, &'allocator Bump>,
    pub and_spans: Vec<Range<usize>, &'allocator Bump>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum TypeAttribute<'input, 'allocator> {
    NumericAttribute(NumericAttribute),
    RegexAttribute(RegexAttribute<'input>),
    AttributeTree(AttributeOr<'input, 'allocator>),
}

#[derive(Debug)]
pub struct NumericAttribute {
    pub kind: Spanned<NumericAttributeKind>,
    pub from_to: FromTo,
    pub span: Range<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumericAttributeKind {
    Value,
    Length,
    U8Size,
}

#[derive(Debug)]
pub enum FromTo {
    /// 0..<10
    FromToExclusive {
        from: Spanned<Either<f64, i128>>,
        to: Spanned<Either<f64, i128>>,
    },
    /// 0..=9
    FromToInclusive {
        from: Spanned<Either<f64, i128>>,
        to: Spanned<Either<f64, i128>>,
    },
    /// 0..
    From { from: Spanned<Either<f64, i128>> },
    /// ..<10
    ToExclusive { to: Spanned<Either<f64, i128>> },
    /// ..=10
    ToInclusive { to: Spanned<Either<f64, i128>> },
}

#[derive(Debug)]
pub struct RegexAttribute<'input> {
    pub regex_keyword_span: Range<usize>,
    pub regex_literal: EscapedLiteral<'input>,
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
    pub documents: Documents<'input, 'allocator>,
    pub keyword_span: Range<usize>,
    pub name: Literal<'input>,
    pub defines: &'allocator Defines<'input, 'allocator>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct EnumDefine<'input, 'allocator> {
    pub documents: Documents<'input, 'allocator>,
    pub keyword_span: Range<usize>,
    pub name: Literal<'input>,
    pub elements: Vec<EnumElement<'input, 'allocator>, &'allocator Bump>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct EnumElement<'input, 'allocator> {
    pub documents: Documents<'input, 'allocator>,
    pub literal: Literal<'input>,
    pub literal_value: Cow<'input, str>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct Properties<'input, 'allocator> {
    pub elements: Vec<Property<'input, 'allocator>, &'allocator Bump>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct Property<'input, 'allocator> {
    pub name: EscapedLiteral<'input>,
    pub values: Vec<ValueLiteral<'input>, &'allocator Bump>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct Interface<'input, 'allocator> {
    pub properties: Properties<'input, 'allocator>,
    pub functions: Vec<Function<'input, 'allocator>, &'allocator Bump>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct Function<'input, 'allocator> {
    pub properties: Properties<'input, 'allocator>,
    pub name: EscapedLiteral<'input>,
    pub arguments: Vec<FunctionArgument<'input, 'allocator>, &'allocator Bump>,
    pub return_type: Option<ReturnType<'input, 'allocator>>,
    pub return_block: Option<ReturnBlock<'input, 'allocator>>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct FunctionArgument<'input, 'allocator> {
    pub properties: Properties<'input, 'allocator>,
    pub name: EscapedLiteral<'input>,
    pub ty: ElementType<'input, 'allocator>,
    pub default_value: Option<JsonValue<'input, 'allocator>>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct ReturnType<'input, 'allocator> {
    pub type_info: OrType<'input, 'allocator>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct ReturnBlock<'input, 'allocator> {
    pub return_expression: ReturnExpression<'input, 'allocator>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct ReturnExpression<'input, 'allocator> {
    pub value: JsonValue<'input, 'allocator>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum JsonValue<'input, 'allocator> {
    Value(ValueLiteral<'input>),
    Array(JsonArray<'input, 'allocator>),
    Object(JsonObject<'input, 'allocator>),
}

#[derive(Debug)]
pub struct JsonArray<'input, 'allocator> {
    pub elements: Vec<JsonValue<'input, 'allocator>, &'allocator Bump>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct JsonObject<'input, 'allocator> {
    pub elements: Vec<JsonObjectElement<'input, 'allocator>, &'allocator Bump>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct JsonObjectElement<'input, 'allocator> {
    pub name: EscapedLiteral<'input>,
    pub value: JsonValue<'input, 'allocator>,
    pub span: Range<usize>,
}
