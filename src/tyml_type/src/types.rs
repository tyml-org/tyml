use std::{
    borrow::Cow,
    fmt::Display,
    ops::{Range, RangeFrom, RangeInclusive, RangeTo, RangeToInclusive},
    sync::Arc,
};

use allocator_api2::{boxed::Box, vec::Vec};
use bumpalo::Bump;
use hashbrown::{DefaultHashBuilder, HashMap};
use regex::Regex;
use tyml_parser::ast::{EscapedLiteral, JsonValue, Spanned};

use crate::{name::NameID, resolver::JsonTreeTypeCache};

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'ty> {
    Int(IntAttribute),
    UnsignedInt(UnsignedIntAttribute),
    Float(FloatAttribute),
    Bool,
    String(StringAttribute),
    MaybeInt,
    MaybeUnsignedInt,
    Named(NameID),
    Or(Vec<Type<'ty>, &'ty Bump>),
    Array(Box<Type<'ty>, &'ty Bump>),
    Optional(Box<Type<'ty>, &'ty Bump>),
    /// This is devil
    Any,
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub enum AttributeTree {
    Or {
        attributes: Vec<AttributeTree>,
    },
    And {
        attributes: Vec<AttributeTree>,
    },
    Tree {
        attribute: Arc<AttributeTree>,
    },
    Base {
        attribute: Arc<AttributeSet>,
    },
    #[default]
    None,
}

impl AttributeTree {
    pub fn validate_as_int_value(&self, value: i64) -> bool {
        match self {
            AttributeTree::Or { attributes } => attributes
                .iter()
                .any(|attribute| attribute.validate_as_int_value(value)),
            AttributeTree::And { attributes } => attributes
                .iter()
                .all(|attribute| attribute.validate_as_int_value(value)),
            AttributeTree::Tree { attribute } => attribute.validate_as_int_value(value),
            AttributeTree::Base { attribute } => match attribute.as_ref() {
                AttributeSet::IntValue(attribute) => attribute.validate(value),
                _ => true,
            },
            AttributeTree::None => true,
        }
    }

    pub fn validate_as_uint_value(&self, value: u64) -> bool {
        match self {
            AttributeTree::Or { attributes } => attributes
                .iter()
                .any(|attribute| attribute.validate_as_uint_value(value)),
            AttributeTree::And { attributes } => attributes
                .iter()
                .all(|attribute| attribute.validate_as_uint_value(value)),
            AttributeTree::Tree { attribute } => attribute.validate_as_uint_value(value),
            AttributeTree::Base { attribute } => match attribute.as_ref() {
                AttributeSet::UIntValue(attribute) => attribute.validate(value),
                _ => true,
            },
            AttributeTree::None => true,
        }
    }

    pub fn validate_as_float_value(&self, value: f64) -> bool {
        match self {
            AttributeTree::Or { attributes } => attributes
                .iter()
                .any(|attribute| attribute.validate_as_float_value(value)),
            AttributeTree::And { attributes } => attributes
                .iter()
                .all(|attribute| attribute.validate_as_float_value(value)),
            AttributeTree::Tree { attribute } => attribute.validate_as_float_value(value),
            AttributeTree::Base { attribute } => match attribute.as_ref() {
                AttributeSet::FloatValue(attribute) => attribute.validate(value),
                _ => true,
            },
            AttributeTree::None => true,
        }
    }

    pub fn validate_as_string_value(&self, value: &str) -> bool {
        match self {
            AttributeTree::Or { attributes } => attributes
                .iter()
                .any(|attribute| attribute.validate_as_string_value(value)),
            AttributeTree::And { attributes } => attributes
                .iter()
                .all(|attribute| attribute.validate_as_string_value(value)),
            AttributeTree::Tree { attribute } => attribute.validate_as_string_value(value),
            AttributeTree::Base { attribute } => match attribute.as_ref() {
                AttributeSet::Length(attribute) => attribute.validate(value.chars().count() as _),
                AttributeSet::U8Size(attribute) => attribute.validate(value.len() as _),
                AttributeSet::Regex(attribute) => attribute.is_match(value),
                _ => true,
            },
            AttributeTree::None => true,
        }
    }
}

#[derive(Debug, Clone)]
pub enum AttributeSet {
    Length(NumericalValueRange<u64>),
    U8Size(NumericalValueRange<u64>),
    IntValue(NumericalValueRange<i64>),
    UIntValue(NumericalValueRange<u64>),
    FloatValue(NumericalValueRange<f64>),
    Regex(Arc<Regex>),
}

impl PartialEq for AttributeSet {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl ToTypeName for AttributeTree {
    fn to_type_name(&self, named_type_map: &NamedTypeMap) -> String {
        match self {
            AttributeTree::Or { attributes } => attributes
                .iter()
                .map(|attribute| attribute.to_type_name(named_type_map))
                .collect::<Vec<_>>()
                .join(" or"),
            AttributeTree::And { attributes } => attributes
                .iter()
                .map(|attribute| attribute.to_type_name(named_type_map))
                .collect::<Vec<_>>()
                .join(" and"),
            AttributeTree::Tree {
                attribute: attributes,
            } => {
                format!(" ({} )", attributes.to_type_name(named_type_map))
            }
            AttributeTree::Base { attribute } => attribute.to_type_name(named_type_map),
            AttributeTree::None => String::new(),
        }
    }
}

impl ToTypeName for AttributeSet {
    fn to_type_name(&self, named_type_map: &NamedTypeMap) -> String {
        match self {
            AttributeSet::Length(attribute) => {
                format!(" @length {}", attribute.to_type_name(named_type_map))
            }
            AttributeSet::U8Size(attribute) => {
                format!(" @u8size {}", attribute.to_type_name(named_type_map))
            }
            AttributeSet::IntValue(attribute) => {
                format!(" @value {}", attribute.to_type_name(named_type_map))
            }
            AttributeSet::UIntValue(attribute) => {
                format!(" @value {}", attribute.to_type_name(named_type_map))
            }
            AttributeSet::FloatValue(attribute) => {
                format!(" @value {}", attribute.to_type_name(named_type_map))
            }
            AttributeSet::Regex(regex) => format!(" @regex r\"{}\"", regex.as_str()),
        }
    }
}

impl<'ty> Type<'ty> {
    /// accept if `self ⊃ other`, but exceptions for optional
    pub(crate) fn try_override_with(
        &self,
        other: &Type<'ty>,
        allocator: &'ty Bump,
    ) -> Result<Type<'ty>, ()> {
        // exceptions for optional
        if let Type::Optional(other_type) = other {
            if let Type::Optional(_) = self {
            } else {
                // If other is optional and self is NOT optional,
                // validate self as optional.
                // We must accept, pattern of "node: int? = 100"
                return Ok(Type::Optional(Box::new_in(
                    self.try_override_with(&other_type, allocator)?,
                    allocator,
                )));
            }
        }

        match self {
            Type::MaybeInt => match other {
                Type::Int(_) | Type::Float(_) => Ok(other.clone()),
                _ => Err(()),
            },
            Type::MaybeUnsignedInt => match other {
                Type::Int(_) | Type::UnsignedInt(_) | Type::Float(_) => Ok(other.clone()),
                _ => Err(()),
            },
            Type::Or(self_types) => match other {
                Type::Or(other_types) => {
                    let mut self_types = self_types.clone();

                    'root: for other_type in other_types.iter() {
                        for self_type in self_types.iter_mut() {
                            if let Ok(new_type) = self_type.try_override_with(other_type, allocator)
                            {
                                *self_type = new_type;
                                continue 'root;
                            }
                        }
                        return Err(());
                    }

                    Ok(Type::Or(self_types))
                }
                _ => {
                    let mut self_types = self_types.clone();

                    for self_type in self_types.iter_mut() {
                        if let Ok(new_type) = self_type.try_override_with(other, allocator) {
                            *self_type = new_type;
                            return Ok(Type::Or(self_types));
                        }
                    }

                    Err(())
                }
            },
            Type::Array(self_type) => match other {
                Type::Array(other_type) => Ok(Type::Array(Box::new_in(
                    self_type.try_override_with(&other_type, allocator)?,
                    allocator,
                ))),
                _ => Err(()),
            },
            Type::Optional(self_type) => match other {
                Type::Optional(other_type) => Ok(Type::Optional(Box::new_in(
                    self_type.try_override_with(other_type, allocator)?,
                    allocator,
                ))),
                _ => Err(()),
            },
            Type::Unknown => Ok(other.clone()),
            _ => match self == other {
                true => Ok(other.clone()),
                false => Err(()),
            },
        }
    }

    /// mainly, check the range
    pub fn validate_value_with_attribute(&self, value: &str) -> bool {
        match self {
            Type::Int(attribute) => value
                .parse::<i64>()
                .map(|value| attribute.validate(value))
                .unwrap_or(false),
            Type::UnsignedInt(attribute) => value
                .parse::<u64>()
                .map(|value| attribute.validate(value))
                .unwrap_or(false),
            Type::Float(attribute) => value
                .parse::<f64>()
                .map(|value| attribute.validate(value))
                .unwrap_or(false),
            Type::Bool => value.to_ascii_lowercase().parse::<bool>().is_ok(),
            Type::String(attribute) => attribute.validate(value),
            Type::MaybeInt => value.parse::<i64>().is_ok(),
            Type::MaybeUnsignedInt => value.parse::<u64>().is_ok(),
            Type::Named(_) => false,
            Type::Or(or_types) => or_types
                .iter()
                .any(|ty| ty.validate_value_with_attribute(value)),
            Type::Array(base_type) | Type::Optional(base_type) => {
                base_type.validate_value_with_attribute(value)
            }
            Type::Any => true,
            Type::Unknown => false,
        }
    }

    pub fn is_allowed_optional(&self) -> bool {
        match self {
            Type::Or(or_types) => or_types.iter().any(|ty| ty.is_allowed_optional()),
            Type::Optional(_) => true,
            _ => false,
        }
    }
}

pub trait ToTypeName {
    fn to_type_name(&self, named_type_map: &NamedTypeMap) -> String;
}

impl<'ty> ToTypeName for Type<'ty> {
    fn to_type_name(&self, named_type_map: &NamedTypeMap) -> String {
        match self {
            Type::Int(attribute) => format!("int{}", attribute.to_type_name(named_type_map)),
            Type::UnsignedInt(attribute) => {
                format!("uint{}", attribute.to_type_name(named_type_map))
            }
            Type::Float(attribute) => format!("float{}", attribute.to_type_name(named_type_map)),
            Type::Bool => "bool".into(),
            Type::String(attribute) => format!("string{}", attribute.to_type_name(named_type_map)),
            Type::MaybeInt => "int(maybe)".into(),
            Type::MaybeUnsignedInt => "uint(maybe)".into(),
            Type::Named(name_id) => named_type_map
                .get_name(*name_id)
                .unwrap_or("unknown")
                .to_string(),
            Type::Or(items) => items
                .iter()
                .map(|item| item.to_type_name(named_type_map))
                .collect::<Vec<_>>()
                .join(" | "),
            Type::Array(base) => format!("[{}]", base.to_type_name(named_type_map)),
            Type::Optional(base) => format!("{}?", base.to_type_name(named_type_map)),
            Type::Any => "any".into(),
            Type::Unknown => "unknown".into(),
        }
    }
}

pub trait Attribute<T>: ToTypeName {
    fn validate(&self, value: T) -> bool;
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct IntAttribute {
    pub range: AttributeTree,
}

impl Attribute<i64> for IntAttribute {
    fn validate(&self, value: i64) -> bool {
        self.range.validate_as_int_value(value)
    }
}

impl ToTypeName for IntAttribute {
    fn to_type_name(&self, named_type_map: &NamedTypeMap) -> String {
        self.range.to_type_name(named_type_map)
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct UnsignedIntAttribute {
    pub range: AttributeTree,
}

impl Attribute<u64> for UnsignedIntAttribute {
    fn validate(&self, value: u64) -> bool {
        self.range.validate_as_uint_value(value)
    }
}

impl ToTypeName for UnsignedIntAttribute {
    fn to_type_name(&self, named_type_map: &NamedTypeMap) -> String {
        self.range.to_type_name(named_type_map)
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub enum NumericalValueRange<T: Display> {
    Range(Range<T>),
    RangeInclusive(RangeInclusive<T>),
    RangeFrom(RangeFrom<T>),
    RangeTo(RangeTo<T>),
    RangeToInclusive(RangeToInclusive<T>),
    #[default]
    None,
}

impl<T: Display> ToTypeName for NumericalValueRange<T> {
    fn to_type_name(&self, _: &NamedTypeMap) -> String {
        match self {
            NumericalValueRange::Range(range) => format!("{}..<{}", range.start, range.end),
            NumericalValueRange::RangeInclusive(range) => {
                format!("{}..={}", range.start(), range.end())
            }
            NumericalValueRange::RangeFrom(range) => format!("{}..", range.start),
            NumericalValueRange::RangeTo(range) => format!("..<{}", range.end),
            NumericalValueRange::RangeToInclusive(range) => format!("..={}", range.end),
            NumericalValueRange::None => String::new(),
        }
    }
}

impl<T: PartialOrd + Display> Attribute<T> for NumericalValueRange<T> {
    fn validate(&self, value: T) -> bool {
        match self {
            NumericalValueRange::Range(range) => range.contains(&value),
            NumericalValueRange::RangeInclusive(range) => range.contains(&value),
            NumericalValueRange::RangeFrom(range) => range.contains(&value),
            NumericalValueRange::RangeTo(range) => range.contains(&value),
            NumericalValueRange::RangeToInclusive(range) => range.contains(&value),
            NumericalValueRange::None => true,
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct FloatAttribute {
    pub range: AttributeTree,
}

impl Attribute<f64> for FloatAttribute {
    fn validate(&self, value: f64) -> bool {
        self.range.validate_as_float_value(value)
    }
}

impl ToTypeName for FloatAttribute {
    fn to_type_name(&self, named_type_map: &NamedTypeMap) -> String {
        self.range.to_type_name(named_type_map)
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct StringAttribute {
    pub attribute: AttributeTree,
}

impl Attribute<&str> for StringAttribute {
    fn validate(&self, value: &str) -> bool {
        self.attribute.validate_as_string_value(value)
    }
}

impl ToTypeName for StringAttribute {
    fn to_type_name(&self, named_type_map: &NamedTypeMap) -> String {
        self.attribute.to_type_name(named_type_map)
    }
}

#[derive(Debug)]
pub struct InterfaceInfo<'input, 'ty, 'ast_allocator> {
    pub documents: Vec<&'input str, &'ast_allocator Bump>,
    pub keyword_span: Range<usize>,
    pub name: Spanned<String>,
    pub original_name: &'input str,
    pub functions: Vec<FunctionInfo<'input, 'ty, 'ast_allocator>, &'ast_allocator Bump>,
    pub json_tree_type_cache: JsonTreeTypeCache<'input, 'ast_allocator>,
}

#[derive(Debug)]
pub struct FunctionInfo<'input, 'ty, 'ast_allocator> {
    pub documents: Vec<&'input str, &'ast_allocator Bump>,
    pub keyword_span: Range<usize>,
    pub authed: Option<Range<usize>>,
    pub name: Spanned<String>,
    pub kind: FunctionKind,
    pub arguments: Vec<FunctionArgumentInfo<'input, 'ty, 'ast_allocator>, &'ty Bump>,
    pub body_argument_info: Option<FunctionBodyArgumentInfo<'input, 'ty, 'ast_allocator>>,
    pub claim_argument_info: Option<AuthClaimArgumentInfo<'input, 'ty, 'ast_allocator>>,
    pub return_info: Option<FunctionReturnInfo<'input, 'ty, 'ast_allocator>>,
    pub throws_type: Option<Type<'ty>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum FunctionKind {
    GET,
    PUT,
    #[default]
    POST,
    PATCH,
    DELETE,
}

#[derive(Debug)]
pub struct FunctionArgumentInfo<'input, 'ty, 'ast_allocator> {
    pub name: EscapedLiteral<'input>,
    pub ty: Type<'ty>,
    pub default_value: Option<&'ast_allocator JsonValue<'input, 'ast_allocator>>,
}

#[derive(Debug)]
pub struct FunctionBodyArgumentInfo<'input, 'ty, 'ast_allocator> {
    pub name: Range<usize>,
    pub ty: Type<'ty>,
    pub default_value: Option<&'ast_allocator JsonValue<'input, 'ast_allocator>>,
}

#[derive(Debug)]
pub struct AuthClaimArgumentInfo<'input, 'ty, 'ast_allocator> {
    pub name: Range<usize>,
    pub ty: Type<'ty>,
    pub default_value: Option<&'ast_allocator JsonValue<'input, 'ast_allocator>>,
}

#[derive(Debug)]
pub struct FunctionReturnInfo<'input, 'ty, 'ast_allocator> {
    pub ty: Type<'ty>,
    pub default_value: Option<&'ast_allocator JsonValue<'input, 'ast_allocator>>,
}

#[derive(Debug)]
pub enum TypeTree<'input, 'ty> {
    Node {
        node: HashMap<Cow<'input, str>, TypeTree<'input, 'ty>, DefaultHashBuilder, &'ty Bump>,
        any_node: Option<Box<TypeTree<'input, 'ty>, &'ty Bump>>,
        node_key_span: HashMap<Cow<'input, str>, Range<usize>, DefaultHashBuilder, &'ty Bump>,
        any_node_key_span: Option<Range<usize>>,
        documents: Vec<&'input str, &'ty Bump>,
        span: Range<usize>,
    },
    Leaf {
        ty: Type<'ty>,
        documents: Vec<&'input str, &'ty Bump>,
        span: Range<usize>,
    },
}

impl TypeTree<'_, '_> {
    pub fn is_allowed_optional(&self) -> bool {
        match self {
            TypeTree::Node {
                node: _,
                any_node: _,
                node_key_span: _,
                any_node_key_span: _,
                documents: _,
                span: _,
            } => false,
            TypeTree::Leaf {
                ty,
                documents: _,
                span: _,
            } => ty.is_allowed_optional(),
        }
    }

    pub fn span(&self) -> Range<usize> {
        match self {
            TypeTree::Node {
                node: _,
                any_node: _,
                node_key_span: _,
                any_node_key_span: _,
                documents: _,
                span,
            } => span.clone(),
            TypeTree::Leaf {
                ty: _,
                documents: _,
                span,
            } => span.clone(),
        }
    }

    pub fn documents(&self) -> &Vec<&str, &Bump> {
        match self {
            TypeTree::Node {
                node: _,
                any_node: _,
                node_key_span: _,
                any_node_key_span: _,
                documents,
                span: _,
            } => documents,
            TypeTree::Leaf {
                ty: _,
                documents,
                span: _,
            } => documents,
        }
    }
}

#[derive(Debug)]
pub enum NamedTypeTree<'input, 'ty> {
    Struct {
        tree: TypeTree<'input, 'ty>,
    },
    Enum {
        elements: Vec<(EscapedLiteral<'input>, Vec<&'input str, &'ty Bump>), &'ty Bump>,
        documents: Vec<&'input str, &'ty Bump>,
    },
}

#[derive(Debug)]
pub struct NamedTypeMap<'input, 'ty> {
    ty: &'ty Bump,
    pub map: HashMap<
        NameID,
        (&'input str, Range<usize>, NamedTypeTree<'input, 'ty>),
        DefaultHashBuilder,
        &'ty Bump,
    >,
    // for lsp
    pub use_link_map: HashMap<NameID, Vec<Range<usize>, &'ty Bump>, DefaultHashBuilder, &'ty Bump>,
}

impl<'input, 'ty> NamedTypeMap<'input, 'ty> {
    pub fn new(ty: &'ty Bump) -> Self {
        Self {
            ty,
            map: HashMap::new_in(ty),
            use_link_map: HashMap::new_in(ty),
        }
    }

    pub fn register(
        &mut self,
        name_id: NameID,
        name: &'input str,
        name_span: Range<usize>,
        type_tree: NamedTypeTree<'input, 'ty>,
    ) {
        self.map
            .insert(name_id, (name, name_span.clone(), type_tree));
        self.use_link_map
            .entry(name_id)
            .or_insert_with(|| Vec::new_in(self.ty))
            .push(name_span);
    }

    pub fn get_name(&self, name_id: NameID) -> Option<&'input str> {
        self.map.get(&name_id).map(|(name, _, _)| *name)
    }

    pub fn get_define_span(&self, name_id: NameID) -> Option<Range<usize>> {
        self.map.get(&name_id).map(|(_, span, _)| span.clone())
    }

    pub fn get_type(&self, name_id: NameID) -> Option<&NamedTypeTree<'input, 'ty>> {
        self.map.get(&name_id).map(|(_, _, ty)| ty)
    }

    pub fn link(&mut self, name_id: NameID, user_range: Range<usize>) {
        let users = self
            .use_link_map
            .entry(name_id)
            .or_insert_with(|| Vec::new_in(self.ty));

        users.push(user_range);
    }
}
