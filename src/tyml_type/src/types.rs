use std::{
    fmt::Display,
    ops::{Range, RangeFrom, RangeInclusive, RangeTo, RangeToInclusive},
    sync::Arc,
};

use allocator_api2::{boxed::Box, vec::Vec};
use bumpalo::Bump;
use hashbrown::{DefaultHashBuilder, HashMap};
use tyml_parser::ast::Literal;

use crate::name::NameID;

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'ty> {
    Int(IntAttribute),
    UnsignedInt(UnsignedIntAttribute),
    Float(FloatAttribute),
    String(StringAttribute),
    MaybeInt,
    MaybeUnsignedInt,
    Named(NameID),
    Or(Vec<Type<'ty>, &'ty Bump>),
    Array(Box<Type<'ty>, &'ty Bump>),
    Optional(Box<Type<'ty>, &'ty Bump>),
    Unknown,
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
            Type::String(attribute) => format!("string{}", attribute.to_type_name(named_type_map)),
            Type::MaybeInt => format!("int(maybe)"),
            Type::MaybeUnsignedInt => format!("uint(maybe)"),
            Type::Named(name_id) => named_type_map.get_name(*name_id).unwrap().to_string(),
            Type::Or(items) => items
                .iter()
                .map(|item| item.to_type_name(named_type_map))
                .collect::<Vec<_>>()
                .join(" | "),
            Type::Array(base) => format!("[{}]", base.to_type_name(named_type_map)),
            Type::Optional(base) => format!("{}?", base.to_type_name(named_type_map)),
            Type::Unknown => "unknown".to_string(),
        }
    }
}

pub trait Attribute<T>: ToTypeName {
    fn validate(&self, value: T) -> bool;
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct IntAttribute {
    pub range: NumericalValueRange<i64>,
}

impl Attribute<i64> for IntAttribute {
    fn validate(&self, value: i64) -> bool {
        self.range.validate(value)
    }
}

impl ToTypeName for IntAttribute {
    fn to_type_name(&self, named_type_map: &NamedTypeMap) -> String {
        self.range.to_type_name(named_type_map)
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct UnsignedIntAttribute {
    pub range: NumericalValueRange<u64>,
}

impl Attribute<u64> for UnsignedIntAttribute {
    fn validate(&self, value: u64) -> bool {
        self.range.validate(value)
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
            NumericalValueRange::Range(range) => format!(" < {}..{}", range.start, range.end),
            NumericalValueRange::RangeInclusive(range) => {
                format!(" < {}..={}", range.start(), range.end())
            }
            NumericalValueRange::RangeFrom(range) => format!(" < {}..", range.start),
            NumericalValueRange::RangeTo(range) => format!(" < ..{}", range.end),
            NumericalValueRange::RangeToInclusive(range) => format!(" < ..={}", range.end),
            NumericalValueRange::None => format!(""),
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
    pub range: NumericalValueRange<f64>,
}

impl Attribute<f64> for FloatAttribute {
    fn validate(&self, value: f64) -> bool {
        self.range.validate(value)
    }
}

impl ToTypeName for FloatAttribute {
    fn to_type_name(&self, named_type_map: &NamedTypeMap) -> String {
        self.range.to_type_name(named_type_map)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct StringAttribute {
    pub tags: Arc<Vec<String>>,
}

impl Attribute<&str> for StringAttribute {
    fn validate(&self, _: &str) -> bool {
        true // TODO : impl attrivute
    }
}

impl ToTypeName for StringAttribute {
    fn to_type_name(&self, _: &NamedTypeMap) -> String {
        format!("")
    }
}

#[derive(Debug)]
pub enum TypeTree<'input, 'ty> {
    Node {
        node: HashMap<&'input str, TypeTree<'input, 'ty>, DefaultHashBuilder, &'ty Bump>,
        any_node: Option<Box<TypeTree<'input, 'ty>, &'ty Bump>>,
        span: Range<usize>,
    },
    Leaf {
        ty: Type<'ty>,
        span: Range<usize>,
    },
}

impl TypeTree<'_, '_> {
    pub fn is_allowed_optional(&self) -> bool {
        match self {
            TypeTree::Node {
                node: _,
                any_node: _,
                span: _,
            } => false,
            TypeTree::Leaf { ty, span: _ } => ty.is_allowed_optional(),
        }
    }

    pub fn span(&self) -> Range<usize> {
        match self {
            TypeTree::Node {
                node: _,
                any_node: _,
                span,
            } => span.clone(),
            TypeTree::Leaf { ty: _, span } => span.clone(),
        }
    }
}

#[derive(Debug)]
pub enum NamedTypeTree<'input, 'ty> {
    Struct {
        tree: TypeTree<'input, 'ty>,
    },
    Enum {
        elements: Vec<Literal<'input>, &'ty Bump>,
    },
}

#[derive(Debug)]
pub struct NamedTypeMap<'input, 'ty> {
    map: HashMap<
        NameID,
        (&'input str, Range<usize>, NamedTypeTree<'input, 'ty>),
        DefaultHashBuilder,
        &'ty Bump,
    >,
}

impl<'input, 'ty> NamedTypeMap<'input, 'ty> {
    pub fn new(ty: &'ty Bump) -> Self {
        Self {
            map: HashMap::new_in(ty),
        }
    }

    pub fn link(
        &mut self,
        name_id: NameID,
        name: &'input str,
        name_span: Range<usize>,
        type_tree: NamedTypeTree<'input, 'ty>,
    ) {
        self.map.insert(name_id, (name, name_span, type_tree));
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
}
