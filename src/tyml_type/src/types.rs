use std::{
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
    /// accept if `self ⊃ other`
    pub fn try_override_with(
        &self,
        other: &Type<'ty>,
        allocator: &'ty Bump,
    ) -> Result<Type<'ty>, ()> {
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
}

pub trait Attribute<T> {
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

#[derive(Debug, Clone, Default, PartialEq)]
pub struct UnsignedIntAttribute {
    pub range: NumericalValueRange<u64>,
}

impl Attribute<u64> for UnsignedIntAttribute {
    fn validate(&self, value: u64) -> bool {
        self.range.validate(value)
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub enum NumericalValueRange<T> {
    Range(Range<T>),
    RangeInclusive(RangeInclusive<T>),
    RangeFrom(RangeFrom<T>),
    RangeTo(RangeTo<T>),
    RangeToInclusive(RangeToInclusive<T>),
    #[default]
    None,
}

impl<T: PartialOrd> Attribute<T> for NumericalValueRange<T> {
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

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct StringAttribute {
    pub tags: Arc<Vec<String>>,
}

impl Attribute<&str> for StringAttribute {
    fn validate(&self, _: &str) -> bool {
        true // TODO : impl attrivute
    }
}

#[derive(Debug)]
pub enum TypeTree<'input, 'ty> {
    Node {
        node: HashMap<&'input str, TypeTree<'input, 'ty>, DefaultHashBuilder, &'ty Bump>,
        any_node: Option<Box<TypeTree<'input, 'ty>, &'ty Bump>>,
    },
    Leaf {
        ty: Type<'ty>,
    },
}

#[derive(Debug)]
pub enum NamedType<'input, 'ty> {
    Struct {
        tree: TypeTree<'input, 'ty>,
    },
    Enum {
        elements: Vec<Literal<'input>, &'ty Bump>,
    },
}

#[derive(Debug)]
pub struct NamedTypeMap<'input, 'ty> {
    map: HashMap<NameID, (Range<usize>, NamedType<'input, 'ty>), DefaultHashBuilder, &'ty Bump>,
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
        name_span: Range<usize>,
        type_tree: NamedType<'input, 'ty>,
    ) {
        self.map.insert(name_id, (name_span, type_tree));
    }

    pub fn get_define_span(&self, name_id: NameID) -> Option<Range<usize>> {
        self.map.get(&name_id).map(|(span, _)| span.clone())
    }

    pub fn get_type(&self, name_id: NameID) -> Option<&NamedType<'input, 'ty>> {
        self.map.get(&name_id).map(|(_, ty)| ty)
    }
}
