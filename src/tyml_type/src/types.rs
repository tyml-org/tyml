use std::{
    ops::{Range, RangeFrom, RangeInclusive, RangeTo, RangeToInclusive},
    sync::Arc,
};

#[derive(Debug, Clone)]
pub enum ProtoType {
    Int(IntAttribute),
    UnsignedInt(UnsignedIntAttribute),
    String(StringAttribute),
    UserType(UserTypeID),
}

#[derive(Debug, Clone, Default)]
pub struct IntAttribute {
    pub range: IntRange<i64>,
}

#[derive(Debug, Clone, Default)]
pub struct UnsignedIntAttribute {
    pub range: IntRange<u64>,
}

#[derive(Debug, Clone, Default)]
pub enum IntRange<T> {
    Range(Range<T>),
    RangeInclusive(RangeInclusive<T>),
    RangeFrom(RangeFrom<T>),
    RangeTo(RangeTo<T>),
    RangeToInclusive(RangeToInclusive<T>),
    #[default]
    None,
}

#[derive(Debug, Clone, Default)]
pub struct StringAttribute {
    pub tags: Arc<Vec<String>>,
}

pub struct UserType {}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct UserTypeID(usize);
