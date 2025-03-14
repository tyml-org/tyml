use types::{NamedTypeMap, Type, TypeTree};

pub mod error;
pub mod name;
pub mod resolver;
pub mod types;

pub struct TypeAccessor<'input, 'ty, 'tree, 'map> {
    pub tree: &'tree TypeTree<'input, 'ty>,
    pub named_type_map: &'map NamedTypeMap<'input, 'ty>,
}

impl<'input, 'ty, 'tree, 'map> TypeAccessor<'input, 'ty, 'tree, 'map> {
    
}
