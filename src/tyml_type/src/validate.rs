use std::borrow::Cow;

use hashbrown::HashMap;

use crate::types::{NamedTypeMap, TypeTree};

pub struct TypeTreeChecker<'input, 'ty, 'tree, 'map, 'section, 'value, Span> {
    pub type_tree: &'tree TypeTree<'input, 'ty>,
    pub named_type_map: &'map NamedTypeMap<'input, 'ty>,
    pub value_tree: ValueTree<'section, 'value, Span>,
}

pub enum ValueTree<'section, 'value, Span> {
    Node {
        elements: HashMap<Cow<'section, str>, ValueTree<'section, 'value, Span>>,
        span: Span,
    },
    Value {
        value: Value<'value>,
        span: Span,
    },
}

pub enum Value<'value> {
    Int(i64),
    UnsignedInt(u64),
    Float(f64),
    String(Cow<'value, str>),
}

impl<'input, 'ty, 'tree, 'map, 'section, 'value, Span>
    TypeTreeChecker<'input, 'ty, 'tree, 'map, 'section, 'value, Span>
{
    pub fn new(
        tree: &'tree TypeTree<'input, 'ty>,
        named_type_map: &'map NamedTypeMap<'input, 'ty>,
        root_span: Span,
    ) -> Self {
        Self {
            type_tree: tree,
            named_type_map,
            value_tree: ValueTree::Node {
                elements: HashMap::new(),
                span: root_span,
            },
        }
    }
}
