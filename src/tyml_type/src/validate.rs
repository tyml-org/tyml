use std::borrow::Cow;

use hashbrown::HashMap;

use crate::types::{NamedTypeMap, TypeTree};

pub struct ValueTypeChecker<
    'input,
    'ty,
    'tree,
    'map,
    'section,
    'value,
    Span: PartialEq + Clone + Default,
> {
    pub type_tree: &'tree TypeTree<'input, 'ty>,
    pub named_type_map: &'map NamedTypeMap<'input, 'ty>,
    pub value_tree: ValueTree<'section, 'value, Span>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueTree<'section, 'value, Span: PartialEq + Clone + Default> {
    Section {
        elements: HashMap<Cow<'section, str>, Vec<ValueTree<'section, 'value, Span>>>,
        span: Span,
    },
    Value {
        value: Value<'value>,
        span: Span,
    },
}

impl<Span: PartialEq + Clone + Default> ValueTree<'_, '_, Span> {
    pub fn span(&self) -> &Span {
        match self {
            ValueTree::Section { elements: _, span } => span,
            ValueTree::Value { value: _, span } => span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'value> {
    Int(i64),
    UnsignedInt(u64),
    Float(f64),
    String(Cow<'value, str>),
    None,
}

impl<'input, 'ty, 'tree, 'map, 'section, 'value, Span: PartialEq + Clone + Default>
    ValueTypeChecker<'input, 'ty, 'tree, 'map, 'section, 'value, Span>
{
    pub fn new(
        tree: &'tree TypeTree<'input, 'ty>,
        named_type_map: &'map NamedTypeMap<'input, 'ty>,
    ) -> Self {
        Self {
            type_tree: tree,
            named_type_map,
            value_tree: ValueTree::Section {
                elements: HashMap::new(),
                span: Default::default(),
            },
        }
    }

    pub fn set_value(
        &mut self,
        sections: impl Iterator<Item = (impl Into<Cow<'section, str>>, Span)>,
        value: ValueTree<'section, 'value, Span>,
    ) {
        let root_section = [(Cow::Borrowed("root"), self.value_tree.span().clone())];
        // Iterator<(Into<Cow>, Span)> => Iterator<(Cow, Span)>
        let sections = sections.map(|(section, span)| (section.into(), span));

        // add root section on the head of sections iterator
        let mut sections = root_section.into_iter().chain(sections).peekable();
        let mut value_tree = &mut self.value_tree;

        loop {
            value_tree = match value_tree {
                ValueTree::Section { elements, span: _ } => {
                    let (current_section, current_section_span) = sections.next().unwrap();

                    let element_branches =
                        elements.entry(current_section.into()).or_insert(Vec::new());

                    // search same section that already exists
                    let next_branch_position =
                        element_branches.iter_mut().position(|branch| match branch {
                            ValueTree::Section { elements: _, span } => {
                                span == &current_section_span
                            }
                            ValueTree::Value { value: _, span: _ } => false,
                        });

                    let matched_section_branch = match next_branch_position {
                        Some(next_branch_position) => &mut element_branches[next_branch_position],
                        None => {
                            element_branches.push(ValueTree::Section {
                                elements: HashMap::new(),
                                span: current_section_span,
                            });
                            element_branches.last_mut().unwrap()
                        }
                    };

                    // if last section
                    if sections.peek().is_none() {
                        *matched_section_branch = value;
                        return;
                    }

                    matched_section_branch
                }
                ValueTree::Value { value: _, span: _ } => {
                    // It is unreachable, because the value has been validated in up layer section.
                    unreachable!()
                }
            };
        }
    }
}
