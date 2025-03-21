use std::borrow::Cow;

use bumpalo::Bump;
use hashbrown::{DefaultHashBuilder, HashMap};

use tyml_parser::ast::Spanned;
use tyml_type::types::{NamedTypeMap, TypeTree};

use crate::error::TymlValueValidateError;

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

    pub fn validate(&self) -> Result<(), Vec<TymlValueValidateError<Span>>> {
        let mut errors = Vec::new();

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn validate_inner<'temp>(
        errors: &mut Vec<TymlValueValidateError<Span>>,
        type_tree: &'tree TypeTree<'input, 'ty>,
        value_tree: &ValueTree<'section, 'value, Span>,
        section_name_stack: &mut allocator_api2::vec::Vec<&'input str, &'temp Bump>,
    ) {
        match type_tree {
            TypeTree::Node {
                node,
                any_node,
                span: type_tree_span,
            } => {
                for (element_name, element_type) in node.iter() {
                    match value_tree {
                        ValueTree::Section {
                            elements,
                            span: value_section_span,
                        } => match elements.get(*element_name) {
                            Some(values) => todo!(),
                            None => {
                                if !element_type.is_allowed_optional() {
                                    let error = TymlValueValidateError::NoValueFound {
                                        required: Spanned::new(
                                            section_name_stack
                                                .iter()
                                                .chain([element_name].into_iter())
                                                .map(|name| name.to_string())
                                                .collect(),
                                            type_tree_span.clone(),
                                        ),
                                        required_in_value_section: todo!(),
                                    };
                                }
                            }
                        },
                        ValueTree::Value { value, span } => todo!(),
                    }
                }
            }
            TypeTree::Leaf { ty, span } => todo!(),
        }
    }
}

pub enum MergedValueTree<'section, 'value, 'temp, Span: Clone> {
    Section {
        elements: HashMap<
            Cow<'section, str>,
            MergedValueTree<'section, 'value, 'temp, Span>,
            DefaultHashBuilder,
            &'temp Bump,
        >,
        span: Span,
    },
    Value {
        value: Value<'value>,
        span: Span,
    },
}

impl<'section, 'value, 'temp, Span: PartialEq + Clone + Default>
    MergedValueTree<'section, 'value, 'temp, Span>
{
    pub fn merge_and_collect_duplicated(
        &mut self,
        value_tree: &ValueTree<'section, 'value, Span>,
        errors: Vec<TymlValueValidateError<Span>>,
    ) {
        match value_tree {
            ValueTree::Section { elements, span } => {
                for (element_name, values) in elements.iter() {
                    for value_tree in values {
                        
                    }
                }
            },
            ValueTree::Value { value, span } => todo!(),
        }
    }
}
