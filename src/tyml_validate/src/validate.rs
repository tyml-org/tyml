use std::{borrow::Cow, ops::Not};

use allocator_api2::vec;
use allocator_api2::vec::Vec;
use auto_enums::auto_enum;
use bumpalo::Bump;
use hashbrown::{DefaultHashBuilder, HashMap};

use tyml_parser::ast::Spanned;
use tyml_type::types::{Attribute, NamedTypeMap, NamedTypeTree, ToTypeName, Type, TypeTree};

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
    value_tree: ValueTree<'section, 'value, Span>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueTree<'section, 'value, Span: PartialEq + Clone + Default> {
    Section {
        elements: HashMap<Cow<'section, str>, Vec<ValueTree<'section, 'value, Span>>>,
        span: Span,
    },
    Value {
        value: Value<'section, 'value, Span>,
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
pub enum Value<'section, 'value, Span: PartialEq + Clone + Default> {
    Int(i64),
    UnsignedInt(u64),
    Float(f64),
    String(Cow<'value, str>),
    AnyString(Cow<'value, str>),
    Tree(Box<ValueTree<'section, 'value, Span>>),
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

    fn validate_tree<'temp>(
        &self,
        errors: &mut Option<Vec<TymlValueValidateError<Span>>>,
        type_tree: &'tree TypeTree<'input, 'ty>,
        value_tree: &MergedValueTree<'section, 'value, 'temp, Span>,
        section_name_stack: &mut allocator_api2::vec::Vec<&'input str, &'temp Bump>,
    ) -> bool {
        match type_tree {
            TypeTree::Node {
                node,
                any_node,
                span: _,
            } => {
                for (element_name, element_type) in node.iter() {
                    match value_tree {
                        MergedValueTree::Section {
                            elements,
                            spans: value_tree_section_spans,
                        } => {
                            match elements.get(*element_name) {
                                Some(element_value) => {
                                    section_name_stack.push(*element_name);

                                    let result = self.validate_tree(
                                        errors,
                                        element_type,
                                        element_value,
                                        section_name_stack,
                                    );

                                    if !result && errors.is_none() {
                                        return false;
                                    }

                                    section_name_stack.pop().unwrap();
                                }
                                None => {
                                    if !element_type.is_allowed_optional() {
                                        if let Some(errors) = errors {
                                            let error = TymlValueValidateError::NoValueFound {
                                                required: Spanned::new(
                                                    section_name_stack
                                                        .iter()
                                                        .chain([element_name].into_iter())
                                                        .map(|name| name.to_string())
                                                        .collect::<Vec<_>>()
                                                        .join("."),
                                                    element_type.span(),
                                                ),
                                                required_in: value_tree_section_spans
                                                    .iter()
                                                    .cloned()
                                                    .collect(),
                                            };
                                            errors.push(error);
                                        } else {
                                            return false;
                                        }
                                    }
                                }
                            }

                            for (value_element_name, value_element) in elements.iter() {
                                if node.contains_key(value_element_name.as_ref()) {
                                    continue;
                                }

                                match &any_node {
                                    Some(any_node_type) => {
                                        section_name_stack.push("*");

                                        let result = self.validate_tree(
                                            errors,
                                            &any_node_type,
                                            value_element,
                                            section_name_stack,
                                        );

                                        if !result && errors.is_none() {
                                            return false;
                                        }

                                        section_name_stack.pop().unwrap();
                                    }
                                    None => {
                                        if let Some(errors) = errors {
                                            let error = TymlValueValidateError::UnknownValue {
                                                values: value_element.spans().cloned().collect(),
                                                path: section_name_stack
                                                    .iter()
                                                    .chain([element_name].into_iter())
                                                    .map(|name| name.to_string())
                                                    .collect::<Vec<_>>()
                                                    .join("."),
                                            };
                                            errors.push(error);
                                        } else {
                                            return false;
                                        }
                                    }
                                }
                            }
                        }
                        MergedValueTree::Value { value: _, span } => {
                            if let Some(errors) = errors {
                                let error = TymlValueValidateError::NoTreeValue {
                                    found: span.clone(),
                                    path: section_name_stack
                                        .iter()
                                        .chain([element_name].into_iter())
                                        .map(|name| name.to_string())
                                        .collect::<Vec<_>>()
                                        .join("."),
                                };
                                errors.push(error);
                            } else {
                                return false;
                            }
                        }
                    }
                }
            }
            TypeTree::Leaf { ty, span } => {
                if let Type::Named(id) = ty {
                    let named_type_tree = self.named_type_map.get_type(*id).unwrap();

                    match named_type_tree {
                        NamedTypeTree::Struct { tree } => {
                            self.validate_tree(errors, tree, value_tree, section_name_stack);
                        }
                        NamedTypeTree::Enum { elements } => {
                            let found_error_spans = match value_tree {
                                MergedValueTree::Section { elements: _, spans } => {
                                    Some(spans.iter().cloned().collect())
                                }
                                MergedValueTree::Value { value, span } => match value {
                                    Value::String(value) => elements
                                        .iter()
                                        .any(|element| element.value == value)
                                        .not()
                                        .then(|| std::vec![span.clone()]),
                                    _ => Some(std::vec![span.clone()]),
                                },
                            };

                            if let Some(found) = found_error_spans {
                                if let Some(errors) = errors {
                                    let enum_name =
                                        self.named_type_map.get_name(*id).unwrap().to_string();
                                    let enum_span =
                                        self.named_type_map.get_define_span(*id).unwrap();

                                    let error = TymlValueValidateError::InvalidValue {
                                        found,
                                        expected: Spanned::new(enum_name, enum_span),
                                    };
                                    errors.push(error);
                                }
                            }
                        }
                    }
                } else {
                    match value_tree {
                        MergedValueTree::Section { elements: _, spans } => {
                            if let Some(errors) = errors {
                                let error = TymlValueValidateError::InvalidValue {
                                    found: spans.iter().cloned().collect(),
                                    expected: Spanned::new(
                                        ty.to_type_name(&self.named_type_map),
                                        span.clone(),
                                    ),
                                };
                                errors.push(error);
                            }
                        }
                        MergedValueTree::Value { value, span } => todo!(),
                    }
                }
            }
        }

        true
    }

    fn validate_type<'temp>(
        &self,
        ty: &Type,
        value_tree: &MergedValueTree<'section, 'value, 'temp, Span>,
        section_name_stack: &mut allocator_api2::vec::Vec<&'input str, &'temp Bump>,
    ) -> bool {
        match ty {
            Type::Int(attribute) => match value_tree {
                MergedValueTree::Value { value, span: _ } => match value {
                    Value::Int(int) => attribute.validate(*int),
                    Value::UnsignedInt(uint) => {
                        *uint <= u64::MAX as _ && attribute.validate(*uint as _)
                    }
                    Value::AnyString(string) => {
                        if string.starts_with("0x") {
                            match i64::from_str_radix(&string[2..], 16) {
                                Ok(int) => attribute.validate(int),
                                Err(_) => false,
                            }
                        } else if string.starts_with("0o") {
                            match i64::from_str_radix(&string[2..], 8) {
                                Ok(int) => attribute.validate(int),
                                Err(_) => false,
                            }
                        } else if string.starts_with("0b") {
                            match i64::from_str_radix(&string[2..], 2) {
                                Ok(int) => attribute.validate(int),
                                Err(_) => false,
                            }
                        } else {
                            match string.parse::<i64>() {
                                Ok(int) => attribute.validate(int),
                                Err(_) => false,
                            }
                        }
                    }
                    _ => false,
                },
                _ => false,
            },
            Type::UnsignedInt(attribute) => match value_tree {
                MergedValueTree::Value { value, span: _ } => match value {
                    Value::UnsignedInt(uint) => attribute.validate(*uint),
                    Value::Int(int) => *int >= 0 && attribute.validate(*int as _),
                    Value::AnyString(string) => {
                        if string.starts_with("0x") {
                            match u64::from_str_radix(&string[2..], 16) {
                                Ok(int) => attribute.validate(int),
                                Err(_) => false,
                            }
                        } else if string.starts_with("0o") {
                            match u64::from_str_radix(&string[2..], 8) {
                                Ok(int) => attribute.validate(int),
                                Err(_) => false,
                            }
                        } else if string.starts_with("0b") {
                            match u64::from_str_radix(&string[2..], 2) {
                                Ok(int) => attribute.validate(int),
                                Err(_) => false,
                            }
                        } else {
                            match string.parse::<u64>() {
                                Ok(int) => attribute.validate(int),
                                Err(_) => false,
                            }
                        }
                    }
                    _ => false,
                },
                _ => false,
            },
            Type::Float(attribute) => match value_tree {
                MergedValueTree::Value { value, span: _ } => match value {
                    Value::Float(float) => attribute.validate(*float),
                    Value::Int(int) => attribute.validate(*int as _),
                    Value::UnsignedInt(uint) => attribute.validate(*uint as _),
                    Value::AnyString(string) => match string.parse::<f64>() {
                        Ok(float) => attribute.validate(float),
                        Err(_) => false,
                    },
                    _ => false,
                },
                _ => false,
            },
            Type::String(attribute) => match value_tree {
                MergedValueTree::Value { value, span: _ } => match value {
                    Value::String(string) => attribute.validate(&string),
                    Value::AnyString(string) => attribute.validate(&string),
                    _ => false,
                },
                _ => false,
            },
            Type::MaybeInt => match value_tree {
                MergedValueTree::Value { value, span: _ } => match value {
                    Value::Int(_) => true,
                    Value::AnyString(string) => {
                        if string.starts_with("0x") {
                            i64::from_str_radix(&string[2..], 16).is_ok()
                        } else if string.starts_with("0o") {
                            i64::from_str_radix(&string[2..], 8).is_ok()
                        } else if string.starts_with("0b") {
                            i64::from_str_radix(&string[2..], 2).is_ok()
                        } else {
                            string.parse::<i64>().is_ok()
                        }
                    }
                    _ => false,
                },
                _ => false,
            },
            Type::MaybeUnsignedInt => match value_tree {
                MergedValueTree::Value { value, span: _ } => match value {
                    Value::Int(_) => true,
                    Value::UnsignedInt(_) => true,
                    Value::AnyString(string) => {
                        if string.starts_with("0x") {
                            u64::from_str_radix(&string[2..], 16).is_ok()
                        } else if string.starts_with("0o") {
                            u64::from_str_radix(&string[2..], 8).is_ok()
                        } else if string.starts_with("0b") {
                            u64::from_str_radix(&string[2..], 2).is_ok()
                        } else {
                            string.parse::<u64>().is_ok()
                        }
                    }
                    _ => false,
                },
                _ => false,
            },
            Type::Named(name_id) => {
                let named_type_tree = self.named_type_map.get_type(*name_id).unwrap();

                match named_type_tree {
                    NamedTypeTree::Struct { tree } => {
                        self.validate_tree(&mut None, tree, value_tree, section_name_stack)
                    }
                    NamedTypeTree::Enum { elements } => match value_tree {
                        MergedValueTree::Value { value, span: _ } => match value {
                            Value::String(string) | Value::AnyString(string) => {
                                elements.iter().any(|element| element.value == string)
                            }
                            _ => false,
                        },
                        _ => false,
                    },
                }
            }
            Type::Or(items) => items
                .iter()
                .any(|ty| self.validate_type(ty, value_tree, section_name_stack)),
            Type::Array(_) => todo!(),
            Type::Optional(_) => todo!(),
            Type::Unknown => todo!(),
        }
    }
}

pub enum MergedValueTree<'section, 'value, 'temp, Span: PartialEq + Clone + Default> {
    Section {
        elements: HashMap<
            Cow<'section, str>,
            MergedValueTree<'section, 'value, 'temp, Span>,
            DefaultHashBuilder,
            &'temp Bump,
        >,
        spans: Vec<Span, &'temp Bump>,
    },
    Value {
        value: Value<'section, 'value, Span>,
        span: Span,
    },
}

impl<'section, 'value, 'temp, Span: PartialEq + Clone + Default>
    MergedValueTree<'section, 'value, 'temp, Span>
{
    #[auto_enum(Iterator)]
    pub fn spans(&self) -> impl Iterator<Item = &Span> {
        match self {
            MergedValueTree::Section { elements: _, spans } => spans.iter(),
            MergedValueTree::Value { value: _, span } => std::iter::once(span),
        }
    }

    fn merge_and_collect_duplicated(
        value_tree: &ValueTree<'section, 'value, Span>,
        errors: &mut Vec<TymlValueValidateError<Span>>,
        allocator: &'temp Bump,
    ) -> Self {
        let mut new_tree = MergedValueTree::Section {
            elements: HashMap::new_in(allocator),
            spans: vec![in allocator; value_tree.span().clone(); 1],
        };

        Self::merge_inner_recursive(&mut new_tree, value_tree, errors, allocator);

        new_tree
    }

    fn merge_inner_recursive(
        new_tree: &mut Self,
        value_tree: &ValueTree<'section, 'value, Span>,
        errors: &mut Vec<TymlValueValidateError<Span>>,
        allocator: &'temp Bump,
    ) {
        match new_tree {
            MergedValueTree::Section {
                elements: new_elements,
                spans,
            } => match value_tree {
                ValueTree::Section { elements, span } => {
                    spans.push(span.clone());

                    for (element_name, element_values) in elements.iter() {
                        if let Some(first_value) = element_values.first() {
                            let mut new_tree = match first_value {
                                ValueTree::Section {
                                    elements: _,
                                    span: _,
                                } => MergedValueTree::Section {
                                    elements: HashMap::new_in(allocator),
                                    spans: Vec::new_in(allocator),
                                },
                                ValueTree::Value { value, span } => MergedValueTree::Value {
                                    value: value.clone(),
                                    span: span.clone(),
                                },
                            };

                            let mut element_iterator = element_values.iter();

                            if let ValueTree::Value { value: _, span: _ } = first_value {
                                element_iterator.next();
                            }

                            for value_tree in element_iterator {
                                Self::merge_inner_recursive(
                                    &mut new_tree,
                                    value_tree,
                                    errors,
                                    allocator,
                                );
                            }

                            new_elements.insert(element_name.clone(), new_tree);
                        }
                    }
                }
                ValueTree::Value {
                    value: _,
                    span: duplicated,
                } => {
                    let error = TymlValueValidateError::DuplicatedValue {
                        exists: spans.iter().cloned().collect(),
                        duplicated: duplicated.clone(),
                    };
                    errors.push(error);
                }
            },
            MergedValueTree::Value { value: _, span } => {
                let error = TymlValueValidateError::DuplicatedValue {
                    exists: std::vec![span.clone()],
                    duplicated: value_tree.span().clone(),
                };
                errors.push(error);
            }
        }
    }
}
