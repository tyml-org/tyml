use std::{borrow::Cow, fmt::Debug, ops::Not};

use allocator_api2::vec::Vec;
use auto_enums::auto_enum;
use bumpalo::Bump;
use hashbrown::{DefaultHashBuilder, HashMap};

use tyml_parser::ast::Spanned;
use tyml_source::SourceCodeSpan;
use tyml_type::types::{Attribute, NamedTypeMap, NamedTypeTree, ToTypeName, Type, TypeTree};

use crate::error::TymlValueValidateError;

#[derive(Debug, Clone, PartialEq)]
pub enum ValueTree<'section, 'value> {
    Section {
        elements: HashMap<Cow<'section, str>, Vec<ValueTree<'section, 'value>>>,
        name_span: SourceCodeSpan,
        define_span: SourceCodeSpan,
    },
    Array {
        elements: Vec<Self>,
        key_span: SourceCodeSpan,
        span: SourceCodeSpan,
    },
    Value {
        value: ValidateValue<'value>,
        key_span: SourceCodeSpan,
        span: SourceCodeSpan,
    },
}

impl ValueTree<'_, '_> {
    pub fn span(&self) -> &SourceCodeSpan {
        match self {
            ValueTree::Section {
                elements: _,
                name_span: _,
                define_span,
            } => define_span,
            ValueTree::Array {
                elements: _,
                key_span: _,
                span,
            } => span,
            ValueTree::Value {
                value: _,
                key_span: _,
                span,
            } => span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValidateValue<'value> {
    Int(i64),
    UnsignedInt(u64),
    Float(f64),
    String(Cow<'value, str>),
    Bool(bool),
    OverflowInt(String),
    None,
}

pub trait AnyStringEvaluator {
    fn validate_value_type(&self, ty: &Type, value: &Cow<'_, str>) -> bool;
}

pub struct StandardAnyStringEvaluator {}

impl AnyStringEvaluator for StandardAnyStringEvaluator {
    fn validate_value_type(&self, ty: &Type, value: &Cow<'_, str>) -> bool {
        match ty {
            Type::Int(attribute) => {
                if value.starts_with("0x") {
                    match i64::from_str_radix(&value[2..].replace("_", ""), 16) {
                        Ok(int) => attribute.validate(int),
                        Err(_) => false,
                    }
                } else if value.starts_with("0o") {
                    match i64::from_str_radix(&value[2..].replace("_", ""), 8) {
                        Ok(int) => attribute.validate(int),
                        Err(_) => false,
                    }
                } else if value.starts_with("0b") {
                    match i64::from_str_radix(&value[2..].replace("_", ""), 2) {
                        Ok(int) => attribute.validate(int),
                        Err(_) => false,
                    }
                } else {
                    match value.replace("_", "").parse::<i64>() {
                        Ok(int) => attribute.validate(int),
                        Err(_) => false,
                    }
                }
            }
            Type::UnsignedInt(attribute) => {
                if value.starts_with("0x") {
                    match u64::from_str_radix(&value[2..].replace("_", ""), 16) {
                        Ok(int) => attribute.validate(int),
                        Err(_) => false,
                    }
                } else if value.starts_with("0o") {
                    match u64::from_str_radix(&value[2..].replace("_", ""), 8) {
                        Ok(int) => attribute.validate(int),
                        Err(_) => false,
                    }
                } else if value.starts_with("0b") {
                    match u64::from_str_radix(&value[2..].replace("_", ""), 2) {
                        Ok(int) => attribute.validate(int),
                        Err(_) => false,
                    }
                } else {
                    match value.replace("_", "").parse::<u64>() {
                        Ok(int) => attribute.validate(int),
                        Err(_) => false,
                    }
                }
            }
            Type::Float(attribute) => match value.parse::<f64>() {
                Ok(float) => attribute.validate(float),
                Err(_) => false,
            },
            Type::String(attribute) => attribute.validate(&value),
            Type::MaybeInt => {
                if value.starts_with("0x") {
                    i64::from_str_radix(&value[2..].replace("_", ""), 16).is_ok()
                } else if value.starts_with("0o") {
                    i64::from_str_radix(&value[2..].replace("_", ""), 8).is_ok()
                } else if value.starts_with("0b") {
                    i64::from_str_radix(&value[2..].replace("_", ""), 2).is_ok()
                } else {
                    value.replace("_", "").parse::<i64>().is_ok()
                }
            }
            Type::MaybeUnsignedInt => {
                if value.starts_with("0x") {
                    u64::from_str_radix(&value[2..].replace("_", ""), 16).is_ok()
                } else if value.starts_with("0o") {
                    u64::from_str_radix(&value[2..].replace("_", ""), 8).is_ok()
                } else if value.starts_with("0b") {
                    u64::from_str_radix(&value[2..].replace("_", ""), 2).is_ok()
                } else {
                    value.replace("_", "").parse::<u64>().is_ok()
                }
            }
            Type::Optional(_) => {
                if value == "null" {
                    true
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct ValueTypeChecker<'input, 'ty, 'tree, 'map, 'section, 'value> {
    pub type_tree: &'tree TypeTree<'input, 'ty>,
    pub named_type_map: &'map NamedTypeMap<'input, 'ty>,
    pub value_tree: ValueTree<'section, 'value>,
    // for [[section.array]]
    pub additional_section_array_spans:
        std::collections::HashMap<SourceCodeSpan, std::vec::Vec<SourceCodeSpan>>,
    pub merged_value_tree: Option<MergedValueTree<'section, 'value>>,
}

pub enum SetValue<'section, 'input> {
    Value(ValueTree<'section, 'input>),
    CreateSection,
}

impl<'input, 'ty, 'tree, 'map, 'section, 'value>
    ValueTypeChecker<'input, 'ty, 'tree, 'map, 'section, 'value>
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
                name_span: Default::default(),
                define_span: Default::default(),
            },
            additional_section_array_spans: std::collections::HashMap::new(),
            merged_value_tree: None,
        }
    }

    pub fn set_value(
        &mut self,
        sections: impl Iterator<
            Item = (
                impl Into<Cow<'section, str>>,
                SourceCodeSpan,
                SourceCodeSpan,
                bool,
            ),
        >,
        value: SetValue<'section, 'value>,
    ) {
        let root_section = [(
            Cow::Borrowed("root"),
            self.value_tree.span().clone(),
            self.value_tree.span().clone(),
            false,
        )];
        // Iterator<(Into<Cow>, Span)> => Iterator<(Cow, Span)>
        let sections = sections.map(|(section, name_span, define_span, is_array)| {
            (section.into(), name_span, define_span, is_array)
        });

        // add root section on the head of sections iterator
        let mut sections = root_section.into_iter().chain(sections).peekable();
        let mut value_tree = &mut self.value_tree;

        loop {
            let (current_section, current_section_name_span, current_section_define_span, is_array) =
                sections.next().unwrap();

            value_tree = match value_tree {
                ValueTree::Section {
                    elements,
                    name_span: _,
                    define_span: _,
                } => {
                    let element_branches = elements
                        .entry(current_section.clone().into())
                        .or_insert(Vec::new());

                    // search same section that already exists
                    let next_branch_position =
                        element_branches.iter().position(|branch| match branch {
                            ValueTree::Section {
                                elements: _,
                                name_span,
                                define_span: _,
                            } => name_span == &current_section_name_span && !is_array,
                            ValueTree::Array {
                                elements: _,
                                key_span: _,
                                span: _,
                            } => {
                                if is_array {
                                    true
                                } else {
                                    false
                                }
                            }
                            ValueTree::Value {
                                value: _,
                                key_span: _,
                                span: _,
                            } => false,
                        });

                    let matched_section_branch = match next_branch_position {
                        Some(next_branch_position) => {
                            let branch = &mut element_branches[next_branch_position];
                            match branch {
                                ValueTree::Array {
                                    elements,
                                    key_span: _,
                                    span: _,
                                } => {
                                    let next_branch_position =
                                        elements.iter().position(|branch| match branch {
                                            ValueTree::Section {
                                                elements: _,
                                                name_span,
                                                define_span: _,
                                            } => name_span == &current_section_name_span,
                                            ValueTree::Array {
                                                elements: _,
                                                key_span: _,
                                                span: _,
                                            } => false,
                                            ValueTree::Value {
                                                value: _,
                                                key_span: _,
                                                span: _,
                                            } => false,
                                        });

                                    match next_branch_position {
                                        Some(next_branch_position) => {
                                            &mut elements[next_branch_position]
                                        }
                                        None => {
                                            elements.push(ValueTree::Section {
                                                elements: HashMap::new(),
                                                name_span: current_section_name_span,
                                                define_span: current_section_define_span,
                                            });
                                            elements.last_mut().unwrap()
                                        }
                                    }
                                }
                                _ => branch,
                            }
                        }
                        None => {
                            if is_array {
                                element_branches.push(ValueTree::Array {
                                    elements: Vec::new(),
                                    key_span: current_section_name_span.clone(),
                                    span: current_section_define_span.clone(),
                                });
                                if let ValueTree::Array {
                                    elements,
                                    key_span: _,
                                    span: _,
                                } = element_branches.last_mut().unwrap()
                                {
                                    elements.push(ValueTree::Section {
                                        elements: HashMap::new(),
                                        name_span: current_section_name_span,
                                        define_span: current_section_define_span,
                                    });
                                    elements.last_mut().unwrap()
                                } else {
                                    unreachable!()
                                }
                            } else {
                                element_branches.push(ValueTree::Section {
                                    elements: HashMap::new(),
                                    name_span: current_section_name_span,
                                    define_span: current_section_define_span,
                                });
                                element_branches.last_mut().unwrap()
                            }
                        }
                    };

                    // if last section
                    if sections.peek().is_none() {
                        if let SetValue::Value(value) = value {
                            *matched_section_branch = value;
                        }
                        return;
                    }

                    matched_section_branch
                }
                ValueTree::Array {
                    elements: _,
                    key_span: _,
                    span: _,
                }
                | ValueTree::Value {
                    value: _,
                    key_span: _,
                    span: _,
                } => {
                    // It is unreachable, because the value has been validated in up layer section.
                    unreachable!()
                }
            };
        }
    }

    pub fn validate(&mut self) -> Result<(), Vec<TymlValueValidateError>> {
        let mut errors = Vec::new();

        let allocator = Bump::new();

        let merged_value_tree = MergedValueTree::merge_and_collect_duplicated(
            &self.value_tree,
            &mut errors,
            &allocator,
        );

        let empty_merged_tree = MergedValueTree::Section {
            elements: HashMap::new(),
            name_spans: allocator_api2::vec![SourceCodeSpan::UTF8Byte(0..0)],
            define_spans: allocator_api2::vec![SourceCodeSpan::UTF8Byte(0..0)],
        };

        let root_value_tree = match &merged_value_tree {
            MergedValueTree::Section {
                elements,
                name_spans: _,
                define_spans: _,
            } => elements.get("root").unwrap_or(&empty_merged_tree),
            MergedValueTree::Array {
                elements: _,
                key_span: _,
                span: _,
            } => unreachable!(),
            MergedValueTree::Value {
                value: _,
                key_span: _,
                span: _,
            } => unreachable!(),
        };

        self.validate_tree(
            &mut Some(&mut errors),
            &self.type_tree,
            &root_value_tree,
            &mut Vec::new_in(&allocator),
            &mut Vec::new(),
        );

        self.merged_value_tree = Some(merged_value_tree);

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn validate_tree<'temp>(
        &self,
        errors: &mut Option<&mut Vec<TymlValueValidateError>>,
        type_tree: &'tree TypeTree<'input, 'ty>,
        value_tree: &MergedValueTree<'section, 'value>,
        section_name_stack: &mut Vec<Cow<'input, str>, &'temp Bump>,
        non_matched_name_stack: &mut Vec<(String, Type<'ty>)>,
    ) -> bool {
        match type_tree {
            TypeTree::Node {
                node,
                any_node,
                documents: _,
                span: tyml_span,
            } => match value_tree {
                MergedValueTree::Section {
                    elements,
                    name_spans: _,
                    define_spans: value_tree_section_spans,
                } => {
                    let mut has_error = false;
                    for (element_name, element_type) in node.iter() {
                        match elements.get(element_name.as_ref()) {
                            Some(element_value) => {
                                section_name_stack.push(element_name.clone());

                                let result = self.validate_tree(
                                    errors,
                                    element_type,
                                    element_value,
                                    section_name_stack,
                                    non_matched_name_stack,
                                );

                                section_name_stack.pop().unwrap();

                                if !result && errors.is_none() {
                                    has_error = true;
                                }
                            }
                            None => {
                                if !element_type.is_allowed_optional() {
                                    if let Some(errors) = errors {
                                        let error = TymlValueValidateError::NoValueFound {
                                            required: Spanned::new(
                                                section_name_stack
                                                    .iter()
                                                    .chain([&element_name.clone()].into_iter())
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
                    }

                    if has_error && errors.is_none() {
                        return false;
                    }

                    for (value_element_name, value_element) in elements.iter() {
                        if node.contains_key(value_element_name.as_ref()) {
                            continue;
                        }

                        match &any_node {
                            Some(any_node_type) => {
                                section_name_stack.push("*".into());

                                let result = self.validate_tree(
                                    errors,
                                    &any_node_type,
                                    value_element,
                                    section_name_stack,
                                    non_matched_name_stack,
                                );

                                section_name_stack.pop().unwrap();

                                if !result && errors.is_none() {
                                    return false;
                                }
                            }
                            None => {
                                if let Some(errors) = errors {
                                    let error = TymlValueValidateError::UnknownValue {
                                        values: value_element.spans().cloned().collect(),
                                        path: section_name_stack
                                            .iter()
                                            .chain([&value_element_name.clone()].into_iter())
                                            .map(|name| name.to_string())
                                            .collect::<Vec<_>>()
                                            .join("."),
                                    };
                                    errors.push(error);
                                } else {
                                    non_matched_name_stack.push((
                                        section_name_stack
                                            .iter()
                                            .map(|name| name.to_string())
                                            .collect::<Vec<_>>()
                                            .join("."),
                                        Type::Unknown,
                                    ));
                                    return false;
                                }
                            }
                        }
                    }
                }
                MergedValueTree::Array {
                    elements: _,
                    key_span: _,
                    span,
                }
                | MergedValueTree::Value {
                    value: _,
                    key_span: _,
                    span,
                } => {
                    let is_empty_value = match value_tree {
                        MergedValueTree::Value {
                            value,
                            key_span: _,
                            span: _,
                        } => value == &ValidateValue::None,
                        _ => false,
                    };

                    if let Some(errors) = errors {
                        if is_empty_value {
                            let error = TymlValueValidateError::NoValueFound {
                                required: Spanned::new(
                                    section_name_stack
                                        .iter()
                                        .map(|name| name.to_string())
                                        .collect::<Vec<_>>()
                                        .join("."),
                                    type_tree.span(),
                                ),
                                required_in: vec![span.clone()],
                            };
                            errors.push(error);
                        } else {
                            let error = TymlValueValidateError::NotTreeValue {
                                found: span.clone(),
                                path: section_name_stack
                                    .iter()
                                    .map(|name| name.to_string())
                                    .collect::<Vec<_>>()
                                    .join("."),
                                tyml_span: tyml_span.clone(),
                            };
                            errors.push(error);
                        }
                    } else {
                        return false;
                    }
                }
            },
            TypeTree::Leaf {
                ty,
                span,
                documents: _,
            } => match ty {
                Type::Named(id) => {
                    let named_type_tree = self.named_type_map.get_type(*id).unwrap();

                    match named_type_tree {
                        NamedTypeTree::Struct { tree } => {
                            let result = self.validate_tree(
                                errors,
                                tree,
                                value_tree,
                                section_name_stack,
                                non_matched_name_stack,
                            );

                            if !result && errors.is_none() {
                                return false;
                            }
                        }
                        NamedTypeTree::Enum {
                            elements,
                            documents: _,
                        } => {
                            let found_error_spans = match value_tree {
                                MergedValueTree::Section {
                                    elements: _,
                                    name_spans: _,
                                    define_spans: spans,
                                } => Some(spans.iter().cloned().collect()),
                                MergedValueTree::Array {
                                    elements: _,
                                    key_span: _,
                                    span,
                                } => Some(std::vec![span.clone()]),
                                MergedValueTree::Value {
                                    value,
                                    key_span: _,
                                    span,
                                } => match value {
                                    ValidateValue::String(value) => elements
                                        .iter()
                                        .any(|element| &element.0.value == value)
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
                                        path: section_name_stack
                                            .iter()
                                            .map(|name| name.to_string())
                                            .collect::<Vec<_>>()
                                            .join("."),
                                        caused_by: std::vec::Vec::new(),
                                    };
                                    errors.push(error);
                                } else {
                                    non_matched_name_stack.push((
                                        section_name_stack
                                            .iter()
                                            .map(|name| name.to_string())
                                            .collect::<Vec<_>>()
                                            .join("."),
                                        ty.clone(),
                                    ));
                                    return false;
                                }
                            }
                        }
                    }
                }
                Type::Array(base_type) => {
                    if let MergedValueTree::Array {
                        elements,
                        key_span: _,
                        span: _,
                    } = value_tree
                    {
                        for element in elements.iter() {
                            let mut new_non_matched_name_stack = Vec::new();

                            if self.validate_type(
                                &base_type,
                                element,
                                section_name_stack,
                                &mut new_non_matched_name_stack,
                            ) {
                                continue;
                            }

                            if let Some(errors) = errors {
                                let error = TymlValueValidateError::InvalidValue {
                                    found: element.spans().cloned().collect(),
                                    expected: Spanned::new(
                                        ty.to_type_name(&self.named_type_map),
                                        span.clone(),
                                    ),
                                    path: section_name_stack
                                        .iter()
                                        .map(|name| name.to_string())
                                        .collect::<Vec<_>>()
                                        .join("."),
                                    caused_by: new_non_matched_name_stack
                                        .into_iter()
                                        .map(|(name, ty)| {
                                            (name, ty.to_type_name(self.named_type_map))
                                        })
                                        .collect(),
                                };
                                errors.push(error);
                            } else {
                                non_matched_name_stack.push((
                                    section_name_stack
                                        .iter()
                                        .map(|name| name.to_string())
                                        .collect::<Vec<_>>()
                                        .join("."),
                                    ty.clone(),
                                ));
                                return false;
                            }
                        }
                    } else {
                        if let Some(errors) = errors {
                            let error = TymlValueValidateError::NotArrayValue {
                                found: value_tree.spans().cloned().collect(),
                                expected: Spanned::new(
                                    ty.to_type_name(&self.named_type_map),
                                    span.clone(),
                                ),
                                path: section_name_stack
                                    .iter()
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
                _ => {
                    let mut new_non_matched_name_stack = Vec::new();
                    let result = self.validate_type(
                        ty,
                        value_tree,
                        section_name_stack,
                        &mut new_non_matched_name_stack,
                    );

                    if !result {
                        if let Some(errors) = errors {
                            let found_spans = match value_tree {
                                MergedValueTree::Array {
                                    elements,
                                    key_span: _,
                                    span: _,
                                } => elements
                                    .iter()
                                    .map(|element| element.spans().cloned())
                                    .flatten()
                                    .collect(),
                                _ => value_tree.spans().cloned().collect(),
                            };

                            let error = TymlValueValidateError::InvalidValue {
                                found: found_spans,
                                expected: Spanned::new(
                                    ty.to_type_name(&self.named_type_map),
                                    span.clone(),
                                ),
                                path: section_name_stack
                                    .iter()
                                    .map(|name| name.to_string())
                                    .collect::<Vec<_>>()
                                    .join("."),
                                caused_by: new_non_matched_name_stack
                                    .into_iter()
                                    .map(|(name, ty)| (name, ty.to_type_name(self.named_type_map)))
                                    .collect(),
                            };
                            errors.push(error);
                        } else {
                            non_matched_name_stack.push((
                                section_name_stack
                                    .iter()
                                    .map(|name| name.to_string())
                                    .collect::<Vec<_>>()
                                    .join("."),
                                ty.clone(),
                            ));
                            return false;
                        }
                    }
                }
            },
        }

        true
    }

    fn validate_type<'temp>(
        &self,
        ty: &Type,
        value_tree: &MergedValueTree<'section, 'value>,
        section_name_stack: &mut Vec<Cow<'input, str>, &'temp Bump>,
        non_matched_name_stack: &mut Vec<(String, Type<'ty>)>,
    ) -> bool {
        match ty {
            Type::Int(attribute) => match value_tree {
                MergedValueTree::Value {
                    value,
                    key_span: _,
                    span: _,
                } => match value {
                    ValidateValue::Int(int) => attribute.validate(*int),
                    ValidateValue::UnsignedInt(uint) => {
                        *uint <= u64::MAX as _ && attribute.validate(*uint as _)
                    }
                    _ => false,
                },
                _ => false,
            },
            Type::UnsignedInt(attribute) => match value_tree {
                MergedValueTree::Value {
                    value,
                    key_span: _,
                    span: _,
                } => match value {
                    ValidateValue::UnsignedInt(uint) => attribute.validate(*uint),
                    ValidateValue::Int(int) => *int >= 0 && attribute.validate(*int as _),
                    _ => false,
                },
                _ => false,
            },
            Type::Float(attribute) => match value_tree {
                MergedValueTree::Value {
                    value,
                    key_span: _,
                    span: _,
                } => match value {
                    ValidateValue::Float(float) => attribute.validate(*float),
                    ValidateValue::Int(int) => attribute.validate(*int as _),
                    ValidateValue::UnsignedInt(uint) => attribute.validate(*uint as _),
                    _ => false,
                },
                _ => false,
            },
            Type::Bool => match value_tree {
                MergedValueTree::Value {
                    value,
                    key_span: _,
                    span: _,
                } => match value {
                    ValidateValue::Bool(_) => true,
                    _ => false,
                },
                _ => false,
            },
            Type::String(attribute) => match value_tree {
                MergedValueTree::Value {
                    value,
                    key_span: _,
                    span: _,
                } => match value {
                    ValidateValue::String(string) => attribute.validate(&string),
                    _ => false,
                },
                _ => false,
            },
            Type::MaybeInt => match value_tree {
                MergedValueTree::Value {
                    value,
                    key_span: _,
                    span: _,
                } => match value {
                    ValidateValue::Int(_) => true,
                    _ => false,
                },
                _ => false,
            },
            Type::MaybeUnsignedInt => match value_tree {
                MergedValueTree::Value {
                    value,
                    key_span: _,
                    span: _,
                } => match value {
                    ValidateValue::Int(_) => true,
                    ValidateValue::UnsignedInt(_) => true,
                    _ => false,
                },
                _ => false,
            },
            Type::Named(name_id) => {
                let named_type_tree = self.named_type_map.get_type(*name_id).unwrap();

                match named_type_tree {
                    NamedTypeTree::Struct { tree } => self.validate_tree(
                        &mut None,
                        tree,
                        value_tree,
                        section_name_stack,
                        non_matched_name_stack,
                    ),
                    NamedTypeTree::Enum {
                        elements,
                        documents: _,
                    } => match value_tree {
                        MergedValueTree::Value {
                            value,
                            key_span: _,
                            span: _,
                        } => match value {
                            ValidateValue::String(string) => {
                                elements.iter().any(|element| &element.0.value == string)
                            }
                            _ => false,
                        },
                        _ => false,
                    },
                }
            }
            Type::Or(items) => items.iter().any(|ty| {
                self.validate_type(ty, value_tree, section_name_stack, non_matched_name_stack)
            }),
            Type::Array(ty) => match value_tree {
                MergedValueTree::Array {
                    elements,
                    key_span: _,
                    span: _,
                } => elements.iter().all(|element| {
                    self.validate_type(&ty, element, section_name_stack, non_matched_name_stack)
                }),
                _ => false,
            },
            Type::Optional(base_type) => {
                if let MergedValueTree::Value {
                    value,
                    key_span: _,
                    span: _,
                } = value_tree
                {
                    match value {
                        ValidateValue::None => return true,
                        _ => {}
                    }
                }

                self.validate_type(
                    &base_type,
                    value_tree,
                    section_name_stack,
                    non_matched_name_stack,
                )
            }
            Type::Any => true,
            Type::Unknown => true,
        }
    }
}

#[derive(Debug)]
pub enum MergedValueTree<'section, 'value> {
    Section {
        elements:
            HashMap<Cow<'section, str>, MergedValueTree<'section, 'value>, DefaultHashBuilder>,
        name_spans: Vec<SourceCodeSpan>,
        define_spans: Vec<SourceCodeSpan>,
    },
    Array {
        elements: Vec<Self>,
        key_span: SourceCodeSpan,
        span: SourceCodeSpan,
    },
    Value {
        value: ValidateValue<'value>,
        key_span: SourceCodeSpan,
        span: SourceCodeSpan,
    },
}

impl<'section, 'value> MergedValueTree<'section, 'value> {
    #[auto_enum(Iterator)]
    pub fn spans(&self) -> impl Iterator<Item = &SourceCodeSpan> {
        match self {
            MergedValueTree::Section {
                elements: _,
                name_spans: _,
                define_spans,
            } => define_spans.iter(),
            MergedValueTree::Array {
                elements: _,
                key_span: _,
                span,
            } => std::iter::once(span),
            MergedValueTree::Value {
                value: _,
                key_span: _,
                span,
            } => std::iter::once(span),
        }
    }

    fn merge_and_collect_duplicated(
        value_tree: &ValueTree<'section, 'value>,
        errors: &mut Vec<TymlValueValidateError>,
        allocator: &Bump,
    ) -> Self {
        let mut new_tree = MergedValueTree::Section {
            elements: HashMap::new(),
            name_spans: Vec::new(),
            define_spans: Vec::new(),
        };

        Self::merge_inner_recursive(
            &mut new_tree,
            value_tree,
            errors,
            &mut allocator_api2::vec::Vec::new_in(allocator),
            false,
        );

        new_tree
    }

    fn merge_inner_recursive<'tree>(
        new_tree: &mut Self,
        value_tree: &'tree ValueTree<'section, 'value>,
        errors: &mut Vec<TymlValueValidateError>,
        section_name_stack: &mut allocator_api2::vec::Vec<&'tree str, &Bump>,
        is_init_merge: bool,
    ) {
        match new_tree {
            MergedValueTree::Section {
                elements: new_elements,
                name_spans: spans,
                define_spans,
            } => match value_tree {
                ValueTree::Section {
                    elements,
                    name_span,
                    define_span,
                } => {
                    spans.push(name_span.clone());
                    define_spans.push(define_span.clone());

                    for (element_name, element_values) in elements.iter() {
                        if let Some(first_value) = element_values.first() {
                            let mut new_tree_instance = match first_value {
                                ValueTree::Section {
                                    elements: _,
                                    name_span: _,
                                    define_span: _,
                                } => MergedValueTree::Section {
                                    elements: HashMap::new(),
                                    name_spans: Vec::new(),
                                    define_spans: Vec::new(),
                                },
                                ValueTree::Array {
                                    elements,
                                    key_span,
                                    span,
                                } => MergedValueTree::Array {
                                    elements: Vec::with_capacity(elements.len()),
                                    key_span: key_span.clone(),
                                    span: span.clone(),
                                },
                                ValueTree::Value {
                                    value,
                                    key_span,
                                    span,
                                } => MergedValueTree::Value {
                                    value: value.clone(),
                                    key_span: key_span.clone(),
                                    span: span.clone(),
                                },
                            };

                            let (new_tree, use_new_instance) =
                                match new_elements.get_mut(element_name.as_ref()) {
                                    Some(exists) => (exists, false),
                                    None => (&mut new_tree_instance, true),
                                };

                            section_name_stack.push(&element_name);

                            for (index, value_tree) in element_values.iter().enumerate() {
                                let is_init_merge = index == 0 && use_new_instance;

                                Self::merge_inner_recursive(
                                    new_tree,
                                    value_tree,
                                    errors,
                                    section_name_stack,
                                    is_init_merge,
                                );
                            }

                            section_name_stack.pop().unwrap();

                            if use_new_instance {
                                new_elements.insert(element_name.clone(), new_tree_instance);
                            }
                        }
                    }
                }
                ValueTree::Value {
                    value: _,
                    key_span: _,
                    span: duplicated,
                }
                | ValueTree::Array {
                    elements: _,
                    key_span: _,
                    span: duplicated,
                } => {
                    let error = TymlValueValidateError::DuplicatedValue {
                        exists: spans.iter().cloned().collect(),
                        duplicated: duplicated.clone(),
                        path: section_name_stack
                            .iter()
                            .skip(1)
                            .map(|path| path.to_string())
                            .collect::<Vec<_>>()
                            .join("."),
                    };
                    errors.push(error);
                }
            },
            MergedValueTree::Array {
                elements: new_elements,
                key_span: _,
                span,
            } => {
                if let ValueTree::Array {
                    elements,
                    key_span: _,
                    span: _,
                } = value_tree
                {
                    for element in elements.iter() {
                        let mut new_tree = match element {
                            ValueTree::Section {
                                elements: _,
                                name_span: _,
                                define_span: _,
                            } => MergedValueTree::Section {
                                elements: HashMap::new(),
                                name_spans: Vec::new(),
                                define_spans: Vec::new(),
                            },
                            ValueTree::Array {
                                elements: _,
                                key_span,
                                span,
                            } => MergedValueTree::Array {
                                elements: Vec::new(),
                                key_span: key_span.clone(),
                                span: span.clone(),
                            },
                            ValueTree::Value {
                                value,
                                key_span,
                                span,
                            } => MergedValueTree::Value {
                                value: value.clone(),
                                key_span: key_span.clone(),
                                span: span.clone(),
                            },
                        };

                        section_name_stack.push("[array]");

                        Self::merge_inner_recursive(
                            &mut new_tree,
                            element,
                            errors,
                            section_name_stack,
                            true,
                        );

                        section_name_stack.pop().unwrap();

                        new_elements.push(new_tree);
                    }
                } else {
                    let error = TymlValueValidateError::DuplicatedValue {
                        exists: std::vec![span.clone()],
                        duplicated: value_tree.span().clone(),
                        path: section_name_stack
                            .iter()
                            .skip(1)
                            .map(|path| path.to_string())
                            .collect::<Vec<_>>()
                            .join("."),
                    };
                    errors.push(error);
                }
            }
            MergedValueTree::Value {
                value: _,
                key_span: _,
                span,
            } => {
                if is_init_merge {
                    if let ValueTree::Value {
                        value: _,
                        key_span: _,
                        span: _,
                    } = value_tree
                    {
                    } else {
                        unreachable!()
                    };
                } else {
                    let error = TymlValueValidateError::DuplicatedValue {
                        exists: std::vec![span.clone()],
                        duplicated: value_tree.span().clone(),
                        path: section_name_stack
                            .iter()
                            .skip(1)
                            .map(|path| path.to_string())
                            .collect::<Vec<_>>()
                            .join("."),
                    };
                    errors.push(error);
                }
            }
        }
    }
}
