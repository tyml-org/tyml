use std::{borrow::Cow, fmt::Display, ops::Range, sync::Arc};

use allocator_api2::{boxed::Box, vec::Vec};
use bumpalo::Bump;
use either::Either;
use hashbrown::{DefaultHashBuilder, HashMap};
use regex::Regex;
use serde_json::Value;
use tyml_parser::ast::{
    AttributeAnd, AttributeOr, BaseType, BinaryLiteral, Define, Defines, Documents, ElementDefine,
    EscapedLiteral, FloatLiteral, FromTo, Interface, JsonValue, LiteralOrDefault, NameOrAtBody,
    NodeLiteral, NumericAttribute, NumericAttributeKind, OrType, Spanned, TypeAttribute,
    TypeDefine, ValueLiteral, AST,
};

use crate::{
    error::{TypeError, TypeErrorKind},
    name::{NameEnvironment, NameID},
    types::{
        AttributeSet, AttributeTree, FloatAttribute, FunctionArgumentInfo,
        FunctionBodyArgumentInfo, FunctionInfo, FunctionReturnInfo, FunctionThrowsInfo,
        IntAttribute, InterfaceInfo, NamedThrowsInfo, NamedTypeMap, NamedTypeTree,
        NumericalValueRange, StringAttribute, Type, TypeTree, UnsignedIntAttribute,
    },
};

pub fn resolve_type<'input, 'ast_allocator>(
    ast: &'ast_allocator Defines<'input, 'ast_allocator>,
    type_allocator: &'ast_allocator Bump,
) -> (
    TypeTree<'input, 'ast_allocator>,
    NamedTypeMap<'input, 'ast_allocator>,
    Vec<InterfaceInfo<'input, 'ast_allocator, 'ast_allocator>, &'ast_allocator Bump>,
    Vec<TypeError<'input, 'ast_allocator>, &'ast_allocator Bump>,
) {
    let env_allocator = Bump::new();
    let name_env = NameEnvironment::new(None, &env_allocator);

    let mut named_type_map = NamedTypeMap::new(type_allocator);
    let mut interfaces = Vec::new_in(type_allocator);
    let mut errors = Vec::new_in(type_allocator);

    let root_tree = resolve_defines_type(
        ast,
        None,
        None,
        name_env,
        &mut named_type_map,
        &mut interfaces,
        &mut errors,
        &env_allocator,
        type_allocator,
    );

    (root_tree, named_type_map, interfaces, errors)
}

fn resolve_defines_type<'input, 'env, 'ast_allocator>(
    ast: &'ast_allocator Defines<'input, 'ast_allocator>,
    documents: Option<&Documents<'input, 'ast_allocator>>,
    tree_span: Option<Range<usize>>,
    name_env: &'env NameEnvironment<'env, 'input>,
    named_type_map: &mut NamedTypeMap<'input, 'ast_allocator>,
    interfaces: &mut Vec<
        InterfaceInfo<'input, 'ast_allocator, 'ast_allocator>,
        &'ast_allocator Bump,
    >,
    errors: &mut Vec<TypeError<'input, 'ast_allocator>, &'ast_allocator Bump>,
    env: &'env Bump,
    ty: &'ast_allocator Bump,
) -> TypeTree<'input, 'ast_allocator> {
    // collect type names first
    for define in ast.defines.iter() {
        if let Define::Type(type_define) = define {
            name_env.register(type_define);
        }
    }

    let mut node = HashMap::new_in(ty);
    let mut any_node = None;
    let mut node_key_span = HashMap::new_in(ty);
    let mut any_node_key_span = None;

    for define in ast.defines.iter() {
        match define {
            Define::Element(element_define) => {
                let element_type = get_element_type(
                    element_define,
                    name_env,
                    named_type_map,
                    interfaces,
                    errors,
                    env,
                    ty,
                );

                match &element_define.node {
                    NodeLiteral::Literal(name) => {
                        node.insert(name.value.clone(), element_type);
                        node_key_span.insert(name.value.clone(), name.span());
                    }
                    NodeLiteral::Asterisk(literal) => {
                        any_node = Some(Box::new_in(element_type, ty));
                        any_node_key_span = Some(literal.span.clone());
                    }
                }
            }
            Define::Type(type_define) => {
                let name_id = NameID::from(type_define);

                let (name, name_span, named_type) = match type_define {
                    TypeDefine::Struct(struct_define) => {
                        let tree = resolve_defines_type(
                            &struct_define.defines,
                            Some(&struct_define.documents),
                            Some(struct_define.span.clone()),
                            name_env,
                            named_type_map,
                            interfaces,
                            errors,
                            env,
                            ty,
                        );
                        (
                            struct_define.name.value,
                            struct_define.name.span.clone(),
                            NamedTypeTree::Struct { tree },
                        )
                    }
                    TypeDefine::Enum(enum_define) => {
                        let mut elements = Vec::with_capacity_in(enum_define.elements.len(), ty);
                        for element in enum_define.elements.iter() {
                            let mut documents =
                                Vec::with_capacity_in(element.documents.lines.len(), ty);
                            for line in element.documents.lines.iter() {
                                documents.push(*line);
                            }

                            elements.push((
                                Spanned::new(
                                    element.literal_value.clone(),
                                    element.literal.span.clone(),
                                ),
                                documents,
                            ));
                        }

                        let mut documents =
                            Vec::with_capacity_in(enum_define.documents.lines.len(), ty);
                        for line in enum_define.documents.lines.iter() {
                            documents.push(*line);
                        }

                        (
                            enum_define.name.value,
                            enum_define.name.span.clone(),
                            NamedTypeTree::Enum {
                                elements,
                                documents,
                            },
                        )
                    }
                };

                named_type_map.register(name_id, name, name_span, named_type);
            }
            Define::Interface(interface) => {
                collect_interface_info(
                    interface,
                    name_env,
                    named_type_map,
                    interfaces,
                    errors,
                    env,
                    ty,
                );
            }
        }
    }

    let mut tree_documents = Vec::with_capacity_in(
        documents
            .map(|documents| documents.lines.len())
            .unwrap_or(0),
        ty,
    );
    if let Some(documents) = documents {
        for line in documents.lines.iter() {
            tree_documents.push(*line);
        }
    }

    TypeTree::Node {
        node,
        any_node,
        node_key_span,
        any_node_key_span,
        documents: tree_documents,
        span: tree_span.unwrap_or(ast.span.clone()),
    }
}

pub fn camel_to_snake(s: &str) -> String {
    use std::iter::Peekable;
    let mut out = String::with_capacity(s.len() * 2);
    let mut chars: Peekable<_> = s.chars().peekable();
    let mut prev: Option<char> = None;

    while let Some(c) = chars.next() {
        let next = chars.peek().copied();

        if c.is_uppercase() {
            let need_underscore = matches!(prev, Some(p) if p != '_' &&
                (p.is_lowercase() || p.is_ascii_digit() ||
                 next.map(|n| n.is_lowercase()).unwrap_or(false)));

            if need_underscore {
                out.push('_');
            }

            for lc in c.to_lowercase() {
                out.push(lc);
            }
        } else {
            out.push(c);
        }

        prev = Some(c);
    }

    out
}

fn collect_interface_info<'input, 'env, 'ast_allocator>(
    ast: &'ast_allocator Interface<'input, 'ast_allocator>,
    name_env: &'env NameEnvironment<'env, 'input>,
    named_type_map: &mut NamedTypeMap<'input, 'ast_allocator>,
    interfaces: &mut Vec<
        InterfaceInfo<'input, 'ast_allocator, 'ast_allocator>,
        &'ast_allocator Bump,
    >,
    errors: &mut Vec<TypeError<'input, 'ast_allocator>, &'ast_allocator Bump>,
    env: &'env Bump,
    ty_allocator: &'ast_allocator Bump,
) {
    let final_name = ast
        .properties
        .elements
        .iter()
        .find(|property| property.name.value.as_ref() == "rename")
        .map(|property| {
            property.values.get(0).map(|value| match value {
                ValueLiteral::String(literal) => Some(literal.clone().map(|name| name.to_string())),
                _ => None,
            })
        })
        .flatten()
        .flatten()
        .unwrap_or(ast.name.clone().map(|name| camel_to_snake(name)));

    let same_name = interfaces
        .iter()
        .map(|interface| &interface.name)
        .find(|name| name.value == final_name.value.as_str());

    if let Some(name) = same_name {
        let error = TypeError {
            kind: TypeErrorKind::NameAlreadyExists {
                exists: name.clone().map(|name| name.into()),
            },
            span: ast.name.span(),
        };
        errors.push(error);
    }

    let mut functions: Vec<FunctionInfo, &'ast_allocator Bump> = Vec::new_in(ty_allocator);
    let mut json_tree_type_cache = JsonTreeTypeCache::new(ty_allocator);
    for function in ast.functions.iter() {
        let final_name = function
            .properties
            .elements
            .iter()
            .find(|property| property.name.value.as_ref() == "rename")
            .map(|property| {
                property.values.get(0).map(|value| match value {
                    ValueLiteral::String(literal) => {
                        Some(literal.clone().map(|name| name.to_string()))
                    }
                    _ => None,
                })
            })
            .flatten()
            .flatten()
            .unwrap_or(function.name.clone().map(|name| camel_to_snake(name)));

        let same_name = functions
            .iter()
            .map(|function| &function.name)
            .find(|name| name.value == final_name.value.as_str());

        if let Some(name) = same_name {
            let error = TypeError {
                kind: TypeErrorKind::NameAlreadyExists {
                    exists: name.clone().map(|name| name.into()),
                },
                span: function.name.span(),
            };
            errors.push(error);
        }

        let authed = function.authed.clone();

        let mut body_argument_info: Option<FunctionBodyArgumentInfo<'_, '_, '_>> = None;
        let mut arguments = Vec::new_in(ty_allocator);
        for argument in function.arguments.iter() {
            let ty = resolve_or_type(
                &argument.ty.type_info,
                name_env,
                named_type_map,
                errors,
                env,
                ty_allocator,
            );
            if let Some(default_value) = &argument.default_value {
                validate_json_type(
                    default_value,
                    &ty,
                    argument.ty.span(),
                    named_type_map,
                    &mut json_tree_type_cache,
                    errors,
                    ty_allocator,
                );
            }

            match &argument.name {
                NameOrAtBody::Name(name) => {
                    arguments.push(FunctionArgumentInfo {
                        name: name.clone(),
                        ty,
                        default_value: argument.default_value.as_ref(),
                    });
                }
                NameOrAtBody::AtBody(name) => {
                    if let Some(exists) = &body_argument_info {
                        let error = TypeError {
                            kind: TypeErrorKind::BodyArgumentAlreadyExists {
                                exists: exists.name.clone(),
                            },
                            span: name.span(),
                        };
                        errors.push(error);
                    }

                    body_argument_info = Some(FunctionBodyArgumentInfo {
                        name: name.span(),
                        ty,
                        default_value: argument.default_value.as_ref(),
                    })
                }
            }
        }

        let return_info = match &function.return_type {
            Some(return_type) => {
                let ty = resolve_or_type(
                    &return_type.type_info,
                    name_env,
                    named_type_map,
                    errors,
                    env,
                    ty_allocator,
                );
                if let Some(return_block) = &function.return_block {
                    validate_json_type(
                        &return_block.return_expression.value,
                        &ty,
                        return_type.span(),
                        named_type_map,
                        &mut json_tree_type_cache,
                        errors,
                        ty_allocator,
                    );
                }
                Some(FunctionReturnInfo {
                    ty,
                    default_value: function
                        .return_block
                        .as_ref()
                        .map(|return_block| &return_block.return_expression.value),
                })
            }
            None => None,
        };

        let throws = match &function.throws {
            Some(throws) => {
                let mut default = None;
                let mut named = Vec::new_in(ty_allocator);

                for error_type in throws.error_types.iter() {
                    let ty = resolve_or_type(
                        &error_type.ty,
                        name_env,
                        named_type_map,
                        errors,
                        env,
                        ty_allocator,
                    );

                    match &error_type.name {
                        LiteralOrDefault::Literal(name) => {
                            named.push(NamedThrowsInfo {
                                name: name.clone(),
                                ty,
                            });
                        }
                        LiteralOrDefault::Default(_) => default = Some(ty),
                    }
                }

                Some(FunctionThrowsInfo { default, named })
            }
            None => None,
        };

        functions.push(FunctionInfo {
            authed,
            name: final_name,
            arguments,
            body_argument_info,
            return_info,
            throws,
        });
    }

    interfaces.push(InterfaceInfo {
        name: final_name,
        functions,
        json_tree_type_cache,
    });
}

#[derive(Debug)]
pub struct JsonTreeTypeCache<'input, 'ast_allocator> {
    pub field_user_map: HashMap<
        Range<usize>,
        Vec<Range<usize>, &'ast_allocator Bump>,
        DefaultHashBuilder,
        &'ast_allocator Bump,
    >,
    pub field_completion_map: HashMap<
        Range<usize>,
        Vec<Spanned<Cow<'input, str>>, &'ast_allocator Bump>,
        DefaultHashBuilder,
        &'ast_allocator Bump,
    >,
}

impl<'input, 'ast_allocator> JsonTreeTypeCache<'input, 'ast_allocator> {
    pub fn new(allocator: &'ast_allocator Bump) -> Self {
        Self {
            field_user_map: HashMap::new_in(allocator),
            field_completion_map: HashMap::new_in(allocator),
        }
    }

    fn link_user(&mut self, define: Range<usize>, user: Range<usize>) {
        let allocator = *self.field_user_map.allocator();
        let users = self
            .field_user_map
            .entry(define)
            .or_insert(Vec::new_in(allocator));
        users.push(user);
    }

    fn link_field_completions(
        &mut self,
        field_value_span: Range<usize>,
        type_tree: &TypeTree<'input, 'ast_allocator>,
    ) {
        let allocator = *self.field_completion_map.allocator();

        if let TypeTree::Node {
            node,
            any_node: _,
            node_key_span,
            any_node_key_span: _,
            documents: _,
            span: _,
        } = type_tree
        {
            let mut completions = Vec::with_capacity_in(node.keys().count(), allocator);
            for element_name in node.keys() {
                completions.push(Spanned::new(
                    element_name.clone(),
                    node_key_span.get(element_name.as_ref()).cloned().unwrap(),
                ));
            }
            self.field_completion_map
                .insert(field_value_span, completions);
        }
    }
}

fn validate_json_type<'input, 'ast_allocator>(
    json_value: &JsonValue<'input, 'ast_allocator>,
    ty: &Type<'ast_allocator>,
    type_span: Range<usize>,
    named_type_map: &NamedTypeMap<'input, 'ast_allocator>,
    json_tree_type_cache: &mut JsonTreeTypeCache<'input, 'ast_allocator>,
    errors: &mut Vec<TypeError<'input, 'ast_allocator>, &'ast_allocator Bump>,
    type_allocator: &'ast_allocator Bump,
) {
    if !check_json_type(
        json_value,
        ty,
        named_type_map,
        json_tree_type_cache,
        type_allocator,
    ) {
        let error = TypeError {
            kind: TypeErrorKind::IncompatibleJsonValueType {
                json: json_value.span(),
                expected: Spanned::new(ty.clone(), type_span.clone()),
            },
            span: type_span,
        };
        errors.push(error);
    }
}

fn check_json_type<'input, 'ast_allocator>(
    value: &JsonValue<'input, 'ast_allocator>,
    ty: &Type<'ast_allocator>,
    named_type_map: &NamedTypeMap<'input, 'ast_allocator>,
    json_tree_type_cache: &mut JsonTreeTypeCache<'input, 'ast_allocator>,
    type_allocator: &'ast_allocator Bump,
) -> bool {
    match ty {
        Type::Named(name_id) => check_json_named_type_tree(
            value,
            named_type_map.get_type(*name_id).unwrap(),
            named_type_map,
            json_tree_type_cache,
            type_allocator,
        ),
        Type::Or(items) => items.iter().any(|ty| {
            check_json_type(
                value,
                ty,
                named_type_map,
                json_tree_type_cache,
                type_allocator,
            )
        }),
        Type::Array(base_type) => match value {
            JsonValue::Array(value) => {
                for element in value.elements.iter() {
                    if !check_json_type(
                        element,
                        &base_type,
                        named_type_map,
                        json_tree_type_cache,
                        type_allocator,
                    ) {
                        return false;
                    }
                }
                true
            }
            _ => false,
        },
        Type::Optional(base_type) => match value {
            JsonValue::Value(ValueLiteral::Null(_)) => true,
            _ => check_json_type(
                value,
                &base_type,
                named_type_map,
                json_tree_type_cache,
                type_allocator,
            ),
        },
        Type::Any => true,
        Type::Unknown => true,
        _ => {
            let JsonValue::Value(value_literal) = value else {
                return false;
            };

            let value_infer_type = get_value_type(value_literal, type_allocator);

            if value_infer_type
                .try_override_with(ty, type_allocator)
                .is_err()
            {
                return false;
            } else if !ty
                .validate_value_with_attribute(get_value_literal(value_literal).value.as_ref())
            {
                return false;
            }

            true
        }
    }
}

fn check_json_named_type_tree<'input, 'ast_allocator>(
    value: &JsonValue<'input, 'ast_allocator>,
    type_tree: &NamedTypeTree<'input, 'ast_allocator>,
    named_type_map: &NamedTypeMap<'input, 'ast_allocator>,
    json_tree_type_cache: &mut JsonTreeTypeCache<'input, 'ast_allocator>,
    type_allocator: &'ast_allocator Bump,
) -> bool {
    match type_tree {
        NamedTypeTree::Struct { tree } => check_json_type_tree(
            value,
            tree,
            named_type_map,
            json_tree_type_cache,
            type_allocator,
        ),
        NamedTypeTree::Enum {
            elements,
            documents: _,
        } => match value {
            JsonValue::Value(ValueLiteral::String(string)) => elements
                .iter()
                .map(|element| &element.0.value)
                .any(|element| element.as_ref() == string.value.as_ref()),
            _ => false,
        },
    }
}

fn check_json_type_tree<'input, 'ast_allocator>(
    value: &JsonValue<'input, 'ast_allocator>,
    type_tree: &TypeTree<'input, 'ast_allocator>,
    named_type_map: &NamedTypeMap<'input, 'ast_allocator>,
    json_tree_type_cache: &mut JsonTreeTypeCache<'input, 'ast_allocator>,
    type_allocator: &'ast_allocator Bump,
) -> bool {
    json_tree_type_cache.link_field_completions(value.span(), type_tree);

    match type_tree {
        TypeTree::Node {
            node,
            any_node,
            node_key_span,
            any_node_key_span,
            documents: _,
            span: _,
        } => match value {
            JsonValue::Object(value) => {
                for (element_name, element_type_tree) in node.iter() {
                    match value
                        .elements
                        .iter()
                        .find(|element| element.name.value.as_ref() == element_name.as_ref())
                    {
                        Some(value) => {
                            let define_span =
                                node_key_span.get(element_name.as_ref()).unwrap().clone();
                            json_tree_type_cache.link_user(define_span, value.name.span());

                            if !check_json_type_tree(
                                &value.value,
                                element_type_tree,
                                named_type_map,
                                json_tree_type_cache,
                                type_allocator,
                            ) {
                                return false;
                            }
                        }
                        None => return false,
                    }
                }

                match any_node {
                    Some(any_node_tree) => {
                        for value in value.elements.iter() {
                            if node.contains_key(value.name.value.as_ref()) {
                                continue;
                            }

                            let define_span = any_node_key_span.clone().unwrap();
                            json_tree_type_cache.link_user(define_span.clone(), value.name.span());

                            if !check_json_type_tree(
                                &value.value,
                                &any_node_tree,
                                named_type_map,
                                json_tree_type_cache,
                                type_allocator,
                            ) {
                                return false;
                            }
                        }
                    }
                    None => {
                        if value
                            .elements
                            .iter()
                            .any(|value| !node.contains_key(value.name.value.as_ref()))
                        {
                            return false;
                        }
                    }
                }

                true
            }
            _ => false,
        },
        TypeTree::Leaf {
            ty,
            documents: _,
            span: _,
        } => check_json_type(
            value,
            ty,
            named_type_map,
            json_tree_type_cache,
            type_allocator,
        ),
    }
}

pub fn check_serde_json_type<'input, 'ast_allocator>(
    value: &Value,
    ty: &Type<'ast_allocator>,
    named_type_map: &NamedTypeMap<'input, 'ast_allocator>,
) -> bool {
    match ty {
        Type::Named(name_id) => check_serde_json_named_type_tree(
            value,
            named_type_map.get_type(*name_id).unwrap(),
            named_type_map,
        ),
        Type::Or(items) => items
            .iter()
            .any(|ty| check_serde_json_type(value, ty, named_type_map)),
        Type::Array(base_type) => match value {
            Value::Array(elements) => {
                for element in elements.iter() {
                    if !check_serde_json_type(element, &base_type, named_type_map) {
                        return false;
                    }
                }
                true
            }
            _ => false,
        },
        Type::Optional(base_type) => match value {
            Value::Null => true,
            _ => check_serde_json_type(value, &base_type, named_type_map),
        },
        Type::Any => true,
        Type::Unknown => true,
        _ => {
            let literal = match value {
                Value::Bool(bool) => bool.to_string(),
                Value::Number(number) => number.to_string(),
                Value::String(string) => string.clone(),
                _ => return false,
            };

            if !ty.validate_value_with_attribute(literal.as_str()) {
                return false;
            }

            true
        }
    }
}

fn check_serde_json_named_type_tree<'input, 'ast_allocator>(
    value: &Value,
    type_tree: &NamedTypeTree<'input, 'ast_allocator>,
    named_type_map: &NamedTypeMap<'input, 'ast_allocator>,
) -> bool {
    match type_tree {
        NamedTypeTree::Struct { tree } => check_serde_json_type_tree(value, tree, named_type_map),
        NamedTypeTree::Enum {
            elements,
            documents: _,
        } => match value {
            Value::String(string) => elements
                .iter()
                .map(|element| &element.0.value)
                .any(|element| element.as_ref() == string.as_str()),
            _ => false,
        },
    }
}

fn check_serde_json_type_tree<'input, 'ast_allocator>(
    value: &Value,
    type_tree: &TypeTree<'input, 'ast_allocator>,
    named_type_map: &NamedTypeMap<'input, 'ast_allocator>,
) -> bool {
    match type_tree {
        TypeTree::Node {
            node,
            any_node,
            node_key_span: _,
            any_node_key_span: _,
            documents: _,
            span: _,
        } => match value {
            Value::Object(json_elements) => {
                for (element_name, element_type_tree) in node.iter() {
                    match json_elements.get(element_name.as_ref()) {
                        Some(value) => {
                            if !check_serde_json_type_tree(
                                &value,
                                element_type_tree,
                                named_type_map,
                            ) {
                                return false;
                            }
                        }
                        None => return false,
                    }
                }

                match any_node {
                    Some(any_node_tree) => {
                        for (value_name, value) in json_elements.iter() {
                            if node.contains_key(value_name.as_str()) {
                                continue;
                            }

                            if !check_serde_json_type_tree(value, &any_node_tree, named_type_map) {
                                return false;
                            }
                        }
                    }
                    None => {
                        if json_elements
                            .keys()
                            .any(|name| !node.contains_key(name.as_str()))
                        {
                            return false;
                        }
                    }
                }

                true
            }
            _ => false,
        },
        TypeTree::Leaf {
            ty,
            documents: _,
            span: _,
        } => check_serde_json_type(value, ty, named_type_map),
    }
}

fn get_element_type<'input, 'env, 'ast_allocator>(
    ast: &ElementDefine<'input, 'ast_allocator>,
    name_env: &'env NameEnvironment<'env, 'input>,
    named_type_map: &mut NamedTypeMap<'input, 'ast_allocator>,
    interfaces: &mut Vec<
        InterfaceInfo<'input, 'ast_allocator, 'ast_allocator>,
        &'ast_allocator Bump,
    >,
    errors: &mut Vec<TypeError<'input, 'ast_allocator>, &'ast_allocator Bump>,
    env: &'env Bump,
    ty: &'ast_allocator Bump,
) -> TypeTree<'input, 'ast_allocator> {
    let mut documents = Vec::with_capacity_in(ast.documents.lines.len(), ty);
    for line in ast.documents.lines.iter() {
        documents.push(*line);
    }

    match (&ast.ty, &ast.inline_type, &ast.default) {
        (None, None, Some(default)) => TypeTree::Leaf {
            ty: get_value_type(&default.value, ty),
            documents,
            span: ast.span.clone(),
        },
        (None, Some(inline), None) => resolve_defines_type(
            inline.defines,
            Some(&ast.documents),
            Some(ast.span.clone()),
            name_env,
            named_type_map,
            interfaces,
            errors,
            env,
            ty,
        ),
        (None, Some(inline), Some(_)) => resolve_defines_type(
            inline.defines,
            Some(&ast.documents),
            Some(ast.span.clone()),
            name_env,
            named_type_map,
            interfaces,
            errors,
            env,
            ty,
        ),
        (Some(element_type), None, None) => {
            let ty = resolve_or_type(
                &element_type.type_info,
                name_env,
                named_type_map,
                errors,
                env,
                ty,
            );
            TypeTree::Leaf {
                ty,
                documents,
                span: ast.span.clone(),
            }
        }
        (Some(element_type), None, Some(default)) => {
            let element_type_span = element_type.span.clone();

            let element_type = resolve_or_type(
                &element_type.type_info,
                name_env,
                named_type_map,
                errors,
                env,
                ty,
            );
            let value_type = get_value_type(&default.value, ty);

            let value = get_value_literal(&default.value);

            if value_type.try_override_with(&element_type, ty).is_err() {
                let span = value.span.clone();

                errors.push(TypeError {
                    kind: TypeErrorKind::IncompatibleValueType {
                        value,
                        value_type,
                        expected: Spanned::new(element_type.clone(), element_type_span),
                    },
                    span,
                });
            } else if !element_type.validate_value_with_attribute(value.value.as_ref()) {
                let span = value.span.clone();

                errors.push(TypeError {
                    kind: TypeErrorKind::IncompatibleValueForAttribute {
                        value,
                        expected: Spanned::new(element_type.clone(), element_type_span),
                    },
                    span,
                });
            }

            TypeTree::Leaf {
                ty: element_type,
                documents,
                span: ast.span.clone(),
            }
        }
        _ => TypeTree::Leaf {
            ty: Type::Unknown,
            documents,
            span: ast.span.clone(),
        },
    }
}

fn get_value_type<'input, 'env, 'ast_allocator>(
    ast: &ValueLiteral<'input>,
    ty: &'ast_allocator Bump,
) -> Type<'ast_allocator> {
    match &ast {
        ValueLiteral::String(_) => Type::String(StringAttribute::default()),
        ValueLiteral::Float(float_literal) => match float_literal {
            FloatLiteral::Float(literal) => {
                match (literal.value.parse::<u64>(), literal.value.parse::<i64>()) {
                    (Ok(_), Ok(_)) => Type::MaybeUnsignedInt,
                    (Ok(_), Err(_)) => Type::UnsignedInt(UnsignedIntAttribute::default()),
                    (Err(_), Ok(_)) => Type::MaybeInt,
                    (Err(_), Err(_)) => Type::Float(FloatAttribute::default()),
                }
            }
            FloatLiteral::Inf(_) => Type::Float(FloatAttribute::default()),
            FloatLiteral::Nan(_) => Type::Float(FloatAttribute::default()),
        },
        ValueLiteral::Binary(binary_literal) => {
            let u64_result = match binary_literal {
                BinaryLiteral::Hex(literal) => {
                    u64::from_str_radix(literal.value.replace("0x", "").as_str(), 16)
                }
                BinaryLiteral::Oct(literal) => {
                    u64::from_str_radix(literal.value.replace("0o", "").as_str(), 8)
                }
                BinaryLiteral::Bin(literal) => {
                    u64::from_str_radix(literal.value.replace("0b", "").as_str(), 2)
                }
            };

            let i64_result = match binary_literal {
                BinaryLiteral::Hex(literal) => {
                    i64::from_str_radix(literal.value.replace("0x", "").as_str(), 16)
                }
                BinaryLiteral::Oct(literal) => {
                    i64::from_str_radix(literal.value.replace("0o", "").as_str(), 8)
                }
                BinaryLiteral::Bin(literal) => {
                    i64::from_str_radix(literal.value.replace("0b", "").as_str(), 2)
                }
            };

            match (u64_result, i64_result) {
                (Ok(_), Ok(_)) => Type::MaybeUnsignedInt,
                (Ok(_), Err(_)) => Type::UnsignedInt(UnsignedIntAttribute::default()),
                (Err(_), Ok(_)) => Type::MaybeInt,
                (Err(_), Err(_)) => Type::Float(FloatAttribute::default()),
            }
        }
        ValueLiteral::Bool(_) => Type::Bool,
        ValueLiteral::Null(_) => Type::Optional(Box::new_in(Type::Unknown, ty)),
    }
}

fn get_value_literal<'ast>(ast: &ValueLiteral<'ast>) -> EscapedLiteral<'ast> {
    match ast {
        ValueLiteral::String(literal) => literal.clone(),
        ValueLiteral::Float(float_literal) => match float_literal {
            FloatLiteral::Float(literal) => literal.clone(),
            FloatLiteral::Inf(literal) => literal.clone(),
            FloatLiteral::Nan(literal) => literal.clone(),
        }
        .map(|literal| literal.into()),
        ValueLiteral::Binary(binary_literal) => match binary_literal {
            BinaryLiteral::Hex(literal) => literal.clone(),
            BinaryLiteral::Oct(literal) => literal.clone(),
            BinaryLiteral::Bin(literal) => literal.clone(),
        }
        .map(|literal| literal.into()),
        ValueLiteral::Bool(literal) => literal.clone().map(|literal| literal.into()),
        ValueLiteral::Null(literal) => literal.clone().map(|literal| literal.into()),
    }
}

fn resolve_or_type<'input, 'env, 'ast_allocator>(
    ast: &OrType<'input, 'ast_allocator>,
    name_env: &'env NameEnvironment<'env, 'input>,
    named_type_map: &mut NamedTypeMap<'input, 'ast_allocator>,
    errors: &mut Vec<TypeError<'input, 'ast_allocator>, &'ast_allocator Bump>,
    env: &'env Bump,
    ty: &'ast_allocator Bump,
) -> Type<'ast_allocator> {
    match ast.or_types.len() {
        0 => Type::Unknown,
        1 => resolve_type_base(&ast.or_types[0], name_env, named_type_map, errors, env, ty),
        _ => {
            let mut or_types = Vec::new_in(ty);

            for or_type in ast.or_types.iter() {
                or_types.push(resolve_type_base(
                    or_type,
                    name_env,
                    named_type_map,
                    errors,
                    env,
                    ty,
                ));
            }

            Type::Or(or_types)
        }
    }
}

fn add_attribute_incompatible<'input, 'ast_allocator>(
    errors: &mut Vec<TypeError<'input, 'ast_allocator>, &'ast_allocator Bump>,
    span: Range<usize>,
    ty: &'static str,
) {
    let error = TypeError {
        kind: TypeErrorKind::IncompatibleAttributeForType { ty },
        span,
    };
    errors.push(error);
}

fn to_int_range<T: TryFrom<i128> + Display>(
    attribute: &NumericAttribute,
) -> Option<NumericalValueRange<T>> {
    match &attribute.from_to {
        FromTo::FromToExclusive { from, to } => match (
            from.value.right().map(|int| int.try_into().ok()).flatten(),
            to.value.right().map(|int| int.try_into().ok()).flatten(),
        ) {
            (Some(from), Some(to)) => Some(NumericalValueRange::Range(from..to)),
            _ => None,
        },
        FromTo::FromToInclusive { from, to } => match (
            from.value.right().map(|int| int.try_into().ok()).flatten(),
            to.value.right().map(|int| int.try_into().ok()).flatten(),
        ) {
            (Some(from), Some(to)) => Some(NumericalValueRange::RangeInclusive(from..=to)),
            _ => None,
        },
        FromTo::From { from } => from
            .value
            .right()
            .map(|int| int.try_into().ok())
            .flatten()
            .map(|int| NumericalValueRange::RangeFrom(int..)),
        FromTo::ToExclusive { to } => to
            .value
            .right()
            .map(|int| int.try_into().ok())
            .flatten()
            .map(|int| NumericalValueRange::RangeTo(..int)),
        FromTo::ToInclusive { to } => to
            .value
            .right()
            .map(|int| int.try_into().ok())
            .flatten()
            .map(|int| NumericalValueRange::RangeToInclusive(..=int)),
    }
}

fn to_float_range<T: TryFrom<f64> + Display>(
    attribute: &NumericAttribute,
) -> Option<NumericalValueRange<T>> {
    match &attribute.from_to {
        FromTo::FromToExclusive { from, to } => match (
            from.value
                .left()
                .map(|float| float.try_into().ok())
                .flatten(),
            to.value.left().map(|float| float.try_into().ok()).flatten(),
        ) {
            (Some(from), Some(to)) => Some(NumericalValueRange::Range(from..to)),
            _ => None,
        },
        FromTo::FromToInclusive { from, to } => match (
            from.value
                .left()
                .map(|float| float.try_into().ok())
                .flatten(),
            to.value.left().map(|float| float.try_into().ok()).flatten(),
        ) {
            (Some(from), Some(to)) => Some(NumericalValueRange::RangeInclusive(from..=to)),
            _ => None,
        },
        FromTo::From { from } => from
            .value
            .left()
            .map(|float| float.try_into().ok())
            .flatten()
            .map(|float| NumericalValueRange::RangeFrom(float..)),
        FromTo::ToExclusive { to } => to
            .value
            .left()
            .map(|float| float.try_into().ok())
            .flatten()
            .map(|float| NumericalValueRange::RangeTo(..float)),
        FromTo::ToInclusive { to } => to
            .value
            .left()
            .map(|float| float.try_into().ok())
            .flatten()
            .map(|float| NumericalValueRange::RangeToInclusive(..=float)),
    }
}

fn resolve_type_base<'input, 'env, 'ast_allocator>(
    ast: &BaseType<'input, 'ast_allocator>,
    name_env: &'env NameEnvironment<'env, 'input>,
    named_type_map: &mut NamedTypeMap<'input, 'ast_allocator>,
    errors: &mut Vec<TypeError<'input, 'ast_allocator>, &'ast_allocator Bump>,
    env: &'env Bump,
    ty_allocator: &'ast_allocator Bump,
) -> Type<'ast_allocator> {
    let ty = match &ast.ty {
        Either::Left(base_type) => {
            match base_type.name.value {
                "int" => {
                    fn int_attribute_processor<'input, 'ast_allocator>(
                        attribute: &TypeAttribute<'input, 'ast_allocator>,
                        errors: &mut Vec<TypeError<'input, 'ast_allocator>, &'ast_allocator Bump>,
                    ) -> AttributeTree {
                        match attribute {
                            TypeAttribute::NumericAttribute(attribute) => {
                                match attribute.kind.value {
                                    NumericAttributeKind::Value => match to_int_range(attribute) {
                                        Some(attribute) => AttributeTree::Base {
                                            attribute: Arc::new(AttributeSet::IntValue(attribute)),
                                        },
                                        None => {
                                            add_attribute_incompatible(
                                                errors,
                                                attribute.span.clone(),
                                                "int",
                                            );
                                            AttributeTree::None
                                        }
                                    },
                                    _ => {
                                        add_attribute_incompatible(
                                            errors,
                                            attribute.span.clone(),
                                            "int",
                                        );
                                        AttributeTree::None
                                    }
                                }
                            }
                            TypeAttribute::RegexAttribute(attribute) => {
                                add_attribute_incompatible(errors, attribute.span.clone(), "int");
                                AttributeTree::None
                            }
                            TypeAttribute::AttributeTree(attribute_or) => AttributeTree::Tree {
                                attribute: Arc::new(resolve_attribute_or(
                                    attribute_or,
                                    int_attribute_processor,
                                    errors,
                                )),
                            },
                        }
                    }

                    let attribute = ast.attribute.as_ref().map(|attribute| {
                        resolve_attribute_or(attribute, int_attribute_processor, errors)
                    });

                    Type::Int(IntAttribute {
                        range: attribute.unwrap_or_default(),
                    })
                }
                "uint" => {
                    fn uint_attribute_processor<'input, 'ast_allocator>(
                        attribute: &TypeAttribute<'input, 'ast_allocator>,
                        errors: &mut Vec<TypeError<'input, 'ast_allocator>, &'ast_allocator Bump>,
                    ) -> AttributeTree {
                        match attribute {
                            TypeAttribute::NumericAttribute(attribute) => {
                                match attribute.kind.value {
                                    NumericAttributeKind::Value => match to_int_range(attribute) {
                                        Some(attribute) => AttributeTree::Base {
                                            attribute: Arc::new(AttributeSet::UIntValue(attribute)),
                                        },
                                        None => {
                                            add_attribute_incompatible(
                                                errors,
                                                attribute.span.clone(),
                                                "uint",
                                            );
                                            AttributeTree::None
                                        }
                                    },
                                    _ => {
                                        add_attribute_incompatible(
                                            errors,
                                            attribute.span.clone(),
                                            "uint",
                                        );
                                        AttributeTree::None
                                    }
                                }
                            }
                            TypeAttribute::RegexAttribute(attribute) => {
                                add_attribute_incompatible(errors, attribute.span.clone(), "uint");
                                AttributeTree::None
                            }
                            TypeAttribute::AttributeTree(attribute_or) => AttributeTree::Tree {
                                attribute: Arc::new(resolve_attribute_or(
                                    attribute_or,
                                    uint_attribute_processor,
                                    errors,
                                )),
                            },
                        }
                    }

                    let attribute = ast.attribute.as_ref().map(|attribute| {
                        resolve_attribute_or(attribute, uint_attribute_processor, errors)
                    });

                    Type::UnsignedInt(UnsignedIntAttribute {
                        range: attribute.unwrap_or_default(),
                    })
                }
                "float" => {
                    fn float_attribute_processor<'input, 'ast_allocator>(
                        attribute: &TypeAttribute<'input, 'ast_allocator>,
                        errors: &mut Vec<TypeError<'input, 'ast_allocator>, &'ast_allocator Bump>,
                    ) -> AttributeTree {
                        match attribute {
                            TypeAttribute::NumericAttribute(attribute) => {
                                match attribute.kind.value {
                                    NumericAttributeKind::Value => {
                                        match to_float_range(attribute) {
                                            Some(attribute) => AttributeTree::Base {
                                                attribute: Arc::new(AttributeSet::FloatValue(
                                                    attribute,
                                                )),
                                            },
                                            None => {
                                                add_attribute_incompatible(
                                                    errors,
                                                    attribute.span.clone(),
                                                    "float",
                                                );
                                                AttributeTree::None
                                            }
                                        }
                                    }
                                    _ => {
                                        add_attribute_incompatible(
                                            errors,
                                            attribute.span.clone(),
                                            "float",
                                        );
                                        AttributeTree::None
                                    }
                                }
                            }
                            TypeAttribute::RegexAttribute(attribute) => {
                                add_attribute_incompatible(errors, attribute.span.clone(), "float");
                                AttributeTree::None
                            }
                            TypeAttribute::AttributeTree(attribute_or) => AttributeTree::Tree {
                                attribute: Arc::new(resolve_attribute_or(
                                    attribute_or,
                                    float_attribute_processor,
                                    errors,
                                )),
                            },
                        }
                    }

                    let attribute = ast.attribute.as_ref().map(|attribute| {
                        resolve_attribute_or(attribute, float_attribute_processor, errors)
                    });

                    Type::Float(FloatAttribute {
                        range: attribute.unwrap_or_default(),
                    })
                }
                "string" => {
                    fn string_attribute_processor<'input, 'ast_allocator>(
                        attribute: &TypeAttribute<'input, 'ast_allocator>,
                        errors: &mut Vec<TypeError<'input, 'ast_allocator>, &'ast_allocator Bump>,
                    ) -> AttributeTree {
                        match attribute {
                            TypeAttribute::NumericAttribute(attribute) => {
                                match attribute.kind.value {
                                    NumericAttributeKind::Length => match to_int_range(attribute) {
                                        Some(attribute) => AttributeTree::Base {
                                            attribute: Arc::new(AttributeSet::Length(attribute)),
                                        },
                                        None => {
                                            add_attribute_incompatible(
                                                errors,
                                                attribute.span.clone(),
                                                "string",
                                            );
                                            AttributeTree::None
                                        }
                                    },
                                    NumericAttributeKind::U8Size => match to_int_range(attribute) {
                                        Some(attribute) => AttributeTree::Base {
                                            attribute: Arc::new(AttributeSet::U8Size(attribute)),
                                        },
                                        None => {
                                            add_attribute_incompatible(
                                                errors,
                                                attribute.span.clone(),
                                                "string",
                                            );
                                            AttributeTree::None
                                        }
                                    },
                                    _ => {
                                        add_attribute_incompatible(
                                            errors,
                                            attribute.span.clone(),
                                            "string",
                                        );
                                        AttributeTree::None
                                    }
                                }
                            }
                            TypeAttribute::RegexAttribute(attribute) => {
                                match Regex::new(attribute.regex_literal.value.as_ref()) {
                                    Ok(regex) => AttributeTree::Base {
                                        attribute: Arc::new(AttributeSet::Regex(Arc::new(regex))),
                                    },
                                    Err(_) => {
                                        let error = TypeError {
                                            kind: TypeErrorKind::InvalidRegexAttribute,
                                            span: attribute.span(),
                                        };
                                        errors.push(error);

                                        AttributeTree::None
                                    }
                                }
                            }
                            TypeAttribute::AttributeTree(attribute_or) => AttributeTree::Tree {
                                attribute: Arc::new(resolve_attribute_or(
                                    attribute_or,
                                    string_attribute_processor,
                                    errors,
                                )),
                            },
                        }
                    }

                    let attribute = ast.attribute.as_ref().map(|attribute| {
                        resolve_attribute_or(attribute, string_attribute_processor, errors)
                    });

                    Type::String(StringAttribute {
                        attribute: attribute.unwrap_or_default(),
                    })
                }
                "bool" => Type::Bool,
                "any" => Type::Any,
                _ => {
                    // maybe user type
                    match name_env.resolve(base_type.name.value) {
                        Some(name_id) => {
                            named_type_map.link(name_id, base_type.name.span.clone());

                            Type::Named(name_id)
                        }
                        None => {
                            errors.push(TypeError {
                                kind: TypeErrorKind::UnknownNamedType {
                                    name: base_type.name.clone(),
                                },
                                span: base_type.name.span.clone(),
                            });
                            Type::Unknown
                        }
                    }
                }
            }
        }
        Either::Right(array_type) => {
            let base = resolve_or_type(
                &array_type.base,
                name_env,
                named_type_map,
                errors,
                env,
                ty_allocator,
            );
            Type::Array(Box::new_in(base, ty_allocator))
        }
    };

    match ast.optional.is_some() {
        true => Type::Optional(Box::new_in(ty, ty_allocator)),
        false => ty,
    }
}

fn resolve_attribute_or<'input, 'ast_allocator>(
    ast: &AttributeOr<'input, 'ast_allocator>,
    processor: impl Fn(
            &TypeAttribute<'input, 'ast_allocator>,
            &mut Vec<TypeError<'input, 'ast_allocator>, &'ast_allocator Bump>,
        ) -> AttributeTree
        + Clone,
    errors: &mut Vec<TypeError<'input, 'ast_allocator>, &'ast_allocator Bump>,
) -> AttributeTree {
    AttributeTree::Or {
        attributes: ast
            .attributes
            .iter()
            .map(|attribute| resolve_attribute_and(attribute, processor.clone(), errors))
            .collect(),
    }
}

fn resolve_attribute_and<'input, 'ast_allocator>(
    ast: &AttributeAnd<'input, 'ast_allocator>,
    processor: impl Fn(
            &TypeAttribute<'input, 'ast_allocator>,
            &mut Vec<TypeError<'input, 'ast_allocator>, &'ast_allocator Bump>,
        ) -> AttributeTree
        + Clone,
    errors: &mut Vec<TypeError<'input, 'ast_allocator>, &'ast_allocator Bump>,
) -> AttributeTree {
    AttributeTree::And {
        attributes: ast
            .attributes
            .iter()
            .map(|attribute| processor.clone()(attribute, errors))
            .collect(),
    }
}
