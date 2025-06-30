use std::{fmt::Display, ops::Range, sync::Arc};

use allocator_api2::{boxed::Box, vec::Vec};
use bumpalo::Bump;
use either::Either;
use hashbrown::HashMap;
use regex::Regex;
use tyml_parser::ast::{
    AttributeAnd, AttributeOr, BaseType, BinaryLiteral, DefaultValue, Define, Defines, Documents,
    ElementDefine, FloatLiteral, FromTo, Literal, NodeLiteral, NumericAttribute,
    NumericAttributeKind, OrType, Spanned, TypeAttribute, TypeDefine, ValueLiteral, AST,
};

use crate::{
    error::{TypeError, TypeErrorKind},
    name::{NameEnvironment, NameID},
    types::{
        AttributeSet, AttributeTree, FloatAttribute, IntAttribute, NamedTypeMap, NamedTypeTree,
        NumericalValueRange, StringAttribute, Type, TypeTree, UnsignedIntAttribute,
    },
};

pub fn resolve_type<'input, 'ast_allocator>(
    ast: &'ast_allocator Defines<'input, 'ast_allocator>,
    type_allocator: &'ast_allocator Bump,
) -> (
    TypeTree<'input, 'ast_allocator>,
    NamedTypeMap<'input, 'ast_allocator>,
    Vec<TypeError<'input, 'ast_allocator>, &'ast_allocator Bump>,
) {
    let env_allocator = Bump::new();
    let name_env = NameEnvironment::new(None, &env_allocator);

    let mut named_type_map = NamedTypeMap::new(type_allocator);
    let mut errors = Vec::new_in(type_allocator);

    let root_tree = resolve_defines_type(
        ast,
        None,
        None,
        name_env,
        &mut named_type_map,
        &mut errors,
        &env_allocator,
        type_allocator,
    );

    (root_tree, named_type_map, errors)
}

fn resolve_defines_type<'input, 'env, 'ast_allocator>(
    ast: &Defines<'input, 'ast_allocator>,
    documents: Option<&Documents<'input, 'ast_allocator>>,
    tree_span: Option<Range<usize>>,
    name_env: &'env NameEnvironment<'env, 'input>,
    named_type_map: &mut NamedTypeMap<'input, 'ast_allocator>,
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

    for define in ast.defines.iter() {
        match define {
            Define::Element(element_define) => {
                let element_type =
                    get_element_type(element_define, name_env, named_type_map, errors, env, ty);

                match &element_define.node {
                    NodeLiteral::Literal(name) => {
                        node.insert(name.value.clone(), element_type);
                    }
                    NodeLiteral::Asterisk(_) => {
                        any_node = Some(Box::new_in(element_type, ty));
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
        documents: tree_documents,
        span: tree_span.unwrap_or(ast.span.clone()),
    }
}

fn get_element_type<'input, 'env, 'ast_allocator>(
    ast: &ElementDefine<'input, 'ast_allocator>,
    name_env: &'env NameEnvironment<'env, 'input>,
    named_type_map: &mut NamedTypeMap<'input, 'ast_allocator>,
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
            ty: get_value_type(default, ty),
            documents,
            span: ast.span.clone(),
        },
        (None, Some(inline), None) => resolve_defines_type(
            inline.defines,
            Some(&ast.documents),
            Some(ast.span.clone()),
            name_env,
            named_type_map,
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
            let value_type = get_value_type(default, ty);

            let value = get_value_literal(&default);

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
            } else if !element_type.validate_value_with_attribute(value.value) {
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
    ast: &DefaultValue<'input>,
    ty: &'ast_allocator Bump,
) -> Type<'ast_allocator> {
    match &ast.value {
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
        ValueLiteral::Null(_) => Type::Optional(Box::new_in(Type::Unknown, ty)),
    }
}

fn get_value_literal<'ast>(ast: &DefaultValue<'ast>) -> Literal<'ast> {
    match &ast.value {
        ValueLiteral::String(literal) => literal.clone(),
        ValueLiteral::Float(float_literal) => match float_literal {
            FloatLiteral::Float(literal) => literal.clone(),
            FloatLiteral::Inf(literal) => literal.clone(),
            FloatLiteral::Nan(literal) => literal.clone(),
        },
        ValueLiteral::Binary(binary_literal) => match binary_literal {
            BinaryLiteral::Hex(literal) => literal.clone(),
            BinaryLiteral::Oct(literal) => literal.clone(),
            BinaryLiteral::Bin(literal) => literal.clone(),
        },
        ValueLiteral::Null(literal) => literal.clone(),
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
