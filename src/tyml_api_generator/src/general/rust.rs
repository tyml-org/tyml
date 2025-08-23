use tyml::tyml_type::types::{
    FloatAttribute, IntAttribute, NamedTypeMap, NamedTypeTree, StringAttribute, Type, TypeTree,
    UnsignedIntAttribute,
};

use crate::name::NameContext;

pub(crate) fn generate_type_for_rust(
    ty: &Type,
    type_def: &mut String,
    name_context: &mut NameContext,
    named_type_map: &NamedTypeMap,
) -> String {
    match ty {
        Type::Int(attribute) => match attribute == &IntAttribute::default() {
            true => "i64".to_string(),
            false => todo!(),
        },
        Type::UnsignedInt(attribute) => match attribute == &UnsignedIntAttribute::default() {
            true => "u64".to_string(),
            false => todo!(),
        },
        Type::Float(attribute) => match attribute == &FloatAttribute::default() {
            true => "f64".to_string(),
            false => todo!(),
        },
        Type::Bool => "bool".to_string(),
        Type::String(attribute) => match attribute == &StringAttribute::default() {
            true => "String".to_string(),
            false => todo!(),
        },
        Type::MaybeInt => "i64".to_string(),
        Type::MaybeUnsignedInt => "u64".to_string(),
        Type::Named(name_id) => {
            let name = named_type_map.get_name(*name_id).unwrap();
            match name_context.is_defined(name) {
                true => name.to_string(),
                false => {
                    match named_type_map.get_type(*name_id).unwrap() {
                        NamedTypeTree::Struct { tree } => {
                            if let TypeTree::Node {
                                node,
                                any_node: _,
                                node_key_span: _,
                                any_node_key_span: _,
                                documents,
                                span: _,
                            } = tree
                            {
                                let documents = documents
                                    .iter()
                                    .map(|line| format!("/// {}", line))
                                    .collect::<Vec<_>>()
                                    .join("");

                                let tag = "#[allow(non_snake_case)]";
                                let derive = "#[derive(Debug, Clone, Deserialize, Serialize)]";

                                let mut new_type_def = String::new();

                                *type_def += format!(
                                    "{}\n{}\n{}\npub struct {} {{",
                                    documents, tag, derive, name
                                )
                                .as_str();

                                for (element_name, element_type) in node.iter() {
                                    let element_type_name = generate_type_tree_for_rust(
                                        element_type,
                                        &mut new_type_def,
                                        name_context,
                                        named_type_map,
                                    );

                                    *type_def +=
                                        format!("   {}: {},\n", element_name, element_type_name)
                                            .as_str();
                                }

                                *type_def += "}\n\n";

                                *type_def += new_type_def.as_str();
                            }
                        }
                        NamedTypeTree::Enum {
                            elements,
                            documents,
                        } => {
                            let documents = documents
                                .iter()
                                .map(|line| format!("/// {}", line))
                                .collect::<Vec<_>>()
                                .join("");

                            let tag = "#[allow(non_snake_case)]";
                            let derive = "#[derive(Debug, Clone, Copy, Deserialize, Serialize)]";

                            *type_def +=
                                format!("{}\n{}\n{}\npub enum {} {{", documents, tag, derive, name)
                                    .as_str();

                            for (element, documents) in elements.iter() {
                                let documents = documents
                                    .iter()
                                    .map(|line| format!("/// {}", line))
                                    .collect::<Vec<_>>()
                                    .join("");

                                *type_def +=
                                    format!("    {}\n{},\n", documents, element.value).as_str();
                            }

                            *type_def += "}\n\n";
                        }
                    }

                    name_context.mark_as_defined(name.to_string());

                    name.to_string()
                }
            }
        }
        Type::Or(elements) => {
            if elements.len() == 1 {
                return generate_type_for_rust(
                    &elements[0],
                    type_def,
                    name_context,
                    named_type_map,
                );
            }

            let type_names = elements
                .iter()
                .map(|ty| generate_type_for_rust(ty, type_def, name_context, named_type_map))
                .collect::<Vec<_>>();

            let or_type_name = type_names.clone().join("Or");

            let or_type_name = name_context.create_name(or_type_name);

            let tag = "#[allow(non_snake_case)]";
            let derive = "#[derive(Debug, Serialize, Deserialize)]\n#[serde(untagged)]";

            *type_def += format!("{}\n{}\npub enum {} {{", tag, derive, or_type_name).as_str();

            return or_type_name;
        }
        Type::Array(ty) => todo!(),
        Type::Optional(_) => todo!(),
        Type::Any => todo!(),
        Type::Unknown => todo!(),
    }
}

pub(crate) fn generate_type_tree_for_rust(
    ty: &TypeTree,
    type_def: &mut String,
    name_context: &mut NameContext,
    named_type_map: &NamedTypeMap,
) -> String {
    match ty {
        TypeTree::Node {
            node,
            any_node,
            node_key_span,
            any_node_key_span,
            documents,
            span,
        } => todo!(),
        TypeTree::Leaf {
            ty,
            documents,
            span,
        } => todo!(),
    }
}
