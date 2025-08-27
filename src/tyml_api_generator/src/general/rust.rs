use tyml_core::tyml_type::types::{NamedTypeMap, NamedTypeTree, Type, TypeTree};

use crate::name::NameContext;

pub(crate) fn generate_type_for_rust(
    ty: &Type,
    type_def: &mut String,
    name_context: &mut NameContext,
    named_type_map: &NamedTypeMap,
) -> String {
    match ty {
        Type::Int(_) => "i64".to_string(),
        Type::UnsignedInt(_) => "u64".to_string(),
        Type::Float(_) => "f64".to_string(),
        Type::Bool => "bool".to_string(),
        Type::String(_) => "String".to_string(),
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
                                    .map(|line| format!("///{}", line))
                                    .collect::<Vec<_>>()
                                    .join("");

                                let tag = "#[allow(non_snake_case)]";
                                let derive = "#[derive(Debug, Clone, Deserialize, Serialize)]";

                                let mut new_type_def = String::new();

                                *type_def += format!(
                                    "{}{}\n{}\npub struct {} {{\n",
                                    documents, tag, derive, name
                                )
                                .as_str();

                                for (element_name, element_type) in node.iter() {
                                    let documents = match element_type {
                                        TypeTree::Node {
                                            node: _,
                                            any_node: _,
                                            node_key_span: _,
                                            any_node_key_span: _,
                                            documents,
                                            span: _,
                                        } => documents,
                                        TypeTree::Leaf {
                                            ty: _,
                                            documents,
                                            span: _,
                                        } => documents,
                                    };

                                    *type_def += documents
                                        .iter()
                                        .map(|line| format!("    ///{}", line))
                                        .collect::<Vec<_>>()
                                        .join("")
                                        .as_str();

                                    let element_type_name = generate_type_tree_for_rust(
                                        element_type,
                                        &mut new_type_def,
                                        name_context,
                                        named_type_map,
                                    );

                                    *type_def += format!(
                                        "    pub {}: {},\n",
                                        element_name, element_type_name
                                    )
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
                                .map(|line| format!("///{}", line))
                                .collect::<Vec<_>>()
                                .join("");

                            let tag = "#[allow(non_snake_case)]";
                            let derive = "#[derive(Debug, Clone, Copy, Deserialize, Serialize)]";

                            *type_def +=
                                format!("{}{}\n{}\npub enum {} {{\n", documents, tag, derive, name)
                                    .as_str();

                            for (element, documents) in elements.iter() {
                                let documents = documents
                                    .iter()
                                    .map(|line| format!("///{}", line))
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

            // into "U"pper
            let pretty_type_names = type_names
                .iter()
                .map(|name| {
                    let mut it = name.chars();
                    it.next()
                        .unwrap()
                        .to_uppercase()
                        .chain(it)
                        .collect::<String>()
                })
                .collect::<Vec<_>>();

            let or_type_name = pretty_type_names.clone().join("Or");

            let or_type_name = name_context.create_name(or_type_name);

            let tag = "#[allow(non_snake_case)]";
            let derive = "#[derive(Debug, Clone, Serialize, Deserialize)]\n#[serde(untagged)]";

            *type_def += format!("{}\n{}\npub enum {} {{\n", tag, derive, or_type_name).as_str();

            for (prerry_type_name, type_name) in
                pretty_type_names.into_iter().zip(type_names.into_iter())
            {
                *type_def += format!("    {}({}),\n", prerry_type_name, type_name).as_str();
            }

            *type_def += "}\n\n";

            return or_type_name;
        }
        Type::Array(ty) => {
            let base_type_name =
                generate_type_for_rust(&ty, type_def, name_context, named_type_map);

            format!("Vec<{}>", base_type_name)
        }
        Type::Optional(ty) => {
            let base_type_name =
                generate_type_for_rust(&ty, type_def, name_context, named_type_map);

            format!("Option<{}>", base_type_name)
        }
        Type::Any => "String".to_string(),
        Type::Unknown => panic!("do not generate with error"),
    }
}

fn generate_type_tree_for_rust(
    ty: &TypeTree,
    type_def: &mut String,
    name_context: &mut NameContext,
    named_type_map: &NamedTypeMap,
) -> String {
    match ty {
        TypeTree::Node {
            node,
            any_node: _,
            node_key_span: _,
            any_node_key_span: _,
            documents,
            span: _,
        } => {
            let type_name = name_context.create_name("InnerTree".to_string());

            let documents = documents
                .iter()
                .map(|line| format!("///{}", line))
                .collect::<Vec<_>>()
                .join("");

            let tag = "#[allow(non_snake_case)]";
            let derive = "#[derive(Debug, Clone, Deserialize, Serialize)]";

            let mut new_type_def = String::new();

            *type_def += format!(
                "{}{}\n{}\npub struct {} {{\n",
                documents, tag, derive, type_name
            )
            .as_str();

            for (element_name, element_type) in node.iter() {
                let documents = match element_type {
                    TypeTree::Node {
                        node: _,
                        any_node: _,
                        node_key_span: _,
                        any_node_key_span: _,
                        documents,
                        span: _,
                    } => documents,
                    TypeTree::Leaf {
                        ty: _,
                        documents,
                        span: _,
                    } => documents,
                };

                *type_def += documents
                    .iter()
                    .map(|line| format!("    ///{}", line))
                    .collect::<Vec<_>>()
                    .join("")
                    .as_str();

                let element_type_name = generate_type_tree_for_rust(
                    element_type,
                    &mut new_type_def,
                    name_context,
                    named_type_map,
                );

                *type_def += format!("    pub {}: {},\n", element_name, element_type_name).as_str();
            }

            *type_def += "}\n\n";

            *type_def += new_type_def.as_str();

            type_name
        }
        TypeTree::Leaf {
            ty,
            documents: _,
            span: _,
        } => generate_type_for_rust(ty, type_def, name_context, named_type_map),
    }
}

#[cfg(test)]
mod test {
    use tyml_core::{Tyml, tyml_type::types::TypeTree};

    use crate::{general::rust::generate_type_tree_for_rust, name::NameContext};

    #[test]
    fn type_gen() {
        let source = r#"
/// User docs
/// yay
type User {
    /// this is id!
    id: int
    name: string | Name
}

type Name {
    name: string
    display_name: string
}

user: User
        "#;

        let tyml = Tyml::parse(source.to_string());

        let mut type_def = String::new();
        let mut name_context = NameContext::new();

        if let TypeTree::Node {
            node,
            any_node: _,
            node_key_span: _,
            any_node_key_span: _,
            documents: _,
            span: _,
        } = tyml.type_tree()
        {
            for (name, tree) in node.iter() {
                let type_name = generate_type_tree_for_rust(
                    tree,
                    &mut type_def,
                    &mut name_context,
                    tyml.named_type_map(),
                );
                println!("{}: {}", name, type_name);
            }
        }

        println!("{}", type_def);
    }
}
