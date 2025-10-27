use tyml_core::tyml_type::types::{NamedTypeMap, NamedTypeTree, Type, TypeTree};

use crate::name::NameContext;

pub(crate) fn generate_type_for_typescript(
    ty: &Type,
    type_def: &mut String,
    name_context: &mut NameContext,
    named_type_map: &NamedTypeMap,
) -> String {
    match ty {
        Type::Int(_) => "number".to_string(),
        Type::UnsignedInt(_) => "number".to_string(),
        Type::Float(_) => "number".to_string(),
        Type::Bool => "boolean".to_string(),
        Type::String(_) => "string".to_string(),
        Type::MaybeInt => "number".to_string(),
        Type::MaybeUnsignedInt => "number".to_string(),
        Type::Named(name_id) => match named_type_map.get_type(*name_id).unwrap() {
            NamedTypeTree::Struct { tree } => {
                let type_name = named_type_map.get_name(*name_id).unwrap();

                let mut new_type_def = String::new();

                let documents = match tree {
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

                *type_def += "/**\n";
                *type_def += documents
                    .iter()
                    .map(|line| format!(" *{}", line))
                    .collect::<Vec<_>>()
                    .join("")
                    .as_str();
                *type_def += " */\n";

                *type_def += format!("export interface {} ", type_name).as_str();
                *type_def += generate_type_tree_for_typescript(
                    tree,
                    &mut new_type_def,
                    1,
                    name_context,
                    named_type_map,
                )
                .as_str();
                *type_def += "\n\n";

                *type_def += new_type_def.as_str();

                type_name.to_string()
            }
            NamedTypeTree::Enum {
                elements,
                documents,
            } => {
                let type_name = named_type_map.get_name(*name_id).unwrap();

                *type_def += "/**\n";
                *type_def += documents
                    .iter()
                    .map(|line| format!(" *{}", line))
                    .collect::<Vec<_>>()
                    .join("")
                    .as_str();
                *type_def += " */\n";

                *type_def += format!(
                    "export type {} = {};\n\n",
                    type_name,
                    elements
                        .iter()
                        .map(|(element, _)| format!(r#""{}""#, &element.value))
                        .collect::<Vec<_>>()
                        .join(" | ")
                )
                .as_str();

                type_name.to_string()
            }
        },
        Type::Or(elements) => elements
            .iter()
            .map(|ty| generate_type_for_typescript(ty, type_def, name_context, named_type_map))
            .collect::<Vec<_>>()
            .join(" | "),
        Type::Array(base_type) => {
            format!(
                "{}[]",
                generate_type_for_typescript(&base_type, type_def, name_context, named_type_map)
            )
        }
        Type::Optional(base_type) => {
            format!(
                "{} | null",
                generate_type_for_typescript(&base_type, type_def, name_context, named_type_map)
            )
        }
        Type::Any => "string".to_string(),
        Type::Unknown => panic!("do not generate with error"),
    }
}

fn generate_type_tree_for_typescript(
    ty: &TypeTree,
    type_def: &mut String,
    indent: usize,
    name_context: &mut NameContext,
    named_type_map: &NamedTypeMap,
) -> String {
    match ty {
        TypeTree::Node {
            node,
            any_node: _,
            node_key_span: _,
            any_node_key_span: _,
            documents: _,
            span: _,
        } => {
            let mut source = String::new();
            source += "{\n";

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

                source += "    ".repeat(indent).as_str();
                source += "/**\n";
                source += documents
                    .iter()
                    .map(|line| format!("{} *{}", "    ".repeat(indent).as_str(), line))
                    .collect::<Vec<_>>()
                    .join("")
                    .as_str();
                source += "    ".repeat(indent).as_str();
                source += " */\n";

                source += "    ".repeat(indent).as_str();
                source += format!(
                    "{}: {};\n",
                    element_name,
                    generate_type_tree_for_typescript(
                        element_type,
                        type_def,
                        indent + 1,
                        name_context,
                        named_type_map
                    )
                )
                .as_str();
            }

            source += "    ".repeat(indent.checked_sub(1).unwrap_or(0)).as_str();
            source += "}";

            source
        }
        TypeTree::Leaf {
            ty,
            documents: _,
            span: _,
        } => generate_type_for_typescript(ty, type_def, name_context, named_type_map),
    }
}

#[cfg(test)]
mod test {
    use tyml_core::{Tyml, tyml_type::types::TypeTree};

    use crate::{general::typescript::generate_type_tree_for_typescript, name::NameContext};

    #[test]
    fn type_gen_ts() {
        let source = r#"
/// User docs
/// yay
type User {
    /// this is id!
    id: {
        /// inner id!
        /// yay!
        id: int
        name: string
    }
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
                let type_name = generate_type_tree_for_typescript(
                    tree,
                    &mut type_def,
                    0,
                    &mut name_context,
                    tyml.named_type_map(),
                );
                println!("{}: {}", name, type_name);
            }
        }

        println!("{}", type_def);
    }
}
