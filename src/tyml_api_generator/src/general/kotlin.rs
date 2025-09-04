use tyml_core::tyml_type::types::{NamedTypeMap, NamedTypeTree, Type, TypeTree};

use crate::name::NameContext;

pub(crate) fn generate_type_for_kotlin(
    ty: &Type,
    type_def: &mut String,
    name_context: &mut NameContext,
    named_type_map: &NamedTypeMap,
) -> String {
    match ty {
        Type::Int(_) => "Long".to_string(),
        Type::UnsignedInt(_) => panic!("uint is not supported in Kotlin!"),
        Type::Float(_) => "Float".to_string(),
        Type::Bool => "Boolean".to_string(),
        Type::String(_) => "String".to_string(),
        Type::MaybeInt => "Long".to_string(),
        Type::MaybeUnsignedInt => "Long".to_string(),
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
                                *type_def += "/**\n";
                                *type_def += documents
                                    .iter()
                                    .map(|line| format!(" *{}", line))
                                    .collect::<Vec<_>>()
                                    .join("")
                                    .as_str();
                                *type_def += " */\n";

                                let mut new_type_def = String::new();

                                *type_def +=
                                    format!("@Serializable\ndata class {} (\n", name).as_str();

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

                                    *type_def += "    /**\n";
                                    *type_def += documents
                                        .iter()
                                        .map(|line| format!("     *{}", line))
                                        .collect::<Vec<_>>()
                                        .join("")
                                        .as_str();
                                    *type_def += "     */\n";

                                    let element_type_name = generate_type_tree_for_kotlin(
                                        element_type,
                                        &mut new_type_def,
                                        name_context,
                                        named_type_map,
                                    );

                                    *type_def += format!(
                                        r#"    @SerialName("{}") val {}: {}"#,
                                        element_name,
                                        snake_to_camel(element_name),
                                        element_type_name
                                    )
                                    .as_str();

                                    if let TypeTree::Leaf {
                                        ty: Type::Optional(_),
                                        documents: _,
                                        span: _,
                                    } = element_type
                                    {
                                        *type_def += " = null";
                                    }

                                    *type_def += ",\n";
                                }

                                *type_def += ")\n\n";

                                *type_def += new_type_def.as_str();
                            }
                        }
                        NamedTypeTree::Enum {
                            elements,
                            documents,
                        } => {
                            *type_def += "/**\n";
                            *type_def += documents
                                .iter()
                                .map(|line| format!(" *{}", line))
                                .collect::<Vec<_>>()
                                .join("")
                                .as_str();
                            *type_def += " */\n";

                            *type_def +=
                                format!("@Serializable\nenum class {} {{\n", name).as_str();

                            for (element, documents) in elements.iter() {
                                *type_def += "    /**\n";
                                *type_def += documents
                                    .iter()
                                    .map(|line| format!("     *{}", line))
                                    .collect::<Vec<_>>()
                                    .join("")
                                    .as_str();
                                *type_def += "     */\n";

                                *type_def += format!(
                                    r#"    @SerialName("{}") {},"#,
                                    element.value,
                                    element.value.to_ascii_uppercase()
                                )
                                .as_str();
                                *type_def += "\n";
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
                return generate_type_for_kotlin(
                    &elements[0],
                    type_def,
                    name_context,
                    named_type_map,
                );
            }

            let type_names = elements
                .iter()
                .map(|ty| generate_type_for_kotlin(ty, type_def, name_context, named_type_map))
                .collect::<Vec<_>>();

            let or_type_name = type_names.clone().join("Or");

            let or_type_name = name_context.create_name(or_type_name);

            *type_def += format!(
                "@Serializable(with = {}.Serializer::class)\nsealed interface {} {{\n",
                &or_type_name, &or_type_name
            )
            .as_str();

            for type_name in type_names.iter() {
                *type_def += format!(
                    "    data class {}{}(val value: {}) : {}\n",
                    &or_type_name,
                    type_name.replace("?", ""),
                    type_name,
                    &or_type_name
                )
                .as_str();
            }

            *type_def += format!(
                "    object Serializer : KSerializer<{}> {{\n",
                &or_type_name
            )
            .as_str();

            *type_def += "        @OptIn(InternalSerializationApi::class)\n";

            *type_def += format!(
                r#"        override val descriptor: SerialDescriptor = buildSerialDescriptor("{}", SerialKind.CONTEXTUAL)"#,
                &or_type_name
            ).as_str();
            *type_def += "\n";

            *type_def += format!(
                "        override fun serialize(encoder: Encoder, value: {}) {{\n",
                &or_type_name
            )
            .as_str();

            *type_def += format!(
                r#"            val je = encoder as? JsonEncoder ?: error("{} supports JSON only")"#,
                &or_type_name
            )
            .as_str();
            *type_def += "\n";

            *type_def += "            val elem: JsonElement = when (value) {\n";

            for (ty, type_name) in elements.iter().zip(type_names.iter()) {
                *type_def +=
                    format!("                is {}{} -> ", &or_type_name, type_name).as_str();

                match ty {
                    Type::Int(_)
                    | Type::UnsignedInt(_)
                    | Type::Float(_)
                    | Type::Bool
                    | Type::String(_)
                    | Type::MaybeInt
                    | Type::MaybeUnsignedInt => {
                        *type_def += "JsonPrimitive(value.value)\n";
                    }
                    _ => {
                        *type_def += format!(
                            "je.json.encodeToJsonElement({}.serializer(), value.value)\n",
                            type_name
                        )
                        .as_str();
                    }
                }
            }

            *type_def += "            }\n";

            *type_def += "            je.encodeJsonElement(elem)\n";

            *type_def += "        }\n";

            *type_def += format!(
                "        override fun deserialize(decoder: Decoder): {} {{\n",
                &or_type_name
            )
            .as_str();

            *type_def += format!(
                r#"            val jd = decoder as? JsonDecoder ?: error("{} supports JSON only")"#,
                &or_type_name
            )
            .as_str();
            *type_def += "\n";

            *type_def += "            val el = jd.decodeJsonElement()\n";

            for (ty, type_name) in elements.iter().zip(type_names.iter()) {
                let (is_optional, ty) = match ty {
                    Type::Optional(ty) => (true, ty.as_ref()),
                    _ => (false, ty),
                };

                match ty {
                    Type::Int(_) | Type::MaybeInt | Type::MaybeUnsignedInt => {
                        *type_def += "            if (el is JsonPrimitive) {\n";

                        *type_def += format!(
                            "                el.longOrNull?.let {{ return {}{}(it) }}\n",
                            &or_type_name, type_name
                        )
                        .as_str();
                    }
                    Type::UnsignedInt(_) => unreachable!(),
                    Type::Float(_) => {
                        *type_def += "            if (el is JsonPrimitive) {\n";

                        *type_def += format!(
                            "                el.doubleOrNull?.let {{ return {}{}(it) }}\n",
                            &or_type_name, type_name
                        )
                        .as_str();
                    }
                    Type::Bool => {
                        *type_def += "            if (el is JsonPrimitive) {\n";

                        *type_def += format!(
                            "                el.booleanOrNull?.let {{ return {}{}(it) }}\n",
                            &or_type_name, type_name
                        )
                        .as_str();
                    }
                    Type::String(_) => {
                        *type_def += "            if (el is JsonPrimitive && el.isString) {\n";

                        *type_def += format!(
                            "                return {}{}(el.content)\n",
                            &or_type_name, type_name
                        )
                        .as_str();
                    }
                    Type::Optional(_) => unreachable!(),
                    _ => {
                        *type_def += "            if (el is JsonObject) {\n";

                        *type_def += format!(
                            "                    try {{
                    val p = jd.json.decodeFromJsonElement({}.serializer(), el)
                    return {}{}(p)
                }} catch (_: SerializationException) {{}}\n",
                            type_name, &or_type_name, type_name
                        )
                        .as_str();
                    }
                }

                *type_def += "            }\n";

                if is_optional {
                    *type_def += "            if (el is JsonNull) {\n";

                    *type_def += format!(
                        "                return {}{}(null)\n",
                        &or_type_name, type_name
                    )
                    .as_str();

                    *type_def += "            }\n";
                }
            }

            *type_def += r#"            throw SerializationException("No matching untagged variant for: $el")"#;
            *type_def += "\n";

            *type_def += "        }\n";
            *type_def += "    }\n";

            *type_def += "}\n\n";

            return or_type_name;
        }
        Type::Array(ty) => {
            let base_type_name =
                generate_type_for_kotlin(&ty, type_def, name_context, named_type_map);

            format!("List<{}>", base_type_name)
        }
        Type::Optional(ty) => {
            let base_type_name =
                generate_type_for_kotlin(&ty, type_def, name_context, named_type_map);

            format!("{}?", base_type_name)
        }
        Type::Any => "String".to_string(),
        Type::Unknown => panic!("do not generate with error"),
    }
}

fn generate_type_tree_for_kotlin(
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

            *type_def += "/**\n";
            *type_def += documents
                .iter()
                .map(|line| format!(" *{}", line))
                .collect::<Vec<_>>()
                .join("")
                .as_str();
            *type_def += " */\n";

            let mut new_type_def = String::new();

            *type_def += format!("@Serializable\ndata class {} (\n", &type_name).as_str();

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

                *type_def += "    /**\n";
                *type_def += documents
                    .iter()
                    .map(|line| format!("     *{}", line))
                    .collect::<Vec<_>>()
                    .join("")
                    .as_str();
                *type_def += "     */\n";

                let element_type_name = generate_type_tree_for_kotlin(
                    element_type,
                    &mut new_type_def,
                    name_context,
                    named_type_map,
                );

                *type_def += format!(
                    r#"    @SerialName("{}") val {}: {}"#,
                    element_name,
                    snake_to_camel(element_name),
                    element_type_name
                )
                .as_str();

                if let TypeTree::Leaf {
                    ty: Type::Optional(_),
                    documents: _,
                    span: _,
                } = element_type
                {
                    *type_def += " = null";
                }

                *type_def += ",\n";
            }

            *type_def += ")\n\n";

            *type_def += new_type_def.as_str();

            type_name
        }
        TypeTree::Leaf {
            ty,
            documents: _,
            span: _,
        } => generate_type_for_kotlin(ty, type_def, name_context, named_type_map),
    }
}

pub(crate) fn snake_to_camel(s: &str) -> String {
    let mut parts = s.split('_').filter(|p| !p.is_empty());

    let mut out = String::new();
    if let Some(first) = parts.next() {
        out.push_str(&first.to_lowercase());
    }
    for part in parts {
        let mut chars = part.chars();
        if let Some(c0) = chars.next() {
            out.extend(c0.to_uppercase());
            out.push_str(&chars.as_str().to_lowercase());
        }
    }
    out
}

#[cfg(test)]
mod test {
    use tyml_core::{Tyml, tyml_type::types::TypeTree};

    use crate::{general::kotlin::generate_type_tree_for_kotlin, name::NameContext};

    #[test]
    fn type_gen_kotlin() {
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
                let type_name = generate_type_tree_for_kotlin(
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
