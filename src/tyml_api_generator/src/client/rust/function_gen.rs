use std::fs;

use tyml_core::{Tyml, tyml_type::types::FunctionKind};

use crate::{GeneratorSettings, general::rust::generate_type_for_rust, name::NameContext};

pub(crate) fn generate_functions_for_rust_client(
    setting: &GeneratorSettings,
    tyml: &Tyml,
) -> Result<(), Box<dyn std::error::Error>> {
    let source = generate_struct(tyml);

    let mut path = setting.package_path.clone();
    path.push("src");
    path.push("types.rs");

    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }

    fs::write(path, source)?;

    Ok(())
}

fn generate_struct(tyml: &Tyml) -> String {
    let mut source = String::new();
    let mut type_def = String::new();
    let mut name_context = NameContext::new();

    source += "use serde::{Deserialize, Serialize};\n";

    type_def += "
pub enum ThrowsOrOtherError<T> {
    Throws(T),
    Other(reqwest::Error),
}

";

    for interface in tyml.interfaces().iter() {
        source += interface
            .documents
            .iter()
            .map(|line| format!("///{}", line))
            .collect::<Vec<_>>()
            .join("")
            .as_str();

        source += format!("pub struct {} {{\n", interface.original_name).as_str();
        source += "    pub url: String,\n";
        source += "}\n\n";

        source += format!("impl {} {{\n", interface.original_name).as_str();

        for function in interface.functions.iter() {
            source += function
                .documents
                .iter()
                .map(|line| format!("    ///{}", line))
                .collect::<Vec<_>>()
                .join("")
                .as_str();

            source += format!("    pub async fn {}(", &function.name.value).as_str();

            let mut arguments = Vec::new();

            arguments.push("&self".to_string());

            if let Some(_) = &function.claim_argument_info {
                arguments.push("__token: impl Into<String>".to_string());
            }

            if let Some(body) = &function.body_argument_info {
                arguments.push(format!(
                    "__body: {}",
                    generate_type_for_rust(
                        &body.ty,
                        &mut type_def,
                        &mut name_context,
                        tyml.named_type_map()
                    )
                ));
            }

            for argument in function.arguments.iter() {
                arguments.push(format!(
                    "{}: {}",
                    &argument.name.value,
                    generate_type_for_rust(
                        &argument.ty,
                        &mut type_def,
                        &mut name_context,
                        tyml.named_type_map()
                    )
                ));
            }

            source += arguments.join(", ").as_str();
            source += ")";

            match (&function.return_info, &function.throws_type) {
                (None, None) => {
                    source += " -> Result<(), reqwest::Error> {\n";
                }
                (None, Some(throws_type)) => {
                    source += format!(
                        " -> Result<(), ThrowsOrOtherError<{}>> {{\n",
                        generate_type_for_rust(
                            throws_type,
                            &mut type_def,
                            &mut name_context,
                            tyml.named_type_map()
                        )
                    )
                    .as_str();
                }
                (Some(return_info), None) => {
                    source += format!(
                        " -> Result<{}, reqwest::Error> {{\n",
                        generate_type_for_rust(
                            &return_info.ty,
                            &mut type_def,
                            &mut name_context,
                            tyml.named_type_map()
                        )
                    )
                    .as_str();
                }
                (Some(return_info), Some(throws_type)) => {
                    source += format!(
                        " -> Result<{}, ThrowsOrOtherError<{}>> {{\n",
                        generate_type_for_rust(
                            &return_info.ty,
                            &mut type_def,
                            &mut name_context,
                            tyml.named_type_map()
                        ),
                        generate_type_for_rust(
                            throws_type,
                            &mut type_def,
                            &mut name_context,
                            tyml.named_type_map()
                        )
                    )
                    .as_str();
                }
            }

            source += "        let client = reqwest::Client::new();\n";

            let method = match function.kind {
                FunctionKind::GET => "get",
                FunctionKind::PUT => "put",
                FunctionKind::POST => "post",
                FunctionKind::PATCH => "patch",
                FunctionKind::DELETE => "delete",
            };

            source += format!(
                r#"        let mut request = client.{}(format!("{{}}/{}/{}", &self.url));"#,
                method, &interface.name.value, &function.name.value
            )
            .as_str();
            source += "\n";

            if let Some(_) = &function.claim_argument_info {
                source += "        request = request.bearer_auth(__token.into());\n";
            }

            let query = function
                .arguments
                .iter()
                .map(|argument| argument.name.value.as_ref())
                .map(|name| format!(r#"("{}", serde_json::to_string(&{}).unwrap())"#, name, name))
                .collect::<Vec<_>>()
                .join(", ");

            if !function.arguments.is_empty() {
                source += format!("        request = request.query(&[{}]);\n", query).as_str();
            }

            source += r#"        request = request.header("Accept", "application/json");"#;
            source += "\n";

            if let Some(_) = &function.body_argument_info {
                source += "        request = request.json(__body);\n";
            }

            match (&function.return_info, &function.throws_type) {
                (None, None) => {
                    source += "        let response = request.send().await?;\n";

                    source += "
        match response.error_for_status_ref() {
            Ok(_) => {
                Ok(())
            }
            Err(error) => {
                Err(error)
            }
        }
";
                }
                (None, Some(_)) => {
                    source += "        let response = request.send().await.map_err(|error| ThrowsOrOtherError::Other(error))?;\n";

                    source += "
        match response.error_for_status_ref() {
            Ok(_) => {
                Ok(())
            }
            Err(error) => {
                match response.json().await {
                    Ok(value) => Err(ThrowsOrOtherError::Throws(value)),
                    Err(_) => Err(ThrowsOrOtherError::Other(error)),
                }
            }
        }
";
                }
                (Some(_), None) => {
                    source += "        let response = request.send().await?;\n";

                    source += "
        match response.error_for_status_ref() {
            Ok(_) => {
                response.json().await
            }
            Err(error) => {
                Err(error)
            }
        }
";
                }
                (Some(_), Some(_)) => {
                    source += "        let response = request.send().await.map_err(|error| ThrowsOrOtherError::Other(error))?;\n";

                    source += "
        match response.error_for_status_ref() {
            Ok(_) => {
                response.json().await.map_err(|error| ThrowsOrOtherError::Other(error))
            }
            Err(error) => {
                match response.json().await {
                    Ok(value) => Err(ThrowsOrOtherError::Throws(value)),
                    Err(_) => Err(ThrowsOrOtherError::Other(error)),
                }
            }
        }
";
                }
            }

            source += "    }\n";
        }

        source += "}\n";
    }

    format!("{}\n\n{}", type_def, source)
}

#[cfg(test)]
mod test {
    use tyml_core::Tyml;

    use crate::client::rust::function_gen::generate_struct;

    #[test]
    fn rust_struct_gen() {
        let source = r#"
/// the User!
/// Yes!!
type User {
    /// the id!
    id: int
    name: string | Name
}

type Name {
    name: string
    display_name: string
}

type Claim {
    iss: string
    sub: int
    iat: int
    exp: int
}

/// API!
interface API {
    /// get_user!
    authed function get_user(@claim: Claim) -> User throws string
}
        "#;

        let tyml = Tyml::parse(source.to_string());

        println!("{}", generate_struct(&tyml));
    }
}
