use std::fs::{self};

use tyml_core::Tyml;

use crate::{GeneratorSettings, general::rust::generate_type_for_rust, name::NameContext};

pub(crate) fn generate_functions_for_rust_axum(
    setting: &GeneratorSettings,
    tyml: &Tyml,
) -> Result<(), Box<dyn std::error::Error>> {
    let source = generate_trait(tyml);

    let mut path = setting.package_path.clone();
    path.push("src");
    path.push("types.rs");

    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }

    fs::write(path, source)?;

    Ok(())
}

fn generate_trait(tyml: &Tyml) -> String {
    let mut source = String::new();
    let mut type_def = String::new();
    let mut name_context = NameContext::new();

    source += "use serde::{Serialize, Deserialize};\n";
    source += "use async_trait::async_trait;\n\n";

    for interface in tyml.interfaces().iter() {
        source += interface
            .documents
            .iter()
            .map(|line| format!("///{}", line))
            .collect::<Vec<_>>()
            .join("")
            .as_str();
        source += "#[async_trait]\n";
        source += format!(
            "pub trait {}: Send + Sync + 'static {{\n",
            interface.original_name
        )
        .as_str();

        for function in interface.functions.iter() {
            source += function
                .documents
                .iter()
                .map(|line| format!("    ///{}", line))
                .collect::<Vec<_>>()
                .join("")
                .as_str();
            source += format!("    async fn {}(", function.name.value.as_str()).as_str();

            let mut arguments = Vec::new();
            arguments.push("&self".to_string());

            if let Some(claim) = &function.claim_argument_info {
                arguments.push(format!(
                    "claim: {}",
                    generate_type_for_rust(
                        &claim.ty,
                        &mut type_def,
                        &mut name_context,
                        tyml.named_type_map()
                    )
                ));
            }

            if let Some(body) = &function.body_argument_info {
                arguments.push(format!(
                    "body: {}",
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

            match &function.return_info {
                Some(return_type) => match &function.throws_type {
                    Some(throws_type) => {
                        source += format!(
                            " -> Result<{}, {}>",
                            generate_type_for_rust(
                                &return_type.ty,
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
                    None => {
                        source += format!(
                            " -> {}",
                            generate_type_for_rust(
                                &return_type.ty,
                                &mut type_def,
                                &mut name_context,
                                tyml.named_type_map()
                            )
                        )
                        .as_str();
                    }
                },
                None => match &function.throws_type {
                    Some(throws_type) => {
                        source += format!(
                            " -> Result<(), {}>",
                            generate_type_for_rust(
                                throws_type,
                                &mut type_def,
                                &mut name_context,
                                tyml.named_type_map()
                            )
                        )
                        .as_str();
                    }
                    None => {}
                },
            }

            source += ";\n"
        }

        source += "}\n\n";
    }

    let has_auth = tyml
        .interfaces()
        .iter()
        .map(|interface| interface.functions.iter())
        .flatten()
        .any(|function| function.authed.is_some());

    if has_auth {
        type_def += r#"

pub(crate) fn __bearer(headers: &axum::http::HeaderMap) -> Option<&str> {
    let auth = headers.get(axum::http::header::AUTHORIZATION)?.to_str().ok()?;
    let prefix = "Bearer ";
    auth.strip_prefix(prefix).or_else(|| auth.strip_prefix(prefix.to_lowercase().as_str()))
}

"#;

        type_def += r#"
/// Implement this for your server struct.
///
/// ## Example
/// ```
/// impl JwtValidator for YourServerStruct {
///     fn validate<T: serde::de::DeserializeOwned>(token: &str) -> Result<T, ()> {
///         let claim = jsonwebtoken::decode(
///             token,
///             &DecodingKey::from_secret("* your secret key *".as_bytes()),
///             &Validation::default(),
///         )
///         .map_err(|_| ())?
///         .claims;
///
///         Ok(claim)
///     }
/// }
/// ```
pub trait JwtValidator {
    fn validate<T: serde::de::DeserializeOwned>(token: &str) -> Result<T, ()>;
}
"#;
    }

    format!("{}\n\n{}", type_def, source)
}

#[cfg(test)]
mod test {
    use tyml_core::Tyml;

    use crate::server::rust_axum::function_gen::generate_trait;

    #[test]
    fn rust_trait_gen() {
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

        println!("{}", generate_trait(&tyml));
    }
}
