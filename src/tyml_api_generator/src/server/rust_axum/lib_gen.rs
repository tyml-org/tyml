use std::fs;
use tyml::Tyml;

use crate::GeneratorSettings;

pub(crate) fn generate_lib(
    setting: &GeneratorSettings,
    tyml: &Tyml,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut path = setting.package_path.clone();
    path.push("src");
    path.push("lib.rs");

    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }

    let source = generate_server(tyml);

    fs::write(path, source)?;

    Ok(())
}

fn generate_server(tyml: &Tyml) -> String {
    let mut source = String::new();

    let traits = tyml
        .interfaces()
        .iter()
        .map(|interface| format!("crate::types::{}", interface.original_name))
        .collect::<Vec<_>>()
        .join(" + ");

    source += "
use std::sync::Arc;
use std::collections::HashMap;
use serde_json;
use axum::{
    Json, Router,
    body::Body,
    extract::Query,
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::get,
};

pub mod types;

";

    source += format!(
        "pub async fn serve<T: {}>(api: T, address: &str) -> Result<(), std::io::Error> {{\n",
        traits
    )
    .as_str();

    source += "    let api = Arc::new(api);\n";
    source += "    let mut router = Router::new();\n\n";

    for interface in tyml.interfaces().iter() {
        for function in interface.functions.iter() {
            source += format!(
                r#"    router = route("/{}/{}", get(async move |Query(query): Query<HashMap<String, String>>, "#,
                interface.name.value, function.name.value
            )
            .as_str();

            if let Some(_) = &function.body_argument_info {
                source += "Json(body): Json<_>";
            }

            source += "| {\n";
            source += "        let api = api.clone();\n";

            for argument in function.arguments.iter() {
                let argument_name = argument.name.value.as_ref();

                source += format!(
                    r#"        let {} = query.get("{}").map(|str| serde_json::from_str(str).ok()).flatten();"#,
                    argument_name, argument_name
                )
                .as_str();
                source += "\n";

                source += format!(
                    "        let Some({}) = {} else {{ ",
                    argument_name, argument_name
                )
                .as_str();

                source += "return Response::builder().status(StatusCode::BAD_REQUEST).body(Body::empty()).unwrap(); };\n";
            }

            source += format!(
                "        let result = <api as crate::types::{}>.{}(",
                interface.original_name, &function.name.value
            )
            .as_str();

            if let Some(_) = &function.body_argument_info {
                source += "body, ";
            }

            for argument in function.arguments.iter() {
                source += argument.name.value.as_ref();
                source += ", ";
            }

            source += ").await;\n";

            source += "        match result {\n";

            match &function.return_info {
                Some(_) => {
                    source += "            Ok(value) => (StatusCode::OK, Json(value)),\n";
                }
                None => {
                    source += "            Ok(_) => Response::builder().status(StatusCode::OK).body(Body::empty()).unwrap(),\n";
                }
            }

            match &function.throws_type {
                Some(_) => {
                    source += "            Err(error) => (StatusCode::BAD_REQUEST, Json(error)),\n";
                }
                None => {
                    source += "            Err(_) => Response::builder().status(StatusCode::BAD_REQUEST).body(Body::empty()).unwrap(),\n";
                }
            }
            source += "        }\n";

            source += "    }));\n";
        }
    }

    source += "\n    let listener = tokio::net::TcpListener::bind(address).await?;\n";
    source += "    axum::serve(listener, router).await?;\n";

    source += "}";

    source
}

#[cfg(test)]
mod test {
    use tyml::Tyml;

    use crate::server::rust_axum::lib_gen::generate_server;

    #[test]
    fn rust_server_gen() {
        let source = r#"
interface API {
    function get_user(@body: int) -> User throws string
}
        "#;

        let tyml = Tyml::parse(source.to_string());

        println!("{}", generate_server(&tyml));
    }
}
