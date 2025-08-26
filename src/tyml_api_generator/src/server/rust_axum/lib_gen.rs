use std::fs;
use tyml::{Tyml, tyml_type::types::FunctionKind};

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

#[allow(unused)]
";

    source += format!(
        "pub async fn serve<T: {}>(api: T, address: &str) -> Result<(), std::io::Error> {{\n",
        traits
    )
    .as_str();

    source += "    let api = Arc::new(api);\n";
    source += "    let mut router = Router::new();\n\n";

    let mut api_counter = 0;

    for interface in tyml.interfaces().iter() {
        for function in interface.functions.iter() {
            let method_name = match function.kind {
                FunctionKind::GET => "get",
                FunctionKind::PUT => "put",
                FunctionKind::POST => "post",
                FunctionKind::PATCH => "patch",
                FunctionKind::DELETE => "delete",
            };

            api_counter += 1;
            source += format!("    let api{} = api.clone();\n", api_counter).as_str();

            source += format!(
                r#"    router = router.route("/{}/{}", {}(async move |Query(query): Query<HashMap<String, String>>, "#,
                interface.name.value, function.name.value, method_name
            )
            .as_str();

            if let Some(_) = &function.body_argument_info {
                source += "Json(body): Json<_>";
            }

            source += "| {\n";
            source += format!("        let api = api{};\n", api_counter).as_str();

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
                "        let result = <T as crate::types::{}>::{}(&api, ",
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

            match &function.throws_type {
                Some(_) => {
                    source += "        match result {\n";

                    match &function.return_info {
                        Some(_) => {
                            source += "            Ok(value) => (StatusCode::OK, Json(value)).into_response(),\n";
                        }
                        None => {
                            source += "            Ok(_) => Response::builder().status(StatusCode::OK).body(Body::empty()).unwrap(),\n";
                        }
                    }

                    source += "            Err(error) => (StatusCode::BAD_REQUEST, Json(error)).into_response(),\n";
                    source += "        }\n";
                }
                None => match &function.return_info {
                    Some(_) => {
                        source += "        (StatusCode::OK, Json(result)).into_response()\n";
                    }
                    None => {
                        source += "        Response::builder().status(StatusCode::OK).body(Body::empty()).unwrap()\n";
                    }
                },
            }

            source += "    }));\n";
        }
    }

    source += "\n    let listener = tokio::net::TcpListener::bind(address).await?;\n";
    source += "    axum::serve(listener, router).await?;\n";

    source += "    Ok(())\n";

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
