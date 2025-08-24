use tyml::Tyml;

pub(crate) fn generate_lib() {}

pub(crate) fn generate_server(tyml: &Tyml) -> String {
    let mut source = String::new();

    let traits = tyml
        .interfaces()
        .iter()
        .map(|interface| interface.original_name.to_string())
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
";

    source += format!(
        "pub async fn serve<T: {}>(api: T, address: &str) -> Result<(), std::io::Error> {{\n",
        traits
    )
    .as_str();

    source += "    let api = Arc::new(api);\n";
    source += "    let mut router = Router::new();\n";

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

            source += format!("        let result = api.{}(", &function.name.value).as_str();

            if let Some(_) = &function.body_argument_info {
                source += "body, ";
            }

            for argument in function.arguments.iter() {
                source += argument.name.value.as_ref();
                source += ", ";
            }

            source += ").await;\n";

            source += "    });\n";
        }
    }

    source += "}";

    source
}
