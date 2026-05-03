use std::fs;
use tyml_core::{Tyml, tyml_type::types::FunctionKind};

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

    let mut traits = tyml
        .interfaces()
        .iter()
        .map(|interface| format!("crate::types::{}", interface.original_name))
        .collect::<Vec<_>>()
        .join(" + ");

    let has_auth = tyml
        .interfaces()
        .iter()
        .map(|interface| interface.functions.iter())
        .flatten()
        .any(|function| function.authed.is_some());

    if has_auth {
        traits += " + crate::types::JwtValidator";
    }

    source += "#![allow(unused)]
use std::sync::Arc;
use std::collections::HashMap;
use serde_json;
use axum::{
    Json, Router,
    body::Body,
    extract::Query,
    http::StatusCode,
    response::{IntoResponse, Response},
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
                r#"    router = router.route("/{}/{}", axum::routing::{}(async move |Query(__query): Query<HashMap<String, String>>, "#,
                interface.name.value, function.name.value, method_name
            )
            .as_str();

            if function.cookie.is_some() {
                source += "\n       __cookies: axum_extra::extract::CookieJar, ";
            }

            if let Some(_) = &function.body_argument_info {
                source += "\n       Json(__body): Json<_>, ";
            }

            if let Some(_) = &function.claim_argument_info {
                source += "\n       __header: axum::http::HeaderMap";
            }

            source += "| {\n";
            source += format!("        let api = api{};\n", api_counter).as_str();

            if let Some(_) = &function.claim_argument_info {
                source += "
        let Some(__claim) = crate::types::__bearer(&__header)
                                .map(|token| <T as crate::types::JwtValidator>::validate(token).ok())
                                .flatten() else {
            return Response::builder().status(StatusCode::UNAUTHORIZED).body(Body::empty()).unwrap();
        };
";
            }

            for argument in function.arguments.iter() {
                let argument_name = argument.name.value.as_ref();

                source += format!(
                    r#"        let {} = __query.get("{}").map(|str| serde_json::from_str(str).ok()).flatten();"#,
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
                "        let __raw = <T as crate::types::{}>::{}(&api, ",
                interface.original_name, &function.name.value
            )
            .as_str();

            if function.cookie.is_some() {
                source += "__cookies, ";
            }

            if let Some(_) = &function.claim_argument_info {
                source += "__claim, ";
            }

            if let Some(_) = &function.body_argument_info {
                source += "__body, ";
            }

            for argument in function.arguments.iter() {
                source += argument.name.value.as_ref();
                source += ", ";
            }

            source += ").await;\n";

            // Unwrap (CookieJar, T) tuple if cookie function. Else, pass through.
            let has_inner_value =
                function.return_info.is_some() || function.throws_type.is_some();
            if function.cookie.is_some() {
                if has_inner_value {
                    source += "        let (__cookies, result) = __raw;\n";
                } else {
                    source += "        let __cookies = __raw;\n";
                }
            } else {
                source += "        let result = __raw;\n";
            }

            // Tuple-with-cookies wrapper helper (rendered as `(jar, response)` if cookie, else just `response`).
            let wrap_open = if function.cookie.is_some() {
                "(__cookies, "
            } else {
                ""
            };
            let wrap_close = if function.cookie.is_some() { ")" } else { "" };

            match &function.throws_type {
                Some(_) => {
                    source += "        match result {\n";

                    match &function.return_info {
                        Some(_) => {
                            source += format!(
                                "            Ok(value) => {}(StatusCode::OK, Json(value)){}.into_response(),\n",
                                wrap_open, wrap_close
                            )
                            .as_str();
                        }
                        None => {
                            source += format!(
                                "            Ok(_) => {}(StatusCode::OK, ()){}.into_response(),\n",
                                wrap_open, wrap_close
                            )
                            .as_str();
                        }
                    }

                    source += format!(
                        "            Err(error) => {}(StatusCode::BAD_REQUEST, Json(error)){}.into_response(),\n",
                        wrap_open, wrap_close
                    )
                    .as_str();
                    source += "        }\n";
                }
                None => match &function.return_info {
                    Some(_) => {
                        source += format!(
                            "        {}(StatusCode::OK, Json(result)){}.into_response()\n",
                            wrap_open, wrap_close
                        )
                        .as_str();
                    }
                    None => {
                        if function.cookie.is_some() {
                            source +=
                                "        (__cookies, (StatusCode::OK, ())).into_response()\n";
                        } else {
                            source += "        Response::builder().status(StatusCode::OK).body(Body::empty()).unwrap()\n";
                        }
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
    use tyml_core::Tyml;

    use crate::server::rust_axum::lib_gen::generate_server;

    #[test]
    fn rust_axum_cookie_gen() {
        let source = r#"
type Token {
    access_token: string
}

interface Auth {
    cookie function refresh() -> Token
    cookie function logout()
    function login() -> Token
}
        "#;

        let tyml = Tyml::parse(source.to_string());

        let generated = generate_server(&tyml);
        println!("{}", generated);

        // CookieJar extracted from request for cookie functions
        assert!(generated.contains("__cookies: axum_extra::extract::CookieJar"));
        // tuple unwrap for cookie + return value
        assert!(generated.contains("let (__cookies, result) = __raw;"));
        // jar-only unwrap for cookie + no return
        assert!(generated.contains("let __cookies = __raw;"));
        // cookies attached to response
        assert!(generated.contains("(__cookies, (StatusCode::OK, Json(result)))"));
    }

    #[test]
    fn rust_axum_gen() {
        let source = r#"
type Claim {
    iss: string
    sub: int
    iat: int
    exp: int
}

interface API {
    authed function get_user(@claim: Claim, @body: int) -> User throws string
}
        "#;

        let tyml = Tyml::parse(source.to_string());

        println!("{}", generate_server(&tyml));
    }
}
