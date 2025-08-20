use std::collections::HashMap;

use axum::{
    Json, Router,
    body::Body,
    extract::Query,
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::get,
};
use serde_json::Value;
use tyml::{
    Tyml,
    tyml_type::resolver::{camel_to_snake, check_serde_json_type},
};

use crate::json::ToSerdeJson;

pub struct TymlMockServer {
    tyml: Tyml,
}

impl TymlMockServer {
    pub fn new(tyml: Tyml) -> Self {
        Self { tyml }
    }

    pub async fn serve(&self, location: ServerSourceLocation) -> Result<(), ()> {
        let mut router = Router::new();

        match location {
            ServerSourceLocation::AllInterfaces => {
                router = self.setup(router)?;
            }
            ServerSourceLocation::Interface(interface_name) => {
                router = self.setup_interface(interface_name.as_str(), router)?;
            }
        }

        let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap();
        axum::serve(listener, router).await.unwrap();

        Ok(())
    }

    fn setup(&self, mut router: Router) -> Result<Router, ()> {
        for interface in self.tyml.interfaces().iter() {
            router = self.setup_interface(interface.name.value.as_str(), router)?;
        }

        Ok(router)
    }

    fn setup_interface(&self, interface_name: &str, mut router: Router) -> Result<Router, ()> {
        if self.tyml.has_error() {
            return Err(());
        }

        let interface_name = camel_to_snake(interface_name);

        let interface = self
            .tyml
            .interfaces()
            .iter()
            .find(|interface| interface.name.value.as_str() == interface_name)
            .ok_or(())?;

        for function in interface.functions.iter() {
            function
                .return_info
                .as_ref()
                .ok_or(())?
                .default_value
                .ok_or(())?;

            let tyml = self.tyml.clone();
            let interface_name = interface_name.clone();
            let function_name = function.name.value.clone();

            router = router.route(
                format!("/{}/{}", &interface_name, &function_name).as_str(),
                get(
                    async |Query(query): Query<HashMap<String, String>>,
                           Json(body): Json<Value>| {
                        let tyml = tyml;
                        let interface_name = interface_name;
                        let function_name = function_name;

                        let interface = tyml
                            .interfaces()
                            .iter()
                            .find(|interface| {
                                interface.name.value.as_str() == interface_name.as_str()
                            })
                            .unwrap();

                        let function = interface
                            .functions
                            .iter()
                            .find(|function| function.name.value.as_str() == function_name.as_str())
                            .unwrap();

                        if let Some(body_argument) = &function.body_argument_info {
                            if !check_serde_json_type(
                                &body,
                                &body_argument.ty,
                                tyml.named_type_map(),
                            ) {
                                return Response::builder()
                                    .status(StatusCode::BAD_REQUEST)
                                    .body(Body::empty())
                                    .unwrap();
                            }
                        }

                        for argument in function.arguments.iter() {
                            let Some(query_argument) = query.get(argument.name.value.as_ref())
                            else {
                                return Response::builder()
                                    .status(StatusCode::BAD_REQUEST)
                                    .body(Body::empty())
                                    .unwrap();
                            };

                            let Ok(json) = serde_json::from_str(query_argument.as_str()) else {
                                return Response::builder()
                                    .status(StatusCode::BAD_REQUEST)
                                    .body(Body::empty())
                                    .unwrap();
                            };

                            if !check_serde_json_type(&json, &argument.ty, tyml.named_type_map()) {
                                return Response::builder()
                                    .status(StatusCode::BAD_REQUEST)
                                    .body(Body::empty())
                                    .unwrap();
                            }
                        }

                        let has_extra_argument = query.keys().any(|key| {
                            !function
                                .arguments
                                .iter()
                                .map(|argument| argument.name.value.as_ref())
                                .any(|name| name == key.as_str())
                        });

                        if has_extra_argument {
                            return Response::builder()
                                .status(StatusCode::BAD_REQUEST)
                                .body(Body::empty())
                                .unwrap();
                        }

                        (
                            StatusCode::OK,
                            Json(
                                function
                                    .return_info
                                    .as_ref()
                                    .unwrap()
                                    .default_value
                                    .unwrap()
                                    .to_serde_json(),
                            ),
                        )
                            .into_response()
                    },
                ),
            );
        }

        Ok(router)
    }
}

#[derive(Debug, Clone)]
pub enum ServerSourceLocation {
    AllInterfaces,
    Interface(String),
}
