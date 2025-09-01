use reqwest::Client;
use tyml_core::{
    Tyml,
    tyml_type::{resolver::camel_to_snake, types::FunctionKind},
};

use crate::json::ToSerdeJson;

pub struct TymlMockClient {
    tyml: Tyml,
}

impl TymlMockClient {
    pub fn new(tyml: Tyml) -> Self {
        Self { tyml }
    }

    pub async fn send(self, interface_name: &str, function_name: &str) -> Result<(), String> {
        let interface_name = camel_to_snake(interface_name);
        let function_name = camel_to_snake(function_name);

        let interface = self
            .tyml
            .interfaces()
            .iter()
            .find(|interface| interface.name.value.as_str() == interface_name.as_str())
            .unwrap();

        let function = interface
            .functions
            .iter()
            .find(|function| function.name.value.as_str() == function_name.as_str())
            .unwrap();

        if function
            .arguments
            .iter()
            .any(|argument| argument.default_value.is_none())
        {
            return Err("no argument's default value".to_string());
        }

        let arguments = function
            .arguments
            .iter()
            .map(|argument| {
                (
                    argument.name.value.to_string(),
                    argument.default_value.unwrap().to_serde_json().to_string(),
                )
            })
            .collect::<Vec<_>>();

        let client = Client::new();

        let url = format!("http://localhost:3000/{}/{}", interface_name, function_name);

        let mut request = match function.kind {
            FunctionKind::GET => client.get(url),
            FunctionKind::PUT => client.put(url),
            FunctionKind::POST => client.post(url),
            FunctionKind::PATCH => client.patch(url),
            FunctionKind::DELETE => client.delete(url),
        };
        request = request.query(&arguments);
        request = request.header("Accept", "application/json");

        if let Some(body) = &function.body_argument_info {
            request = request.json(&body.default_value.unwrap().to_serde_json());
        }

        let response = request.send().await.map_err(|error| error.to_string())?;

        match response.error_for_status_ref() {
            Ok(_) => {
                if let Ok(text) = response.text().await {
                    println!("Response : {}", text);
                } else {
                    println!("OK, but not text.");
                }
            }
            Err(error) => {
                return Err(error.to_string());
            }
        }

        Ok(())
    }
}
