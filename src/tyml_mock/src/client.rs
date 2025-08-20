use reqwest::Client;
use tyml::{Tyml, tyml_type::resolver::camel_to_snake};

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

        let mut request = client
            .get(format!(
                "http://localhost:3000/{}/{}",
                interface_name, function_name
            ))
            .query(&arguments)
            .header("Accept", "application/json");

        if let Some(body) = &function.body_argument_info {
            request = request.json(&body.default_value.unwrap().to_serde_json());
        }

        let response = request.send().await.map_err(|error| error.to_string())?;

        if let Ok(text) = response.text().await {
            println!("Response : {}", text);
        } else {
            println!("OK, but not text.");
        }

        Ok(())
    }
}
