use std::str::FromStr;

use extension_fn::extension_fn;
use serde_json::{Number, Value};
use tyml_core::tyml_parser::ast::{BinaryLiteral, FloatLiteral, JsonValue, ValueLiteral};

#[extension_fn(<'input, 'allocator> JsonValue<'input, 'allocator>)]
pub(crate) fn to_serde_json(&self) -> Value {
    match self {
        JsonValue::Value(value) => match value {
            ValueLiteral::String(string) => Value::String(string.value.to_string()),
            ValueLiteral::Float(float) => {
                let literal = match float {
                    FloatLiteral::Float(literal) => literal.value,
                    FloatLiteral::Inf(literal) => literal.value,
                    FloatLiteral::Nan(literal) => literal.value,
                };
                match Number::from_str(literal) {
                    Ok(number) => Value::Number(number),
                    Err(_) => Value::Null,
                }
            }
            ValueLiteral::Binary(literal) => {
                let u128 = match literal {
                    BinaryLiteral::Hex(literal) => {
                        u128::from_str_radix(literal.value.replace("0x", "").as_str(), 16)
                    }
                    BinaryLiteral::Oct(literal) => {
                        u128::from_str_radix(literal.value.replace("0o", "").as_str(), 8)
                    }
                    BinaryLiteral::Bin(literal) => {
                        u128::from_str_radix(literal.value.replace("0b", "").as_str(), 2)
                    }
                };
                match u128 {
                    Ok(u128) => Number::from_u128(u128)
                        .map(|number| Value::Number(number))
                        .unwrap_or(Value::Null),
                    Err(_) => Value::Null,
                }
            }
            ValueLiteral::Bool(bool) => Value::Bool(bool.value.to_lowercase() == "true"),
            ValueLiteral::Null(_) => Value::Null,
        },
        JsonValue::Array(array) => Value::Array(
            array
                .elements
                .iter()
                .map(|element| element.to_serde_json())
                .collect(),
        ),
        JsonValue::Object(object) => Value::Object(
            object
                .elements
                .iter()
                .map(|element| {
                    (
                        element.name.value.to_string(),
                        element.value.to_serde_json(),
                    )
                })
                .collect(),
        ),
    }
}
