use std::fs;

use tyml_core::{Tyml, tyml_type::types::FunctionKind};

use crate::{
    GeneratorSettings,
    general::kotlin::{generate_type_for_kotlin, snake_to_camel},
    name::NameContext,
};

pub(crate) fn generate_functions_for_kotlin_client(
    setting: &GeneratorSettings,
    tyml: &Tyml,
) -> Result<(), Box<dyn std::error::Error>> {
    let source = generate_class(tyml);

    let mut path = setting.package_path.clone();
    path.push("src");
    path.push("main");
    path.push("kotlin");
    path.push("Client.kt");

    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }

    fs::write(path, source)?;

    Ok(())
}

fn generate_class(tyml: &Tyml) -> String {
    let mut source = String::new();
    let mut type_def = String::new();
    let mut name_context = NameContext::new();

    type_def += "
package org.lang.tyml

import kotlinx.serialization.*
import kotlinx.serialization.descriptors.*
import kotlinx.serialization.encoding.*
import kotlinx.serialization.json.*
import okhttp3.HttpUrl.Companion.toHttpUrl
import okhttp3.MediaType.Companion.toMediaType
import okhttp3.OkHttpClient
import okhttp3.Request
import okhttp3.RequestBody.Companion.toRequestBody

sealed class Result<out V, out E>
class Ok<out V>(val value: V) : Result<V, Nothing>()
class Err<out E>(val error: E) : Result<Nothing, E>()
";

    for interface in tyml.interfaces().iter() {
        source += "/**\n";
        source += interface
            .documents
            .iter()
            .map(|line| format!(" *{}", line))
            .collect::<Vec<_>>()
            .join("")
            .as_str();
        source += " */\n";

        source += format!(
            "class {}(private val url: String) {{\n",
            interface.original_name
        )
        .as_str();

        for function in interface.functions.iter() {
            source += "    /**\n";
            source += function
                .documents
                .iter()
                .map(|line| format!("     *{}", line))
                .collect::<Vec<_>>()
                .join("")
                .as_str();
            source += "     */\n";

            source += format!("    fun {}(", snake_to_camel(&function.name.value)).as_str();

            let mut arguments = Vec::new();

            if let Some(_) = &function.claim_argument_info {
                arguments.push("__token: String".to_string());
            }

            if let Some(body) = &function.body_argument_info {
                arguments.push(format!(
                    "__body: {}",
                    generate_type_for_kotlin(
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
                    generate_type_for_kotlin(
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
                    source += " {\n";
                }
                (None, Some(throws_type)) => {
                    source += format!(
                        ": Result<Unit, {}> {{\n",
                        generate_type_for_kotlin(
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
                        ": {} {{\n",
                        generate_type_for_kotlin(
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
                        ": Result<{}, {}> {{\n",
                        generate_type_for_kotlin(
                            &return_info.ty,
                            &mut type_def,
                            &mut name_context,
                            tyml.named_type_map()
                        ),
                        generate_type_for_kotlin(
                            throws_type,
                            &mut type_def,
                            &mut name_context,
                            tyml.named_type_map()
                        )
                    )
                    .as_str();
                }
            }

            source += "        val client = OkHttpClient()\n";

            source += format!(
                r#"        val url = "${{this.url}}/{}/{}".toHttpUrl().newBuilder()"#,
                &interface.name.value, &function.name.value
            )
            .as_str();
            source += "\n";

            for argument in function.arguments.iter() {
                source += format!(
                    r#"            .addQueryParameter("{}", Json.encodeToString({}))"#,
                    &argument.name.value, &argument.name.value
                )
                .as_str();
                source += "\n";
            }

            source += "            .build()\n";

            match &function.body_argument_info {
                Some(_) => {
                    source += "        val reqBody = Json.encodeToString(__body)\n";
                }
                None => {
                    source += r#"        val reqBody = """#;
                    source += "\n";
                }
            }
            source += r#"            .toRequestBody("application/json".toMediaType())"#;
            source += "\n";

            source += "        val request = Request.Builder()\n";
            source += "            .url(url)\n";

            if let Some(_) = &function.claim_argument_info {
                source += r#"            .addHeader("Authorization", "Bearer $__token")"#;
                source += "\n";
            }

            source += match function.kind {
                FunctionKind::GET => "            .get()\n",
                FunctionKind::PUT => "            .put(reqBody)\n",
                FunctionKind::POST => "            .post(reqBody)\n",
                FunctionKind::PATCH => "            .patch(reqBody)\n",
                FunctionKind::DELETE => "            .delete(reqBody)\n",
            };

            source += "            .build()\n";

            source += "        client.newCall(request).execute().use { response ->\n";

            match (&function.return_info, &function.throws_type) {
                (None, None) => {
                    source += "            if (!response.isSuccessful) {\n";
                    source += r#"                error("HTTP ${response.code}: ${response.body?.string()}")"#;
                    source += "\n";
                    source += "            }\n";
                }
                (None, Some(_)) => {
                    source +=
                        "            if (!response.isSuccessful && response.body != null) {\n";
                    source += "                return Err(Json.decodeFromString(response.body!!.string()))\n";
                    source += "            }\n";
                    source += "            if (!response.isSuccessful) {\n";
                    source += r#"                error("HTTP ${response.code}: ${response.body?.string()}")"#;
                    source += "\n";
                    source += "            }\n";
                }
                (Some(_), None) => {
                    source += "            if (!response.isSuccessful) {\n";
                    source += r#"                error("HTTP ${response.code}: ${response.body?.string()}")"#;
                    source += "\n";
                    source += "            }\n";
                    source +=
                        "            return Json.decodeFromString(response.body!!.string())\n";
                }
                (Some(_), Some(_)) => {
                    source +=
                        "            if (!response.isSuccessful && response.body != null) {\n";
                    source += "                return Err(Json.decodeFromString(response.body!!.string()))\n";
                    source += "            }\n";
                    source += "            if (!response.isSuccessful) {\n";
                    source += r#"                error("HTTP ${response.code}: ${response.body?.string()}")"#;
                    source += "\n";
                    source += "            }\n";
                    source +=
                        "            return Ok(Json.decodeFromString(response.body!!.string()))\n";
                }
            }

            source += "        }\n";

            source += "    }\n";
        }

        source += "}\n";
    }

    format!("{}\n\n{}", type_def, source)
}

#[cfg(test)]
mod test {
    use tyml_core::Tyml;

    use crate::client::kotlin::function_gen::generate_class;

    #[test]
    fn kotlin_class_gen() {
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

        println!("{}", generate_class(&tyml));
    }
}
