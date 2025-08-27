use std::fs;

use tyml::{Tyml, tyml_type::types::FunctionKind};

use crate::{GeneratorSettings, general::typescript::generate_type_for_typescript};

pub fn generate_functions_for_typescript(
    setting: &GeneratorSettings,
    tyml: &Tyml,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut path = setting.package_path.clone();
    path.push("types.ts");

    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }

    let source = generate_functions(tyml);

    fs::write(path, source)?;

    Ok(())
}

fn generate_functions(tyml: &Tyml) -> String {
    let mut source = String::new();

    source += "
type Ok<T> = { readonly ok: true; readonly value: T };
type Err<E> = { readonly ok: false; readonly error: E };
export type Result<T, E> = Ok<T> | Err<E>;

const __ok = <T>(value: T): Ok<T> => ({ ok: true, value } as const);
const __err = <E>(error: E): Err<E> => ({ ok: false, error } as const);


";

    let mut type_def = String::new();

    for interface in tyml.interfaces().iter() {
        source += format!("export class {} {{\n", interface.original_name).as_str();
        source += "    private url: string;\n";
        source += "    public constructor(url: string) { this.url = url; }\n\n";

        for function in interface.functions.iter() {
            source += format!("    public async {}(", function.name.value.as_str()).as_str();

            let mut arguments = Vec::new();

            if let Some(_) = &function.claim_argument_info {
                arguments.push("__token: string".to_string());
            }

            if let Some(body) = &function.body_argument_info {
                arguments.push(format!(
                    "__body: {}",
                    generate_type_for_typescript(&body.ty, &mut type_def, tyml.named_type_map())
                ));
            }

            for argument in function.arguments.iter() {
                arguments.push(format!(
                    "{}: {}",
                    &argument.name.value,
                    generate_type_for_typescript(
                        &argument.ty,
                        &mut type_def,
                        tyml.named_type_map()
                    )
                ));
            }

            source += arguments.join(", ").as_str();
            source += ")";

            match (&function.return_info, &function.throws_type) {
                (None, None) => {
                    source += ": Promise<void> {\n";
                }
                (None, Some(throws_type)) => {
                    source += format!(
                        ": Promise<Result<void, {}>> {{\n",
                        generate_type_for_typescript(
                            throws_type,
                            &mut type_def,
                            tyml.named_type_map()
                        )
                    )
                    .as_str();
                }
                (Some(return_info), None) => {
                    source += format!(
                        ": Promise<{}> {{\n",
                        generate_type_for_typescript(
                            &return_info.ty,
                            &mut type_def,
                            tyml.named_type_map()
                        )
                    )
                    .as_str();
                }
                (Some(return_info), Some(throws_type)) => {
                    source += format!(
                        ": Promise<Result<{}, {}>> {{\n",
                        generate_type_for_typescript(
                            &return_info.ty,
                            &mut type_def,
                            tyml.named_type_map()
                        ),
                        generate_type_for_typescript(
                            throws_type,
                            &mut type_def,
                            tyml.named_type_map()
                        )
                    )
                    .as_str();
                }
            }

            source += "        let __url = this.url;\n";
            source += format!(
                "        __url += '/{}/{}';\n",
                &interface.name.value, &function.name.value
            )
            .as_str();

            if !function.arguments.is_empty() {
                source += "        __url += '?';\n";
            }

            let mut queries = Vec::new();
            for argument in function.arguments.iter() {
                let mut query = String::new();
                query += format!("        __url += '{}=';\n", &argument.name.value).as_str();
                query += format!(
                    "        __url += encodeURIComponent(JSON.stringify({}));\n",
                    &argument.name.value
                )
                .as_str();

                queries.push(query);
            }

            source += queries.join("        __url += '&';\n").as_str();

            let method = match function.kind {
                FunctionKind::GET => "GET",
                FunctionKind::PUT => "PUT",
                FunctionKind::POST => "POST",
                FunctionKind::PATCH => "PATCH",
                FunctionKind::DELETE => "DELETE",
            };

            let body = match &function.body_argument_info {
                Some(_) => ", body: JSON.stringify(__body)",
                None => "",
            };

            let mut header = String::new();
            header += ", headers: { Accept: 'application/json'";
            if let Some(_) = &function.claim_argument_info {
                header += ", Authorization: `Bearer ${__token}`";
            }
            header += " }";

            source += format!(
                "        const result = await fetch(__url, {{ method: '{}'{}{} }});\n",
                method, body, header
            )
            .as_str();

            source += "        if (result.ok) {\n";
            match (&function.return_info, &function.throws_type) {
                (None, None) => {
                    source += "            return;\n";
                }
                (None, Some(_)) => {
                    source += "            return __ok(undefined);\n";
                }
                (Some(_), None) => {
                    source += format!("            return result.json();\n").as_str();
                }
                (Some(_), Some(_)) => {
                    source += format!("            return __ok(result.json());\n").as_str();
                }
            }

            source += "        } else {\n";
            match (&function.return_info, &function.throws_type) {
                (None, None) => {
                    source += format!(
                        "            throw new Error(`{} ${{__url}} failed: ${{result.status}} ${{result.statusText}}`);\n",
                        method
                    ).as_str();
                }
                (_, Some(_)) => {
                    source += "            const json = result.json();\n";
                    source += "            if (json !== undefined) {\n";
                    source += "                return __err(json);\n";
                    source += "            } else {\n";
                    source += format!(
                        "                throw new Error(`{} ${{__url}} failed: ${{result.status}} ${{result.statusText}}`);\n",
                        method
                    ).as_str();
                    source += "            }\n";
                }
                (Some(_), None) => {
                    source += format!(
                        "            throw new Error(`{} ${{__url}} failed: ${{result.status}} ${{result.statusText}}`);\n",
                        method
                    ).as_str();
                }
            }

            source += "        }\n";

            source += "    }\n";
        }

        source += "}\n\n";
    }

    source += type_def.as_str();

    source
}

#[cfg(test)]
mod test {
    use tyml::Tyml;

    use crate::client::typescript::generate_functions;

    #[test]
    fn ts_client_gen() {
        let source = r#"
type User {
    id: int
    name: string | Name
}

type Name {
    name: string
    display_name: string
}

interface API {
    function get_user(id: int) -> User throws string
}
        "#;

        let tyml = Tyml::parse(source.to_string());

        println!("{}", generate_functions(&tyml));
    }
}
