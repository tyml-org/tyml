use tyml_core::{Tyml, tyml_type::types::FunctionKind};

use crate::{general::typescript::generate_type_for_typescript, name::NameContext};

pub(crate) fn generate_functions(tyml: &Tyml) -> String {
    let mut source = String::new();

    // Result helpers + the minimal CookieJar interface contract.
    source += "
type Ok<T> = { readonly ok: true; readonly value: T };
type Err<E> = { readonly ok: false; readonly error: E };
export type Result<T, E> = Ok<T> | Err<E>;

const __ok = <T>(value: T): Ok<T> => ({ ok: true, value } as const);
const __err = <E>(error: E): Err<E> => ({ ok: false, error } as const);

export type FetchLike = (input: string, init?: any) => Promise<Response>;

/**
 * Minimal cookie-jar contract for non-browser environments.
 * Supply an implementation that wraps tough-cookie / your runtime's store.
 *
 *   getCookieHeader(url): combined `Cookie:` header value (or null) for that URL
 *   setCookie(setCookieHeader, url): persist a single Set-Cookie header line for url
 */
export interface CookieJar {
    getCookieHeader(url: string): Promise<string | null>;
    setCookie(setCookieHeader: string, url: string): Promise<void>;
}

export interface ClientOptions {
    url: string;
    fetch?: FetchLike;
    cookieJar?: CookieJar;
}

const __defaultFetch: FetchLike = (input, init) =>
    (globalThis as any).fetch(input, init);


";

    let mut type_def = String::new();
    let mut name_context = NameContext::new();

    for interface in tyml.interfaces().iter() {
        let interface_uses_cookie = interface
            .functions
            .iter()
            .any(|function| function.cookie.is_some());

        source += "/**\n";
        source += interface
            .documents
            .iter()
            .map(|line| format!(" *{}", line))
            .collect::<Vec<_>>()
            .join("")
            .as_str();
        source += " */\n";

        source += format!("export class {} {{\n", interface.original_name).as_str();
        source += "    private url: string;\n";
        source += "    private fetch: FetchLike;\n";
        if interface_uses_cookie {
            source += "    private cookieJar: CookieJar | undefined;\n";
        }
        source += "    public constructor(options: ClientOptions) {\n";
        source += "        this.url = options.url;\n";
        source += "        this.fetch = options.fetch ?? __defaultFetch;\n";
        if interface_uses_cookie {
            source += "        this.cookieJar = options.cookieJar;\n";
        }
        source += "    }\n\n";

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

            source += format!("    public async {}(", function.name.value.as_str()).as_str();

            let mut arguments = Vec::new();

            if let Some(_) = &function.claim_argument_info {
                arguments.push("__token: string".to_string());
            }

            if let Some(body) = &function.body_argument_info {
                arguments.push(format!(
                    "__body: {}",
                    generate_type_for_typescript(
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
                    generate_type_for_typescript(
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
                    source += ": Promise<void> {\n";
                }
                (None, Some(throws_type)) => {
                    source += format!(
                        ": Promise<Result<void, {}>> {{\n",
                        generate_type_for_typescript(
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
                        ": Promise<{}> {{\n",
                        generate_type_for_typescript(
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
                        ": Promise<Result<{}, {}>> {{\n",
                        generate_type_for_typescript(
                            &return_info.ty,
                            &mut type_def,
                            &mut name_context,
                            tyml.named_type_map()
                        ),
                        generate_type_for_typescript(
                            throws_type,
                            &mut type_def,
                            &mut name_context,
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

            // Headers: build as a mutable object so we can attach a Cookie header from the jar.
            source += "        const __headers: Record<string, string> = { Accept: 'application/json' };\n";
            if let Some(_) = &function.claim_argument_info {
                source += "        __headers['Authorization'] = `Bearer ${__token}`;\n";
            }

            if function.cookie.is_some() {
                source += "        if (this.cookieJar) {\n";
                source += "            const __cookieHeader = await this.cookieJar.getCookieHeader(__url);\n";
                source += "            if (__cookieHeader) __headers['Cookie'] = __cookieHeader;\n";
                source += "        }\n";
            }

            let body = match &function.body_argument_info {
                Some(_) => ", body: JSON.stringify(__body)",
                None => "",
            };

            source += format!(
                "        const result = await this.fetch(__url, {{ method: '{}'{}, headers: __headers }});\n",
                method, body
            )
            .as_str();

            // After the response, persist any Set-Cookie headers into the jar.
            if function.cookie.is_some() {
                source += "        if (this.cookieJar) {\n";
                source += "            const __setCookies: string[] = (typeof (result.headers as any).getSetCookie === 'function')\n";
                source += "                ? (result.headers as any).getSetCookie()\n";
                source += "                : (result.headers.get('set-cookie') ? [result.headers.get('set-cookie') as string] : []);\n";
                source += "            for (const __sc of __setCookies) {\n";
                source += "                await this.cookieJar.setCookie(__sc, __url);\n";
                source += "            }\n";
                source += "        }\n";
            }

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
    use tyml_core::Tyml;

    use crate::client::typescript::isomorphic::generate_functions;

    #[test]
    fn ts_isomorphic_client_gen() {
        let source = r#"
type Token {
    access_token: string
}

interface Auth {
    cookie function refresh() -> Token
    function login() -> Token
}
        "#;

        let tyml = Tyml::parse(source.to_string());

        let generated = generate_functions(&tyml);
        println!("{}", generated);

        assert!(generated.contains("CookieJar"));
        assert!(generated.contains("getCookieHeader"));
        assert!(generated.contains("setCookie"));
        assert!(generated.contains("ClientOptions"));
    }
}
