use std::fs;

use tyml_core::Tyml;

use crate::{GeneratorSettings, general::rust::generate_type_for_rust, name::NameContext};

pub(crate) fn generate_functions_for_rust_client(
    setting: &GeneratorSettings,
    tyml: &Tyml,
) -> Result<(), Box<dyn std::error::Error>> {
    let source = generate_struct(tyml);

    let mut path = setting.package_path.clone();
    path.push("src");
    path.push("types.rs");

    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }

    fs::write(path, source)?;

    Ok(())
}

fn generate_struct(tyml: &Tyml) -> String {
    let mut source = String::new();
    let mut type_def = String::new();
    let mut name_context = NameContext::new();

    for interface in tyml.interfaces().iter() {
        source += format!("pub struct {} {{\n", interface.original_name).as_str();
        source += "    pub url: String,\n";
        source += "}\n\n";

        source += format!("impl {} {{\n", interface.original_name).as_str();

        for function in interface.functions.iter() {
            source += format!("    pub async fn {}(", &function.name.value).as_str();

            let mut arguments = Vec::new();

            if let Some(claim) = &function.claim_argument_info {
                arguments.push(format!(
                    "__claim: {}",
                    generate_type_for_rust(
                        &claim.ty,
                        &mut type_def,
                        &mut name_context,
                        tyml.named_type_map()
                    )
                ));
            }

            if let Some(body) = &function.body_argument_info {
                arguments.push(format!(
                    "__body: {}",
                    generate_type_for_rust(
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
                    generate_type_for_rust(
                        &argument.ty,
                        &mut type_def,
                        &mut name_context,
                        tyml.named_type_map()
                    )
                ));
            }

            source += arguments.join(", ").as_str();

            match (&function.return_info, &function.throws_type) {
                (None, None) => {
                    source += " {\n";
                }
                (None, Some(throws_type)) => {
                    source += format!(
                        " -> Result<(), {}> {{\n",
                        generate_type_for_rust(
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
                        " -> {} {{\n",
                        generate_type_for_rust(
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
                        " -> Result<{}, {}> {{\n",
                        generate_type_for_rust(
                            &return_info.ty,
                            &mut type_def,
                            &mut name_context,
                            tyml.named_type_map()
                        ),
                        generate_type_for_rust(
                            throws_type,
                            &mut type_def,
                            &mut name_context,
                            tyml.named_type_map()
                        )
                    )
                    .as_str();
                }
            }
        }
    }

    format!("{}\n\n{}", type_def, source)
}
