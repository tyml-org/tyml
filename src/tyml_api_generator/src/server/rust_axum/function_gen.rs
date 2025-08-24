use std::{fs::File, io::Write};

use tyml::Tyml;

use crate::{GeneratorSettings, general::rust::generate_type_for_rust, name::NameContext};

pub(crate) fn generate_functions(
    setting: &GeneratorSettings,
    tyml: &Tyml,
) -> Result<(), Box<dyn std::error::Error>> {
    let source = String::new();

    let mut path = setting.package_path.clone();
    path.push("src");
    path.push("types.rs");

    let mut file = File::create(path)?;

    file.write(source.as_bytes())?;

    Ok(())
}

pub(crate) fn generate_trait(tyml: &Tyml) -> String {
    let mut source = String::new();
    let mut type_def = String::new();
    let mut name_context = NameContext::new();

    for interface in tyml.interfaces().iter() {
        source += "#![allow(async_fn_in_trait)]\n";
        source += format!("pub trait {}: Send + Clone {{\n", interface.original_name).as_str();

        for function in interface.functions.iter() {
            source += format!("    async fn {}(", function.name.value.as_str()).as_str();

            if let Some(body) = &function.body_argument_info {
                source += format!(
                    "body: {}, ",
                    generate_type_for_rust(
                        &body.ty,
                        &mut type_def,
                        &mut name_context,
                        tyml.named_type_map()
                    )
                )
                .as_str();
            }

            for argument in function.arguments.iter() {
                source += format!(
                    "{}: {}, ",
                    &argument.name.value,
                    generate_type_for_rust(
                        &argument.ty,
                        &mut type_def,
                        &mut name_context,
                        tyml.named_type_map()
                    )
                )
                .as_str();
            }

            source += ")";

            match &function.return_info {
                Some(return_type) => match &function.throws_type {
                    Some(throws_type) => {
                        source += format!(
                            " -> Result<{}, {}>",
                            generate_type_for_rust(
                                &return_type.ty,
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
                    None => {
                        source += format!(
                            " -> Result<{}, ()>",
                            generate_type_for_rust(
                                &return_type.ty,
                                &mut type_def,
                                &mut name_context,
                                tyml.named_type_map()
                            )
                        )
                        .as_str();
                    }
                },
                None => match &function.throws_type {
                    Some(throws_type) => {
                        source += format!(
                            " -> Result<(), {}>",
                            generate_type_for_rust(
                                throws_type,
                                &mut type_def,
                                &mut name_context,
                                tyml.named_type_map()
                            )
                        )
                        .as_str();
                    }
                    None => {}
                },
            }

            source += ";\n"
        }

        source += "}\n\n";
    }

    format!("{}\n\n{}", type_def, source)
}
