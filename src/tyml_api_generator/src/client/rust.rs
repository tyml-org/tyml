use tyml_core::Tyml;

use crate::{
    GeneratorSettings,
    client::rust::{
        crate_gen::generate_cargo_toml, function_gen::generate_functions_for_rust_client,
        lib_gen::generate_lib,
    },
};

pub mod crate_gen;
pub mod function_gen;
pub mod lib_gen;

pub fn generate_rust_client(
    setting: &GeneratorSettings,
    tyml: &Tyml,
) -> Result<(), Box<dyn std::error::Error>> {
    generate_cargo_toml(setting)?;
    generate_functions_for_rust_client(setting, tyml)?;
    generate_lib(setting)?;

    Ok(())
}
