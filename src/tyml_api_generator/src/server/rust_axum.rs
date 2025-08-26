use tyml::Tyml;

use crate::{server::rust_axum::{crate_gen::generate_cargo_toml, function_gen::generate_functions_for_rust_axum, lib_gen::generate_lib}, GeneratorSettings};

pub mod crate_gen;
pub mod function_gen;
pub mod lib_gen;

pub fn generate_rust_axum_server(setting: &GeneratorSettings, tyml: &Tyml) -> Result<(), Box<dyn std::error::Error>> {
    generate_cargo_toml(setting)?;
    generate_lib(setting, tyml)?;
    generate_functions_for_rust_axum(setting, tyml)?;

    Ok(())
}
