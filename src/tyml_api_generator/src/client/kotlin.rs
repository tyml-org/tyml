use tyml_core::Tyml;

use crate::{
    GeneratorSettings,
    client::kotlin::{
        function_gen::generate_functions_for_kotlin_client,
        gradle_gen::{generate_build_gradle_kts, generate_settings_gradle_kts},
    },
};

pub mod function_gen;
pub mod gradle_gen;

pub fn generate_kotlin_client(
    setting: &GeneratorSettings,
    tyml: &Tyml,
) -> Result<(), Box<dyn std::error::Error>> {
    generate_functions_for_kotlin_client(setting, tyml)?;
    generate_build_gradle_kts(setting)?;
    generate_settings_gradle_kts(setting)?;

    Ok(())
}
