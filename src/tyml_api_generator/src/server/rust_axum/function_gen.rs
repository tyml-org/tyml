use std::{fs::File, io::Write};

use crate::GeneratorSettings;

pub(crate) fn generate_functions(
    setting: &GeneratorSettings,
) -> Result<(), Box<dyn std::error::Error>> {
    let source = String::new();

    let mut path = setting.package_path.clone();
    path.push("src");
    path.push("types.rs");

    let mut file = File::create(path)?;

    file.write(source.as_bytes())?;

    Ok(())
}
