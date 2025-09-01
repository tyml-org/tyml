use std::fs;

use crate::GeneratorSettings;

pub(crate) fn generate_lib(setting: &GeneratorSettings) -> Result<(), Box<dyn std::error::Error>> {
    let mut path = setting.package_path.clone();
    path.push("src");
    path.push("lib.rs");

    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }

    let source = "#![allow(unused)]\npub mod types;";

    fs::write(path, source)?;

    Ok(())
}
