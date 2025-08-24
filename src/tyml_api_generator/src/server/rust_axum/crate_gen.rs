use std::{fs::File, io::Write};

use crate::GeneratorSettings;

pub fn generate_cargo_toml(setting: &GeneratorSettings) -> Result<(), Box<dyn std::error::Error>> {
    let source = format!(
        r#"
[package]
name = "{}"
version = "0.1.0"
edition = "2024"

[dependencies]
axum = "0.8.4"
serde = "1.0.219"
serde_json = "1.0.142"
extension-fn = "1.2.0"
tokio = "1.47.1"
"#,
        &setting.package_name
    );

    let mut path = setting.package_path.clone();
    path.push("Cargo.toml");

    let mut file = File::create(path)?;

    file.write(source.as_bytes())?;

    Ok(())
}
