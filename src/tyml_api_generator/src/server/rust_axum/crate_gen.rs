use std::fs::{self};

use crate::GeneratorSettings;

pub fn generate_cargo_toml(setting: &GeneratorSettings) -> Result<(), Box<dyn std::error::Error>> {
    let source = format!(
        r#"
[package]
name = "{}"
version = "0.1.0"
edition = "2024"

[dependencies]
axum = "0.8"
serde = {{ version = "1.0", features = ["derive"] }}
serde_json = "1.0"
extension-fn = "1.2"
tokio = "1"
async-trait = "0.1"
"#,
        &setting.package_name
    );

    let mut path = setting.package_path.clone();
    path.push("Cargo.toml");

    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }

    fs::write(path, source)?;

    Ok(())
}
