use std::{
    fs::{self, File},
    io::Read,
    path::PathBuf,
};

use clap::Parser;
use tyml_api_generator::{general::rust::generate_type_tree_for_rust, name::NameContext};
use tyml_core::{
    Tyml,
    tyml_diagnostic::{DiagnosticBuilder, message::Lang},
    tyml_source::SourceCode,
};

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    tyml: String,
    dir: String,
    name: Option<String>,
}

fn main() -> Result<(), String> {
    let args = Args::parse();

    let tyml_file_name = shellexpand::full(&args.tyml)
        .map_err(|error| format!("Failed to resolve tyml path : {}", error))?
        .to_string();
    let dir = shellexpand::full(&args.dir)
        .map_err(|error| format!("Failed to resolve dir path : {}", error))?
        .to_string();

    let mut tyml_file = File::open(&tyml_file_name)
        .map_err(|error| format!("Failed to open '{}' : {}", &tyml_file_name, error))?;

    let mut path = PathBuf::new();
    path.push(dir);

    let mut tyml_source = String::new();

    tyml_file
        .read_to_string(&mut tyml_source)
        .map_err(|_| format!("Failed to read '{}'.", &tyml_file_name))?;

    let tyml = Tyml::parse(tyml_source);

    if tyml.has_error() {
        let tyml_source_code = SourceCode::new(tyml_file_name.clone(), tyml.source_code().clone());
        let empty_source_code = SourceCode::new("".to_string(), "".to_string());

        for error in tyml.parse_errors().iter() {
            error.build(tyml.named_type_map()).print(
                Lang::system(),
                &tyml_source_code,
                &empty_source_code,
            );
        }

        for error in tyml.type_errors().iter() {
            error.build(tyml.named_type_map()).print(
                Lang::system(),
                &tyml_source_code,
                &empty_source_code,
            );
        }

        return Err("TYML file has error.".to_string());
    }

    let name = args
        .name
        .clone()
        .unwrap_or_else(|| "tyml_serde".to_string());

    let settings = GeneratorSettings {
        package_name: name,
        package_path: path,
    };

    generate_cargo_toml(&settings)
        .map_err(|error| format!("Failed to generate Cargo.toml : {}", error))?;
    generate_lib_rs(&settings, &tyml)
        .map_err(|error| format!("Failed to generate lib.rs : {}", error))?;

    println!("Success!");

    Ok(())
}

struct GeneratorSettings {
    package_name: String,
    package_path: PathBuf,
}

fn generate_cargo_toml(setting: &GeneratorSettings) -> Result<(), Box<dyn std::error::Error>> {
    let source = format!(
        r#"# !tyml @cargo/Cargo
[package]
name = "{}"
version = "0.1.0"
edition = "2024"

[dependencies]
serde = {{ version = "1.0", features = ["derive"] }}
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

fn generate_lib_rs(
    setting: &GeneratorSettings,
    tyml: &Tyml,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut type_def = String::new();
    type_def += "use serde::{Serialize, Deserialize};\n";

    let mut name_context = NameContext::new();

    generate_type_tree_for_rust(
        tyml.type_tree(),
        &mut type_def,
        &mut name_context,
        tyml.named_type_map(),
        Some("Config".to_string()),
    );

    let mut path = setting.package_path.clone();
    path.push("src");
    path.push("lib.rs");

    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }

    fs::write(path, type_def)?;

    Ok(())
}
