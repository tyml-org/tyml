use std::{fs::File, io::Read, path::PathBuf};

use clap::{Parser, Subcommand, ValueEnum};
use tyml::{
    Tyml,
    tyml_diagnostic::{DiagnosticBuilder, message::Lang},
    tyml_source::SourceCode,
};
use tyml_api_generator::{GeneratorSettings, server::rust_axum::generate_rust_axum_server};

/// Generate api for REST-API server and client with tyml
#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    sub_command: SubCommand,
}

#[derive(Subcommand)]
enum SubCommand {
    Server {
        kind: ServerKind,
        tyml: String,
        dir: String,
        name: Option<String>,
    },
    Client {
        kind: ClientKind,
        tyml: String,
        dir: String,
        name: Option<String>,
    },
}

#[derive(ValueEnum, Clone, Copy, PartialEq, Eq)]
enum ServerKind {
    RustAxum,
}

#[derive(ValueEnum, Clone, Copy, PartialEq, Eq)]
enum ClientKind {}

fn main() -> Result<(), String> {
    let args = Args::parse();

    let (tyml_file_name, dir, name) = match &args.sub_command {
        SubCommand::Server {
            kind: _,
            tyml,
            dir,
            name,
        } => (tyml, dir, name),
        SubCommand::Client {
            kind: _,
            tyml,
            dir,
            name,
        } => (tyml, dir, name),
    };

    let tyml_file_name = shellexpand::full(tyml_file_name)
        .map_err(|error| format!("Failed to resolve TYML file name : {}", error))?
        .to_string();
    let dir = shellexpand::full(dir)
        .map_err(|error| format!("Failed to resolve directory name : {}", error))?
        .to_string();

    let mut path = PathBuf::new();
    path.push(dir);

    let setting = GeneratorSettings {
        package_name: name.clone().unwrap_or("api".to_string()),
        package_path: path,
    };

    let mut file = File::open(tyml_file_name.as_str())
        .map_err(|_| format!("TYML file '{}' is not found.", &tyml_file_name))?;

    let mut tyml_source = String::new();

    file.read_to_string(&mut tyml_source)
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

    match args.sub_command {
        SubCommand::Server {
            kind,
            tyml: _,
            dir: _,
            name: _,
        } => match kind {
            ServerKind::RustAxum => {
                generate_rust_axum_server(&setting, &tyml)
                    .map_err(|error| format!("Failed to generate : {}", error))?;
            }
        },
        SubCommand::Client {
            kind: _,
            tyml: _,
            dir: _,
            name: _,
        } => todo!(),
    }

    println!("Success!");

    Ok(())
}
