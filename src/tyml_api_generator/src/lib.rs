use std::path::PathBuf;

pub mod name;
pub mod server;

pub struct GeneratorSettings {
    pub package_name: String,
    pub package_path: PathBuf,
}
