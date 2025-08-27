use std::path::PathBuf;

pub mod general;
pub mod name;
pub mod server;
pub mod client;

pub struct GeneratorSettings {
    pub package_name: String,
    pub package_path: PathBuf,
}

#[cfg(test)]
mod test {
    use std::env::current_dir;

    use tyml_core::Tyml;

    use crate::{GeneratorSettings, server::rust_axum::generate_rust_axum_server};

    #[test]
    fn rust_axum_gen() -> Result<(), Box<dyn std::error::Error>> {
        let mut path = current_dir()?;
        path.push(".test_gen");

        let setting = GeneratorSettings {
            package_name: "test_gen".to_string(),
            package_path: path,
        };

        let source = r#"
type User {
    id: string | int
    name: string
}

interface API {
    function get_user(id: int) -> User
}
        "#;

        let tyml = Tyml::parse(source.to_string());

        generate_rust_axum_server(&setting, &tyml)?;

        Ok(())
    }
}
