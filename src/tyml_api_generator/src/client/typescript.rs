use std::fs;

use tyml_core::Tyml;

use crate::GeneratorSettings;

pub mod browser;
pub mod isomorphic;

/// Isomorphic TypeScript client generator (works in Node, Deno, Bun, Workers, browsers).
/// Cookie handling is delegated to a user-provided `CookieJar` implementation.
pub fn generate_functions_for_typescript(
    setting: &GeneratorSettings,
    tyml: &Tyml,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut path = setting.package_path.clone();
    path.push("types.ts");

    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }

    let source = isomorphic::generate_functions(tyml);

    fs::write(path, source)?;

    Ok(())
}

/// Browser-only TypeScript client generator.
/// Cookies are handled transparently by the browser via `credentials: "include"`.
pub fn generate_functions_for_typescript_browser(
    setting: &GeneratorSettings,
    tyml: &Tyml,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut path = setting.package_path.clone();
    path.push("types.ts");

    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }

    let source = browser::generate_functions(tyml);

    fs::write(path, source)?;

    Ok(())
}
