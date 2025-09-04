use std::fs;

use crate::GeneratorSettings;

pub(crate) fn generate_build_gradle_kts(
    setting: &GeneratorSettings,
) -> Result<(), Box<dyn std::error::Error>> {
    let source = r#"
plugins {
    kotlin("jvm") version "2.2.0"
    kotlin("plugin.serialization") version "2.2.0"
}

group = "org.lang.tyml"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.9.0")
    implementation("com.squareup.okhttp3:okhttp:4.12.0")
}
"#;

    let mut path = setting.package_path.clone();
    path.push("build.gradle.kts");

    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }

    fs::write(path, source)?;

    Ok(())
}

pub(crate) fn generate_settings_gradle_kts(
    setting: &GeneratorSettings,
) -> Result<(), Box<dyn std::error::Error>> {
    let source = format!(r#"rootProject.name = "{}""#, &setting.package_name);

    let mut path = setting.package_path.clone();
    path.push("settings.gradle.kts");

    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }

    fs::write(path, source)?;

    Ok(())
}
