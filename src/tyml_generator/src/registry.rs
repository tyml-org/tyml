pub(crate) mod ini;
pub(crate) mod toml_;

use std::{
    collections::HashMap,
    fs::File,
    io::Read,
    path::Path,
    sync::{Arc, LazyLock, RwLock},
};

use shellexpand::full;

use crate::style::language::LanguageStyle;

pub static STYLE_REGISTRY: LazyLock<StyleRegistry> = LazyLock::new(|| StyleRegistry::new());

pub struct StyleRegistry {
    styles: RwLock<HashMap<String, Arc<LanguageStyle>>>,
}

impl StyleRegistry {
    fn new() -> Self {
        let mut styles = HashMap::new();

        styles.insert("".to_string(), Arc::new(LanguageStyle::Empty));
        styles.insert("ini".to_string(), Arc::new(ini::ini()));
        styles.insert("toml".to_string(), Arc::new(toml_::toml()));

        Self {
            styles: RwLock::new(styles),
        }
    }

    pub fn resolve(&self, name: &str) -> Option<Arc<LanguageStyle>> {
        {
            if let Some(style) = self.styles.read().unwrap().get(name) {
                return Some(style.clone());
            }
        }

        let mut file = if let Some(file) = File::open(Path::new(
            full(format!("~/.tyml_style/{}", name).as_str())
                .unwrap()
                .as_ref(),
        ))
        .ok()
        {
            file
        } else if let Some(file) = File::open(Path::new(name)).ok() {
            file
        } else {
            return None;
        };

        let mut style_string = String::new();

        file.read_to_string(&mut style_string).ok()?;

        let style = Arc::new(toml::from_str::<LanguageStyle>(style_string.as_str()).ok()?);

        self.styles
            .write()
            .unwrap()
            .insert(name.to_string(), style.clone());

        Some(style)
    }
}
