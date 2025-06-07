use std::{ops::Deref, sync::LazyLock};

use ariadne::{Color, Fmt};
use regex::Regex;
use resource::resource_str;
use sys_locale::get_locale;
use toml::{Table, Value};

static JA_JP: LazyLock<Table> =
    LazyLock::new(|| resource_str!("message/ja_JP.toml").parse().unwrap());
static EN_US: LazyLock<Table> =
    LazyLock::new(|| resource_str!("message/en_US.toml").parse().unwrap());

pub struct Lang {}

#[allow(non_upper_case_globals)]
impl Lang {
    pub const ja_JP: &str = "ja_JP";
    pub const en_US: &str = "en_US";

    pub fn system() -> &'static str {
        let locale = get_locale()
            .unwrap_or(Lang::en_US.to_string())
            .replace("-", "_");

        if locale.contains(Lang::ja_JP) {
            Lang::ja_JP
        } else {
            Lang::en_US
        }
    }
}

pub fn get_text(key: &str, lang: &str) -> String {
    return get_text_optional(key, lang)
        .unwrap_or_else(|| panic!("No message found! lang: {}, key: {}", lang, key));
}

pub fn get_text_optional(key: &str, lang: &str) -> Option<String> {
    let table = match lang {
        Lang::ja_JP => JA_JP.deref(),
        Lang::en_US => EN_US.deref(),
        _ => panic!("Unsupported language : {}", lang),
    };

    let mut current_table = table;
    for key in key.split(".") {
        match current_table.get(key) {
            Some(value) => match value {
                Value::Table(table) => current_table = table,
                Value::String(string) => return Some(string.clone()),
                _ => break,
            },
            None => break,
        }
    }

    None
}

static COLOR_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"%color:\w+\{(\\\}|[^\}])*\}").unwrap());
static COLOR_GROUP_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"%color:(\w+)\{((\\\}|[^\}])*)\}").unwrap());
static ESCAPE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\\(.)").unwrap());

pub fn replace_message(mut message: String, arguments: &Vec<String>, colored: bool) -> String {
    for (index, replace) in arguments.iter().enumerate() {
        message = message.replace(format!("%{}", index).as_str(), replace.as_str());
    }

    if colored {
        let reset_color = "".to_string().fg(Color::White);
        message = format!("{}{}", reset_color, message);
    }

    for matched in COLOR_REGEX.find_iter(message.clone().as_str()) {
        let tags = COLOR_GROUP_REGEX.captures(matched.as_str()).unwrap();
        let color = &tags[1];
        let text = &tags[2];

        match colored {
            true => {
                let color = match color {
                    "cyan" => Color::Cyan,
                    "yellow" => Color::Yellow,
                    "red" => Color::Red,
                    _ => panic!("Unsupported color : {}", color),
                };

                message = message.replace(
                    matched.as_str(),
                    ESCAPE
                        .replace_all(text, "$1")
                        .fg(color)
                        .to_string()
                        .as_str(),
                );
            }
            false => {
                message = message.replace(matched.as_str(), text);
            }
        }
    }

    message
}
