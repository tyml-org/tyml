use std::{ops::Deref, sync::LazyLock};

use ariadne::{Color, Fmt};
use regex::Regex;
use resource::resource_str;
use toml::{Table, Value};

static JA_JP: LazyLock<Table> =
    LazyLock::new(|| resource_str!("message/ja_JP.toml").parse().unwrap());
static EN_US: LazyLock<Table> =
    LazyLock::new(|| resource_str!("message/en_US.toml").parse().unwrap());

pub(crate) fn get_text(key: &str, lang: &str) -> String {
    return get_text_optional(key, lang).unwrap();
}

pub(crate) fn get_text_optional(key: &str, lang: &str) -> Option<String> {
    let table = match lang {
        "ja_JP" => JA_JP.deref(),
        "en_US" => EN_US.deref(),
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
    LazyLock::new(|| Regex::new(r"%color:\w+{(\\}|[^}])*}").unwrap());
static COLOR_GROUP_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"%color:(\w+){((\\}|[^}])*)}").unwrap());

pub(crate) fn replace_message(
    mut message: String,
    arguments: &Vec<String>,
    colored: bool,
) -> String {
    for (index, replace) in arguments.iter().enumerate() {
        message = message.replace(format!("%{}", index).as_str(), replace.as_str());
    }

    for matched in COLOR_REGEX.find_iter(message.clone().as_str()) {
        let tags = COLOR_GROUP_REGEX.captures(matched.as_str()).unwrap();
        let color = &tags[0];
        let text = &tags[1];

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
                    text.to_string().fg(color).to_string().as_str(),
                );
            }
            false => {
                message = message.replace(matched.as_str(), text);
            }
        }
    }

    message
}
