use std::collections::HashMap;

pub struct NameContext {
    name_count_map: HashMap<String, usize>,
}

impl NameContext {
    pub fn new() -> Self {
        Self {
            name_count_map: HashMap::new(),
        }
    }

    pub fn create_name(&mut self, name: String) -> String {
        let count = self.name_count_map.entry(name.clone()).or_insert(0);

        let name = match *count {
            0 => name,
            _ => format!("{}{}", name, count),
        };

        *count += 1;

        name
    }
}
