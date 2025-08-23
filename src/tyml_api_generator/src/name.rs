use std::collections::{HashMap, HashSet};

pub struct NameContext {
    name_count_map: HashMap<String, usize>,
    defined: HashSet<String>,
}

impl NameContext {
    pub fn new() -> Self {
        Self {
            name_count_map: HashMap::new(),
            defined: HashSet::new(),
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

    pub fn mark_as_defined(&mut self, name: String) {
        self.defined.insert(name);
    }

    pub fn is_defined(&self, name: &str) -> bool {
        self.defined.contains(name)
    }
}
