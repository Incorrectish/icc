use std::collections::HashMap;

#[repr(transparent)]
pub struct SymbolTable(HashMap<String, String>);

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            0: HashMap::default(),
        }
    }

    pub fn add(&mut self, name: String, location: String) {
        self.0.insert(name, location);
    }

    pub fn remove(&mut self, name: &String) {
        self.0.remove(name);
    }

    pub fn get(&mut self, name: &str) -> Option<&String> {
        let optional_location = self.0.get(name);
        if let Some(location) = optional_location {
            Some(location)
        } else {
            None
        }
    }
}
