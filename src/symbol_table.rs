#[macro_use]
use crate::fail;

use std::collections::HashMap;

// #[repr(transparent)]
pub struct SymbolTable(HashMap<String, String>, u64);

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            0: HashMap::default(),
            1: 0,
        }
    }

    pub fn allocate(&mut self, name: String, size: u64) -> String {
        if !self.0.contains_key(&name) {
            let location = self.gen_location(size);
            self.0.insert(name, location.clone());
            location
        } else {
            fail!("Duplicate variable {name}, declared already");
        }
    }

    pub fn remove(&mut self, name: &String) {
        self.0.remove(name);
    }

    pub fn get(&self, name: &str) -> Option<&String> {
        let optional_location = self.0.get(name);
        if let Some(location) = optional_location {
            Some(location)
        } else {
            None
        }
    }

    pub fn gen_location(&mut self, allocation: u64) -> String {
        self.1 += allocation;
        format!("-{}(%rbp)", self.1)
    }
}
