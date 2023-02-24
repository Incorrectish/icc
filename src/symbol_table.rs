use std::collections::HashMap;

use crate::fail;

// #[repr(transparent)]
pub struct SymbolTable {
    symbol_table: HashMap<(String, u64), String>,
    // maps scopes to (top of stack, size of current scope)
    newest_mem_locations_and_allocation_sizes_by_scope: HashMap<u64, (u64, u64)>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbol_table: HashMap::default(),
            newest_mem_locations_and_allocation_sizes_by_scope: HashMap::default(),
        }
    }

    #[allow(unused)]
    pub fn print(&self) {
        println!("Symbol Table: ");
        for (key, value) in &self.symbol_table {
            print!("    ");
            println!("Variable \"{}\" in scope '{}, where the location is {}", key.0, key.1, value);
        }
    }

    pub fn allocate(&mut self, name: String, parent_scopes: &Vec<u64>, size: u64, /* parent_scope: u64 */) -> String {
        for i in (0..parent_scopes.len()).rev() {
            // Look for the first scope that is defined, and give all its children scopes its
            // top_of_stack and 0 for their allocations
            let scope = parent_scopes[i];
            if self.newest_mem_locations_and_allocation_sizes_by_scope.contains_key(&scope) {
                let top_of_stack = self.newest_mem_locations_and_allocation_sizes_by_scope[&scope].0;
                for j in (i+1)..parent_scopes.len() {
                    self.newest_mem_locations_and_allocation_sizes_by_scope.insert(parent_scopes[j], (top_of_stack, 0));
                    dbg!(j);
                }
                break;
            }
            if i == 0 {
                for j in 0..parent_scopes.len() {
                    self.newest_mem_locations_and_allocation_sizes_by_scope.insert(parent_scopes[j], (0, 0));
                }
            }
        }
        let curr_scope = *parent_scopes.last().expect("This vector should never be empty");
        dbg!(parent_scopes);
        dbg!(curr_scope);
        let new_location = self.gen_location(size, curr_scope);
        if self.symbol_table.contains_key(&(name.clone(), curr_scope)) {
            fail!("Variable {name} already defined in scope '{curr_scope}");
        } else {
            self.symbol_table.insert((name, curr_scope), new_location.clone());
        }
        new_location
    }

    #[allow(unused)]
    pub fn remove(&mut self, key: &(String, u64)) {
        self.symbol_table.remove(key);
    }

    pub fn get(&self, name: &String, parent_scopes: &Vec<u64>) -> Option<&String> {
        let mut key = (name.clone(), 0u64);
        for i in parent_scopes.iter().rev() {
            key.1 = *i;
            let value = self.symbol_table.get(&key);
            if value.is_some() {
                return value;
            }
        }
        None
    }

    pub fn gen_location(&mut self, allocation: u64, scope: u64) -> String {
        let mut previous = self.newest_mem_locations_and_allocation_sizes_by_scope[&scope];
        previous.0 += allocation;
        previous.1 += allocation;
        *self.newest_mem_locations_and_allocation_sizes_by_scope.get_mut(&scope).unwrap() = previous;
        format!(
            "-{}(%rbp)",
            self.newest_mem_locations_and_allocation_sizes_by_scope[&scope].0
        )
    }
}
