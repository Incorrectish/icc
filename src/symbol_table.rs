use crate::fail;

use std::collections::HashMap;

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

    pub fn print(&self) {
        println!("Symbol Table: ");
        for (key, value) in &self.symbol_table {
            print!("    ");
            println!("Variable \"{}\" in scope '{}, where the location is {}", key.0, key.1, value);
        }
    }

    pub fn allocate(&mut self, name: String, scope: u64, size: u64, parent_scope: u64) -> String {
        let key = (name, scope);
        if !self.symbol_table.contains_key(&key) {
            if self
                .newest_mem_locations_and_allocation_sizes_by_scope
                .contains_key(&scope)
            {
                let location = self.gen_location(size, scope);
                self.symbol_table.insert(key, location.clone());
                self.print();
                location
            } else {
                // create top of stack for scope
                if self
                    .newest_mem_locations_and_allocation_sizes_by_scope
                    .contains_key(&parent_scope)
                {
                    self.newest_mem_locations_and_allocation_sizes_by_scope
                        .insert(
                            scope,
                            (
                                self.newest_mem_locations_and_allocation_sizes_by_scope
                                    [&parent_scope]
                                    .0,
                                0,
                            ),
                        );
                    let location = self.gen_location(size, scope);
                    self.symbol_table.insert(key, location.clone());
                self.print();
                    location
                } else {
                    // This means the parent scope hasn't even been created, which means the top of
                    // the stack and allocations should just be 0
                    self.newest_mem_locations_and_allocation_sizes_by_scope
                        .insert(parent_scope, (0, 0));
                    self.newest_mem_locations_and_allocation_sizes_by_scope
                        .insert(
                            scope,
                            (
                                self.newest_mem_locations_and_allocation_sizes_by_scope
                                    [&parent_scope]
                                    .0,
                                0,
                            ),
                        );
                    let location = self.gen_location(size, scope);
                    self.symbol_table.insert(key, location.clone());
                self.print();
                    location
                }
            }
        } else {
            self.print();
            fail!("Duplicate variable {name}, declared already");
        }
    }

    pub fn remove(&mut self, key: &(String, u64)) {
        self.symbol_table.remove(key);
    }

    pub fn get(&self, key: &(String, u64)) -> Option<&String> {
        self.symbol_table.get(key)
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
