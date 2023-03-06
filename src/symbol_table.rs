use std::collections::HashMap;

use crate::fail;

// #[repr(transparent)]
pub struct SymbolTable {
    // maps variable + scope to location
    symbol_table: HashMap<(String, u64), String>,
    // maps scopes to (top of stack, size of current scope)
    newest_mem_locations_and_allocation_sizes_by_scope: HashMap<u64, (u64, u64)>,
    // hopefully maps every scope to its parent, only scope 0 has no parent
    parents: HashMap<u64, Option<u64>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbol_table: HashMap::default(),
            newest_mem_locations_and_allocation_sizes_by_scope: {
                let mut map = HashMap::new();
                map.insert(0, (0, 0));
                map
            },
            parents: {
                let mut map = HashMap::new();
                map.insert(0, None);
                map
            },
        }
    }

    #[allow(unused)]
    pub fn print(&self) {
        println!("Symbol Table: ");
        for (key, value) in &self.symbol_table {
            print!("    ");
            println!(
                "Variable \"{}\" in scope '{}, where the location is {}",
                key.0, key.1, value
            );
        }
    }

    pub fn create_scope(&mut self, scope: u64, parent: u64) {
        println!("Creating scope '{scope} with parent '{parent}");
        self.parents.insert(scope, Some(parent));
        self.newest_mem_locations_and_allocation_sizes_by_scope
            .insert(
                scope,
                *self.newest_mem_locations_and_allocation_sizes_by_scope.get(&parent).expect(&format!("scope '{scope} not created")),
            );
    }

    pub fn allocate(
        &mut self,
        name: String,
        curr_scope: u64,
        size: u64, /* parent_scope: u64 */
    ) -> String {
        let parent = self.parents[&curr_scope];
        // for i in (0..parent_scopes.len()).rev() {
        //     // Look for the first scope that is defined, and give all its children scopes its
        //     // top_of_stack and 0 for their allocations
        //     let scope = parent_scopes[i];
        //     if self
        //         .newest_mem_locations_and_allocation_sizes_by_scope
        //         .contains_key(&scope)
        //     {
        //         let top_of_stack =
        //             self.newest_mem_locations_and_allocation_sizes_by_scope[&scope].0;
        //         for j in (i + 1)..parent_scopes.len() {
        //             self.newest_mem_locations_and_allocation_sizes_by_scope
        //                 .insert(parent_scopes[j], (top_of_stack, 0));
        //         }
        //         break;
        //     }
        //     if i == 0 {
        //         for j in 0..parent_scopes.len() {
        //             self.newest_mem_locations_and_allocation_sizes_by_scope
        //                 .insert(parent_scopes[j], (0, 0));
        //         }
        //     }
        // }
        // let curr_scope = *parent_scopes
        //     .last()
        //     .expect("This vector should never be empty");
        // dbg!(parent_scopes);
        // dbg!(curr_scope);
        let new_location = self.gen_location(size, curr_scope);
        if self.symbol_table.contains_key(&(name.clone(), curr_scope)) {
            fail!("Variable {name} already defined in scope '{curr_scope}");
        } else {
            self.symbol_table
                .insert((name, curr_scope), new_location.clone());
            let mut mem_loc_alloc = self.newest_mem_locations_and_allocation_sizes_by_scope[&curr_scope];
            mem_loc_alloc.0 += size;
            mem_loc_alloc.1 += size;
            self.newest_mem_locations_and_allocation_sizes_by_scope.insert(curr_scope, mem_loc_alloc);
        }
        new_location
    }

    #[allow(unused)]
    pub fn remove(&mut self, key: &(String, u64)) {
        self.symbol_table.remove(key);
    }

    pub fn get(&self, name: &String, scope: u64) -> Option<&String> {
        let mut key = (name.clone(), 0u64);
        let mut parent = Some(scope);
        while let Some(curr_scope) = parent {
            key.1 = curr_scope;
            let value = self.symbol_table.get(&key);
            if value.is_some() {
                return value;
            }
            parent = *self.parents.get(&curr_scope).expect("Scope not properly added with create scope, smh :(");
        }
        None
    }

    pub fn gen_location(&mut self, allocation: u64, scope: u64) -> String {
        let mut previous = self.newest_mem_locations_and_allocation_sizes_by_scope[&scope];
        previous.0 += allocation;
        previous.1 += allocation;
        *self
            .newest_mem_locations_and_allocation_sizes_by_scope
            .get_mut(&scope)
            .unwrap() = previous;
        format!(
            "-{}(%rbp)",
            self.newest_mem_locations_and_allocation_sizes_by_scope[&scope].0
        )
    }
}
