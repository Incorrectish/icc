use std::collections::HashMap;

use crate::{asm_gen::LONG_SIZE, fail};

// The start of the function parameters is 16 not 8 to make room for the return address of the
// function and the base pointer
pub const START_OF_FUNCTION_PARAMETERS: u64 = 16;

// #[repr(transparent)]
#[derive(Debug)]
pub struct SymbolTable {
    // maps variable + scope to location
    scoped_variables_to_location: HashMap<(String, u64), String>,
    // maps scopes to (top of stack, size of current scope)
    // TODO these can potentially be turned into vectors, including parents because scopes are
    // always 1..last scope, and just -1 to find them(indexing) or something
    scope_to_top_of_stack: HashMap<u64, u64>,
    size_of_scopes: HashMap<u64, u64>,
    // hopefully maps every scope to its parent, only scope 0 has no parent
    parents: HashMap<u64, Option<u64>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            scoped_variables_to_location: HashMap::default(),
            scope_to_top_of_stack: {
                let mut map = HashMap::new();
                map.insert(0, 0);
                map
            },
            size_of_scopes: {
                let mut map = HashMap::new();
                map.insert(0, 0);
                map
            },
            parents: {
                let mut map = HashMap::new();
                map.insert(0, None);
                map
            },
        }
    }

    // DO NOT EVER CALL THIS FUNCTION EXCEPT TO CREATE FUNCTION ARGUMENTS. IT PURPOSELY DOESN'T
    // ALLOCATE SPACE AND USES ADRESSES THAT WILL NEVER WORK OTHERWISE. This function also will not
    // deallocate the arguments properly, the caller MUST do that
    pub fn create_function_arguments(&mut self, scope: u64, arguments: &Vec<String>) {
        let mut top_of_stack = 16;
        for argument in arguments.iter().rev() {
            self.scoped_variables_to_location
                .insert((argument.clone(), scope), format!("{top_of_stack}(%rbp)"));
            top_of_stack += LONG_SIZE;
        }
    }

    pub fn get_scope_size(&self, scope: u64) -> u64 {
        self.size_of_scopes[&scope]
    }

    #[allow(unused)]
    pub fn print(&self) {
        println!("Symbol Table: ");
        for (key, value) in &self.scoped_variables_to_location {
            print!("    ");
            println!(
                "Variable \"{}\" in scope '{}, where the location is {}",
                key.0, key.1, value
            );
        }
    }

    pub fn create_scope(&mut self, scope: u64, parent: u64) {
        // println!("Creating scope '{scope} with parent '{parent}");
        self.parents.insert(scope, Some(parent));
        self.scope_to_top_of_stack.insert(
            scope,
            // keep the top of the stack pointer the same, but the allocation size for the
            // scope should be 0
            *self
                .scope_to_top_of_stack
                .get(&parent)
                .expect(&format!("scope '{scope} not created")),
        );
        self.size_of_scopes.insert(scope, 0);
    }

    pub fn allocate(
        &mut self,
        name: String,
        curr_scope: u64,
        size: u64, /* parent_scope: u64 */
    ) -> String {
        let new_location = self.gen_location(size, curr_scope);
        if self
            .scoped_variables_to_location
            .contains_key(&(name.clone(), curr_scope))
        {
            fail!("Variable {name} already defined in scope '{curr_scope}");
        } else {
            // TODO SO MANY BUGS maybe
            self.scoped_variables_to_location
                .insert((name, curr_scope), new_location.clone());
            // println!("symbol table is {:?}", self.symbol_table);
            // *self.size_of_scopes.get_mut(&curr_scope).expect("Scope '{curr_scope} doesn't have a top of stack") += size;
            // *self.scope_to_top_of_stack.get_mut(&curr_scope).expect("Scope '{curr_scope} doesn't have a size") += size;
        }
        println!("symbol table: \n{:#?}", self.scoped_variables_to_location);
        println!("symbol table: \n{:#?}", self.size_of_scopes);
        new_location
    }

    #[allow(unused)]
    pub fn remove(&mut self, key: &(String, u64)) {
        self.scoped_variables_to_location.remove(key);
    }

    pub fn get(&self, name: &String, scope: u64) -> Option<&String> {
        let mut key = (name.clone(), 0u64);
        let mut parent = Some(scope);
        while let Some(curr_scope) = parent {
            key.1 = curr_scope;
            let value = self.scoped_variables_to_location.get(&key);
            if value.is_some() {
                return value;
            }
            parent = *self
                .parents
                .get(&curr_scope)
                .expect("Scope not properly added with create scope, smh :(");
        }
        // self.debug_scope(scope);
        // println!("{:?}", self.symbol_table);
        None
    }

    #[allow(unused)]
    pub fn debug_scope(&self, scope: u64) {
        let mut parent = Some(scope);
        while let Some(curr_scope) = parent {
            print!("'{curr_scope}, ");
            parent = self.parents[&curr_scope];
        }
        println!("All the scopes folks");
    }

    pub fn gen_location(&mut self, allocation: u64, scope: u64) -> String {
        *self
            .size_of_scopes
            .get_mut(&scope)
            .expect("Scope '{curr_scope} doesn't have a top of stack") += allocation;
        *self
            .scope_to_top_of_stack
            .get_mut(&scope)
            .expect("Scope '{curr_scope} doesn't have a size") += allocation;
        format!("-{}(%rbp)", self.scope_to_top_of_stack[&scope])
    }
}
