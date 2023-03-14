#[derive(Debug)]
pub struct Var {
    name: String,
    var_type: &'static str,
}

impl Var {
    pub fn new(name: String, var_type: &'static str) -> Self {
        Self {
            name, 
            var_type
        }
    }

    pub fn name(&self) -> &String {
        &self.name 
    }

    pub fn type_(&self) -> &'static str {
        self.var_type
    }
}
