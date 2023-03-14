use std::fmt::Display;

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

impl Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.var_type)
    }
}
