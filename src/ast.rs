pub const INDENT: &'static str = "    ";

#[derive(Debug)]
pub enum Exp {
    Integer(i32),
}

#[derive(Debug)]
pub enum Statement {
    Return(Exp),
}

#[derive(Debug)]
pub enum FuncDecl {
    Func(String, Statement),
}

#[derive(Debug)]
pub enum Prog {
    Prog(FuncDecl),
}

impl Prog {
    pub fn print(&self) {
        match self {
            Prog::Prog(ref func) => {
                println!("Program: ");
                func.print(1);
            }
        }
    }
}

impl FuncDecl {
    fn print(&self, depth: usize) {
        let indentation = INDENT.repeat(depth);
        match self {
            FuncDecl::Func(ref indentifier, ref statement) => {
                println!("{indentation}fn {indentifier} -> int\n{indentation}params: ()\n{indentation}body:");
                statement.print(2);
            }
        }
    }
} 

impl Statement {
    fn print(&self, depth: usize) {
        let indentation = INDENT.repeat(depth);
        match self {
            Statement::Return(ref exp) => {
                print!("{indentation}return ");
                exp.print();
            }
        }
    }
}

impl Exp {
    fn print(&self) {
        match self {
            Exp::Integer(int) => print!("Integer<{int}>"),
        }
    }
}
