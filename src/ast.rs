// Indentation for pretty printing, made a constant so size can be easily changed
pub const INDENT: &str = "    ";

// These are all the enums necessary for the abstract syntax tree
// The grammer is as follows:
// Program ::= Function Decleration
// Function Decleration ::= (name, Statement)
// Statement ::= return(Expression)
// Expression ::= value

#[derive(Debug)]
pub enum Operator {
    Negation,
    BitwiseComplement,
    LogicalNegation,
}

#[derive(Debug)]
pub enum Expression {
    Constant(i32),
    UnaryOp(Operator, Box<Expression>),
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
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

impl Expression {
    fn print(&self) {
        match self {
            Expression::Constant(int) => print!("int<{int}>"),
            Expression::UnaryOp(operator, expression) => {
                print!("{operator:?}<");
                expression.print();
                print!(">");
            }
        }
    }

    // fn gen_assembly(&self) -> String {
    //     match self {
    //         Expression::Constant()
    //     }
    // }
}
