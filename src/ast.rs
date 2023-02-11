// Indentation for pretty printing, made a constant so size can be easily changed
pub const INDENT: &str = "    ";

// These are all the enums necessary for the abstract syntax tree
// The grammer is as follows:
// Program ::= Function Decleration
// Function Decleration ::= (name, Statement)
// Statement ::= return(Expression)
// Expression ::= unaryop(operator, expression) | value

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Minus,
    Multiply,
    Divide,
    Modulo,
}

#[derive(Debug)]
pub enum UnaryOperator {
    Negation,
    BitwiseComplement,
    LogicalNegation,
}

#[derive(Debug)]
pub enum Expression {
    Constant(i32),
    UnaryOp(UnaryOperator, Box<Expression>),
    BinaryOp(BinaryOperator, Box<Expression>, Box<Expression>),
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
            Expression::BinaryOp(binary_operator, left_expr, right_expr) => {
                print!("(");
                left_expr.print();
                print!(" {binary_operator:?} ");
                right_expr.print();
                print!(")");
            }
        }
    }

    // fn gen_assembly(&self) -> String {
    //     match self {
    //         Expression::Constant()
    //     }
    // }
}

impl BinaryOperator {}
