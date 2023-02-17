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
    BitwiseLeftShift,
    BitwiseRightShift,
    BitwiseOr,
    BitwiseAnd,
    Xor,
    GreaterEq,
    Greater,
    LessEq,
    Less,
    Equal,
    NotEqual,
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug)]
pub enum UnaryOperator {
    Negation,
    BitwiseComplement,
    LogicalNegation,
    PrefixIncrement(String),
    PrefixDecrement(String),
    PostfixIncrement(String),
    PostfixDecrement(String),
}

#[derive(Debug)]
pub enum Expression {
    Constant(i32),
    UnaryOp(UnaryOperator, Box<Expression>),
    BinaryOp(BinaryOperator, Box<Expression>, Box<Expression>),
    Assign(String, Box<Expression>),
    ReferenceVariable(String),
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>),
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
    Declare(String, Option<Expression>),
    Expression(Expression),
    Conditional(Expression, Vec<Statement>, Option<Vec<Statement>>),
}

#[derive(Debug)]
pub enum FuncDecl {
    Func(String, Vec<Statement>),
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
            FuncDecl::Func(ref indentifier, ref statements) => {
                println!("{indentation}fn {indentifier} -> int\n{indentation}params: ()\n{indentation}body:");
                for statement in statements {
                    statement.print(2);
                }
            }
        }
    }
}

impl Statement {
    pub fn print(&self, depth: usize) {
        let indentation = INDENT.repeat(depth);
        match self {
            Statement::Return(ref exp) => {
                print!("{indentation}return ");
                exp.print();
                println!();
            }
            Statement::Declare(name, expression) => {
                if let Some(expr) = expression {
                    print!("{indentation}int {name} = ");
                    expr.print();
                    println!();
                } else {
                    println!("{indentation}int {name}");
                }
            }
            Statement::Expression(expr) => {
                print!("{indentation}");
                expr.print();
                println!();
            }
            Statement::Conditional(expression, if_children, optional_else_children) => {
                print!("{indentation}if ");
                expression.print();
                println!(" :");
                for child in if_children {
                    child.print(depth + 1);
                }
                if let Some(else_children) = optional_else_children {
                    println!("{indentation}else: ");
                    for child in else_children {
                        child.print(depth + 1);
                    }
                }
            }
        }
    }
}

impl Expression {
    pub fn print(&self) {
        match self {
            Expression::Constant(int) => print!("int<{int}>"),
            Expression::UnaryOp(operator, expression) => {
                if matches!(
                    operator,
                    UnaryOperator::PostfixIncrement(_) | UnaryOperator::PostfixDecrement(_)
                ) {
                    print!("<");
                    expression.print();
                    print!(">");
                    operator.print();
                } else {
                    operator.print();
                    print!("<");
                    expression.print();
                    print!(">");
                }
            }
            Expression::BinaryOp(binary_operator, left_expr, right_expr) => {
                print!("(");
                left_expr.print();
                print!(" {binary_operator:?} ");
                right_expr.print();
                print!(")");
            }
            Expression::Assign(name, expression) => {
                // TODO: Maybe change so it doesn't always print "(" and only does it for
                // expressions, not assignment expressions
                print!("({name} = ");
                expression.print();
                print!(")");
            }
            Expression::ReferenceVariable(name) => print!("var<{name}>"),
            Expression::Ternary(exp1, exp2, exp3) => {
                print!("[");
                exp1.print();
                print!("? ");
                exp2.print();
                print!(" : ");
                exp3.print();
                print!("]");
            }
        }
    }
}

impl UnaryOperator {
    fn print(&self) {
        match self {
            UnaryOperator::Negation => print!("-"),
            UnaryOperator::LogicalNegation => print!("!"),
            UnaryOperator::BitwiseComplement => print!("~"),
            UnaryOperator::PrefixIncrement(_) => print!("++"),
            UnaryOperator::PostfixDecrement(_) => print!("--"),
            UnaryOperator::PrefixDecrement(_) => print!("--"),
            UnaryOperator::PostfixIncrement(_) => print!("++"),
        }
    }
}
