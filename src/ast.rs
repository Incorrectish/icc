// Indentation for pretty printing, made a constant so size can be easily changed
pub const INDENT: &str = "    ";

// These are all the enums necessary for the abstract syntax tree
// The grammer is as follows:
// Program ::= Function Declaration
// Function Declaration ::= (name, Statement)
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
    Expression(Expression),
    Conditional(Expression, Box<Statement>, Box<Option<Statement>>),
    Block(Vec<BlockItem>),
}

#[derive(Debug)]
pub enum Declaration {
    Declare(String, Option<Expression>, Box<Option<Declaration>>),
}

#[derive(Debug)]
pub enum BlockItem {
    Statement(Statement),
    Declaration(Declaration),
}
// TODO: make declarations as the only part of an if statement without an associated block invalid

#[derive(Debug)]
pub enum FuncDecl {
    Func(String, Vec<BlockItem>),
}

#[derive(Debug)]
pub enum Prog {
    Prog(FuncDecl),
}

impl Prog {
    pub fn print(&self) {
        match self {
            Prog::Prog(ref func) => {
                let mut current_scope = 0;
                println!("'{current_scope}: Program: ");
                current_scope += 1;
                let curr_clone = current_scope;
                func.print(1, &mut current_scope, curr_clone);
            }
        }
    }
}

impl FuncDecl {
    fn print(&self, depth: usize, scope: &mut u64, parent_scope: u64) {
        let indentation = INDENT.repeat(depth);
        match self {
            FuncDecl::Func(ref indentifier, ref block_items) => {
                println!("{indentation}'{parent_scope}: fn {indentifier} -> int\n{indentation}'{parent_scope}: params: ()\n{indentation}'{parent_scope}: body:");
                *scope += 1;
                let curr_scope = *scope;
                for block_item in block_items {
                    block_item.print(2, scope, curr_scope);
                }
            }
        }
    }
}

impl BlockItem {
    pub fn print(&self, depth: usize, scope: &mut u64, parent_scope: u64) {
        match self {
            Self::Statement(statement) => statement.print(depth, scope, parent_scope),
            Self::Declaration(declaration) => declaration.print(depth, parent_scope),
        }
    }
}

impl Declaration {
    pub fn print(&self, depth: usize, parent_scope: u64) {
        match self {
            Self::Declare(name, optional_expression, optional_child_declaration) => {
                let indentation = INDENT.repeat(depth);
                if let Some(expr) = optional_expression {
                    print!("{indentation}'{parent_scope}: int {name} = ");
                    expr.print();
                    println!();
                } else {
                    println!("{indentation}'{parent_scope}: int {name}");
                }
                if let Some(child_declaration) = optional_child_declaration.as_ref() {
                    child_declaration.print(depth, parent_scope);
                }
            }
        }
    }
}

impl Statement {
    pub fn print(&self, depth: usize, scope: &mut u64, parent_scope: u64) {
        let indentation = INDENT.repeat(depth);
        match self {
            Statement::Return(ref exp) => {
                print!("{indentation}'{parent_scope}:return ");
                exp.print();
                println!();
            }
            Statement::Expression(expr) => {
                print!("{indentation}'{parent_scope}:");
                expr.print();
                println!();
            }
            Statement::Conditional(expression, if_child, optional_else_child) => {
                print!("{indentation}'{parent_scope}: if ");
                expression.print();
                println!(" :");
                // print!("'{parent_scope}:");
                if_child.print(depth + 1, scope, parent_scope);
                if let Some(else_child) = optional_else_child.as_ref() {
                    println!("{indentation}'{}: else: ", parent_scope);
                    else_child.print(depth + 1, scope, parent_scope);
                }
            }
            Self::Block(statements) => {
                *scope += 1;
                // current_scope += 1;
                let curr_scope = *scope;
                println!("{indentation}'{parent_scope}: begin");
                for statement in statements {
                    // print!("'{current_scope}:");
                    statement.print(depth + 1, scope, curr_scope);
                }
                println!("{indentation}'{}: end", parent_scope);
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
