#![allow(unused)]

use crate::{ast, variable::Var, expression::Constant};

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
    Constant(Constant),
    UnaryOp(UnaryOperator, Box<Expression>),
    BinaryOp(BinaryOperator, Box<Expression>, Box<Expression>),
    Assign(String, Box<Expression>),
    ReferenceVariable(String),
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>),
    FunctionCall(String, Vec<Expression>),
    NullExp,
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
    Expression(Expression),
    Conditional(Expression, Box<Statement>, Box<Option<Statement>>),
    Block(Vec<BlockItem>),
    For(
        Option<Expression>,
        Expression,
        Option<Expression>,
        Box<Statement>,
    ),
    ForDecl(Declaration, Expression, Option<Expression>, Box<Statement>),
    While(Expression, Box<Statement>),
    Do(Box<Statement>, Expression),
    Break,
    Continue,
}

#[derive(Debug)]
pub enum Declaration {
    Declare(Var, Option<Expression>, Box<Option<Declaration>>),
}

#[derive(Debug)]
pub enum BlockItem {
    Statement(Statement),
    Declaration(Declaration),
}
// TODO: make declarations as the only part of an if statement without an associated block invalid

#[derive(Debug)]
pub enum FuncDecl {
    Func(Var, Vec<Var>, Vec<BlockItem>),
    FuncPrototype(Var, Vec<Var>),
}

#[derive(Debug)]
pub enum Prog {
    Prog(Vec<FuncDecl>),
}

impl Prog {
    pub fn print(&self) {
        match self {
            Prog::Prog(ref funcs) => {
                let mut current_scope = 0;
                let immut_curr_scope = current_scope;
                println!("'{current_scope}: Program: ");
                current_scope += 1;
                let curr_clone = current_scope;
                for func in funcs {
                    func.print(1, &mut current_scope, curr_clone);
                    println!();
                }
                println!("'{immut_curr_scope}: EndProgram");
            }
        }
    }
}

impl FuncDecl {
    pub fn print(&self, depth: usize, scope: &mut u64, parent_scope: u64) {
        let indentation = INDENT.repeat(depth);
        match self {
            FuncDecl::FuncPrototype(ref var, ref args) => {
                println!("{indentation}'{parent_scope}prototype\n{indentation}'{parent_scope}: fn {var}\n{indentation}'{parent_scope}: params: {args:?}\n{indentation})");
            }
            FuncDecl::Func(ref var, ref arguments, ref block_items) => {
                println!("{indentation}'{parent_scope}: fn {var}\n{indentation}'{parent_scope}: params: {arguments:?}\n{indentation}'{parent_scope}: begin:");
                *scope += 1;
                let curr_scope = *scope;
                for block_item in block_items {
                    block_item.print(2, scope, curr_scope);
                }
                println!("{indentation}'{parent_scope}: end")
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
            Self::Declare(var, optional_expression, optional_child_declaration) => {
                let indentation = INDENT.repeat(depth);
                if let Some(expr) = optional_expression {
                    print!("{indentation}'{parent_scope}: {var} = ");
                    expr.print();
                    println!();
                } else {
                    println!("{indentation}'{parent_scope}: {var} = ");
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
                print!("{indentation}'{parent_scope}: return ");
                exp.print();
                println!();
            }
            Statement::Expression(expr) => {
                print!("{indentation}'{parent_scope}: ");
                expr.print();
                println!();
            }
            Statement::Conditional(expression, if_child, optional_else_child) => {
                print!("{indentation}'{parent_scope}: if ");
                expression.print();
                println!();
                // println!(" :");
                // print!("'{parent_scope}:");
                if_child.print(depth + 1, scope, parent_scope);
                if let Some(else_child) = optional_else_child.as_ref() {
                    println!("{indentation}'{}: else: ", parent_scope);
                    else_child.print(depth + 1, scope, parent_scope);
                }
            }
            Self::Block(statements) => {
                *scope += 1;
                let lower_indent = INDENT.repeat(depth - 1);
                // current_scope += 1;
                let curr_scope = *scope;
                println!("{lower_indent}'{parent_scope}: begin");
                for statement in statements {
                    // print!("'{current_scope}:");
                    statement.print(depth, scope, curr_scope);
                }
                println!("{lower_indent}'{}: end", parent_scope);
            }
            Self::Continue => println!("{indentation}'{parent_scope}: continue"),
            Self::Break => println!("{indentation}'{parent_scope}: break"),
            Self::ForDecl(decl, exp2, opt_exp3, statement) => {
                print!("{indentation}'{parent_scope}: for ");
                print_for_decl(decl);
                print!(" , ");
                exp2.print();
                print!(" , ");
                if let Some(exp3) = opt_exp3 {
                    exp3.print();
                }
                println!();
                statement.print(depth + 1, scope, parent_scope);
            }
            Self::For(opt_exp1, exp, opt_exp2, statement) => {
                print!("{indentation}'{parent_scope}: for ");
                if let Some(exp1) = opt_exp1 {
                    exp1.print();
                }
                print!(" , ");
                exp.print();
                print!(" , ");
                if let Some(exp2) = opt_exp2 {
                    exp2.print();
                }
                println!();
                statement.print(depth + 1, scope, parent_scope);
            }
            Self::Do(statement, exp) => {
                println!("{indentation}'{parent_scope}: do ");
                statement.print(depth + 1, scope, parent_scope);
                print!("{indentation}'{parent_scope}: while ");
                exp.print();
                println!();
            }
            Self::While(exp, statement) => {
                print!("{indentation}'{parent_scope}: while ");
                exp.print();
                println!();
                statement.print(depth + 1, scope, parent_scope);
            }
        }
    }
}

pub fn print_for_decl(decl: &Declaration) {
    let mut opt_decl = Some(decl);
    while let Some(decl) = opt_decl {
        match decl {
            ast::Declaration::Declare(var, opt_exp, opt_decl2) => {
                print!("{var} = ");
                if let Some(exp) = opt_exp {
                    print!(" ");
                    exp.print();
                }
                opt_decl = opt_decl2.as_ref().as_ref();
                if opt_decl.is_some() {
                    print!(", ");
                }
            }
        }
    }
}

impl Expression {
    pub fn print(&self) {
        match self {
            Self::NullExp => print!(""),
            Expression::Constant(int) => print!("<{int:?}>"),
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
            Expression::FunctionCall(name, arguments) => {
                print!("({name} ");
                print!("[");
                for argument in arguments {
                    argument.print();
                    print!(", ")
                }
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
