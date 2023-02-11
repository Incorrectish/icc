use crate::{
    ast::{self, BinaryOperator},
    lexer::Lexer,
    token::Token,
};

use colored::Colorize;

pub struct Parser {
    lexer: Lexer,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Parser { lexer }
    }

    // does exactly what you think
    fn fail(message: String) -> ! {
        eprintln!("{}: {message}", "Error".bold().red());
        std::process::exit(0);
    }

    // begins the parsing process from the input lexer
    pub fn parse(&mut self) -> ast::Prog {
        ast::Prog::Prog(self.parse_func())
    }

    // This parses a function such as ret_type ident(params...) {statements...}
    fn parse_func(&mut self) -> ast::FuncDecl {
        let token = self.lexer.next().expect("Missing return type");
        if !matches!(token, Token::KeywordInt) {
            Self::fail(format!("Needs int return type, got {token:?}"));
        }

        let token = self.lexer.next().expect("Missing function name");
        let indentifier = if let Token::Identifier(ident) = token {
            ident
        } else {
            Self::fail(format!("Needs int return type, got {token:?}"));
        };

        let token = self.lexer.next().expect("Missing opening parentheses");
        if !matches!(token, Token::OpenParen) {
            Self::fail(format!("Needs opening parentheses, got {token:?}"));
        }

        let token = self.lexer.next().expect("Missing closing parentheses");
        if !matches!(token, Token::CloseParen) {
            Self::fail(format!("Needs closing parentheses, got {token:?}"));
        }

        let token = self.lexer.next().expect("Missing opening brace");
        if !matches!(token, Token::OpenBrace) {
            Self::fail(format!("Needs opening brace, got {token:?}"));
        }

        let statement = self.parse_statement();

        let token = self.lexer.next().expect("Missing closing brace");
        if !matches!(token, Token::CloseBrace) {
            Self::fail(format!("Needs closing brace, got {token:?}"));
        }

        ast::FuncDecl::Func(indentifier, statement)
    }

    fn parse_statement(&mut self) -> ast::Statement {
        let token = self.lexer.next().expect("Invalid token sequence");
        if !matches!(token, Token::KeywordReturn) {
            Self::fail(format!("Needs return keyword, got {token:?}"));
        }
        let exp = self.parse_expression();
        let statement = ast::Statement::Return(exp);
        let token = self.lexer.next().expect("Invalid token sequence");
        if !matches!(token, Token::Semicolon) {
            Self::fail(format!("Needs semicolon, got {token:?}"));
        }
        statement
    }

    // TODO: Error messages
    fn parse_expression(&mut self) -> ast::Expression {
        let mut term = self.parse_term();
        loop {
            let operation = self.lexer.peek();
            if !matches!(operation, Some(Token::Minus) | Some(Token::Add)) {
                return term;
            }
            let operation = Self::parse_to_op(self.lexer.next().unwrap());
            let next_term = self.parse_term();
            term = ast::Expression::BinaryOp(operation, Box::new(term), Box::new(next_term));
        }
    }

    fn parse_term(&mut self) -> ast::Expression {
        let mut term = self.parse_factor();
        loop {
            let operation = self.lexer.peek();
            if !matches!(
                operation,
                Some(Token::Multiplication) | Some(Token::Division)
            ) {
                return term;
            }
            let operation = Self::parse_to_op(self.lexer.next().unwrap());
            let next_term = self.parse_factor();
            term = ast::Expression::BinaryOp(operation, Box::new(term), Box::new(next_term));
        }
    }

    fn parse_factor(&mut self) -> ast::Expression {
        let token = self.lexer.next();
        match token {
            Some(Token::IntegerLiteral(int_literal)) => {
                ast::Expression::Constant(int_literal.parse().expect("This is guaranteed to be a valid integer because it can only be turned into a token if it is"))
            }
            Some(Token::BitwiseComplement) => {
                ast::Expression::UnaryOp(ast::UnaryOperator::BitwiseComplement, Box::new(self.parse_expression()))
            },
            Some(Token::LogicalNegation) => {
                ast::Expression::UnaryOp(ast::UnaryOperator::LogicalNegation, Box::new(self.parse_expression()))
            },
            Some(Token::Minus) => {
                ast::Expression::UnaryOp(ast::UnaryOperator::Negation, Box::new(self.parse_expression()))
            },
            Some(Token::OpenParen) => {
                let expression = self.parse_expression();
                let next_token = self.lexer.next();
                if !matches!(next_token, Some(Token::CloseParen)) {
                    Self::fail(format!("Token must be an closing parentheses, instead got {next_token:?}"));
                }
                expression
            }
            _ => Self::fail(format!("Token must be an expression, instead got {token:?}")),
        }
    }

    fn parse_to_op(operation: Token) -> ast::BinaryOperator {
        match operation {
            Token::Add => ast::BinaryOperator::Add,
            Token::Minus => ast::BinaryOperator::Minus,
            Token::Multiplication => ast::BinaryOperator::Multiply,
            Token::Division => ast::BinaryOperator::Divide,
            _ => unreachable!(),
        }
    }

    // Generates the assembly from the abstract syntax tree
    pub fn generate(ast: ast::Prog) -> String {
        match ast {
            // parses the program header
            ast::Prog::Prog(function_decleration) => Self::gen_function(function_decleration),
        }
    }

    fn gen_function(function_decleration: ast::FuncDecl) -> String {
        match function_decleration {
            ast::FuncDecl::Func(indentifier, statement) => {
                format!(
                    ".globl {indentifier}\n{indentifier}:\n{}",
                    Self::gen_statement(statement)
                )
            }
        }
    }

    fn gen_statement(statement: ast::Statement) -> String {
        match statement {
            // parses the statements
            ast::Statement::Return(expr) => {
                let constructed_assembly =
                    format!("{}\npopl %eax\nret", Self::gen_expression(expr, "%eax"));
                // TODO: if the assembly pushes something onto to the stack only to immediately pop
                // it off onto %eax, replace it with just moving to eax. For example
                // pushl $5
                // neg (esp)
                // popl %eax
                // ret
                // can become:
                // movl $5, %eax
                // neg %eax
                // ret
                constructed_assembly
            }
        }
    }

    fn gen_expression(expr: ast::Expression, location: &str) -> String {
        match expr {
            ast::Expression::Constant(int) => {
                format!("pushl ${int}")
            }
            ast::Expression::UnaryOp(operator, expression) => {
                format!(
                    "{new_expr}\n{operation}",
                    new_expr = Self::gen_expression(*expression, location),
                    operation = Self::operation(operator, location),
                )
            }
            ast::Expression::BinaryOp(binary_operator, left_expr, right_expr) => {
                let left_exp = Self::gen_expression(*left_expr, location);
                let right_exp = Self::gen_expression(*right_expr, location);
                let binop = Self::binary_operation(binary_operator);
                format!(
                    "{left_exp}\n{right_exp}\npopl %eax\npopl %ebx\n{binop} %ebx, %eax\npushl %eax"
                )
            }
        }
    }

    fn gen_assembly(binop: BinaryOperator, loc1: &str, loc2: &str) -> String {
        match binop {
            BinaryOperator::Add => {
                // result stored in the second register
                format!("addl {loc1}, {loc2}")
            }
            BinaryOperator::Minus => {
                format!("subl {loc1}, {loc2}")
            }
            BinaryOperator::Multiply => {
                format!("imul {loc1}, {loc2}")
            }
            BinaryOperator::Divide => {
                todo!()
            }
        }
    }

    fn operation(operator: ast::UnaryOperator, location: &str) -> String {
        match operator {
            ast::UnaryOperator::Negation => format!("neg {location}"),
            ast::UnaryOperator::BitwiseComplement => format!("not {location}"),
            ast::UnaryOperator::LogicalNegation => format!("cmpl $0, {location}\nsete {location}"),
        }
    }

    fn binary_operation(operator: ast::BinaryOperator) -> String {
        match operator {
            ast::BinaryOperator::Add => "addl".to_string(),
            ast::BinaryOperator::Minus => "subl".to_string(),
            ast::BinaryOperator::Multiply => "imul".to_string(),
            ast::BinaryOperator::Divide => todo!(),
        }
    }
}
