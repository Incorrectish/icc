use crate::{
    assembly::*,
    ast::{self, BinaryOperator},
    lexer::Lexer,
    token::Token,
};

// does exactly what you think
pub fn fail(message: String) -> ! {
    eprintln!("{}: {message}", "Error".bold().red());
    std::process::exit(0);
}

use colored::Colorize;

pub struct Parser {
    lexer: Lexer,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Parser { lexer }
    }

    // begins the parsing process from the input lexer
    pub fn parse(&mut self) -> ast::Prog {
        ast::Prog::Prog(self.parse_func())
    }

    // This parses a function such as ret_type ident(params...) {statements...}
    fn parse_func(&mut self) -> ast::FuncDecl {
        let token = self.lexer.next().expect("Missing return type");
        if !matches!(token, Token::KeywordInt) {
            fail(format!("Needs int return type, got {token:?}"));
        }

        let token = self.lexer.next().expect("Missing function name");
        let indentifier = if let Token::Identifier(ident) = token {
            ident
        } else {
            fail(format!("Needs int return type, got {token:?}"));
        };

        let token = self.lexer.next().expect("Missing opening parentheses");
        if !matches!(token, Token::OpenParen) {
            fail(format!("Needs opening parentheses, got {token:?}"));
        }

        let token = self.lexer.next().expect("Missing closing parentheses");
        if !matches!(token, Token::CloseParen) {
            fail(format!("Needs closing parentheses, got {token:?}"));
        }

        let token = self.lexer.next().expect("Missing opening brace");
        if !matches!(token, Token::OpenBrace) {
            fail(format!("Needs opening brace, got {token:?}"));
        }

        // TODO: fix this in case it is not working

        let mut statements = vec![];
        while let Some(mut statement) = self.parse_statement() {
            statements.append(&mut statement);
        }

        let token = self.lexer.next().expect("Missing closing brace");
        if !matches!(token, Token::CloseBrace) {
            fail(format!("Needs closing brace, got {token:?}"));
        }

        ast::FuncDecl::Func(indentifier, statements)
    }

    // This returns a collection of statements. It returns none if the the token is a curly brace
    // eg for something like
    //  int a = 5;
    //  int b = 7;
    // }
    // it will return Some(vec[declare(a, 5)]), Some(vec[declare(a, 5)]), None
    fn parse_statement(&mut self) -> Option<Vec<ast::Statement>> {
        // let token = self.lexer.next().expect("Invalid token sequence");
        let token = self.lexer.peek().expect("Invalid token sequence");
        if matches!(token, Token::CloseBrace) {
            return None;
        }

        match token {
            Token::KeywordReturn => Some(vec![self.parse_return_statement()]),
            Token::KeywordIf => Some(vec![self.parse_if_statement()]),
            Token::KeywordInt => {
                let _ = self.lexer.next();
                Some(self.parse_assignment_statement())
            }
            Token::Identifier(_) => Some(vec![self.parse_expression_statement()]),
            Token::IntegerLiteral(_) => Some(vec![self.parse_expression_statement()]),
            _ => panic!("Is this the token: {token:?}?"),
        }
    }

    fn parse_if_statement(&mut self) -> ast::Statement {
        let Token::KeywordIf = self.lexer.next().expect("Should be if") else {
        unreachable!()
    };
        let Token::OpenParen = self.lexer.next().expect("Needs opening parentheses around if condition") else {
        fail(format!("Needed opening parens around if"));
    };
        let expression = self.parse_expression();
        let Token::CloseParen = self.lexer.next().expect("Needs closing parentheses around if condition") else {
        fail(format!("Needed closing parens around if"));
    };
        // TODO:
        let potential_open_brace = self.lexer.peek().expect("Needs statement after if");
        if let Token::OpenBrace = potential_open_brace {
            let _ = self.lexer.next();
            let mut if_children_statements = vec![];
            while let Some(mut statements) = self.parse_statement() {
                if_children_statements.append(&mut statements);
            }
            let Token::CloseBrace = self.lexer.next().expect("Needs closing brace") else {unreachable!()};
            let else_token = self.lexer.peek().expect("This is invalid, there should be a curly brace, signifying a block ending or a statement after the end of an if statement");
            if !matches!(else_token, Token::KeywordElse) {
                return ast::Statement::Conditional(expression, if_children_statements, None);
            }

            let _ = self.lexer.next();

            let potential_else_open_brace =
                self.lexer.peek().expect("Else cannot be the last token :(");
            if let Token::OpenBrace = potential_else_open_brace {
                let mut else_children_statements = vec![];
                while let Some(mut statements) = self.parse_statement() {
                    else_children_statements.append(&mut statements);
                }
                let Token::CloseBrace = self.lexer.next().expect("Needs closing brace") else {unreachable!()};
                return ast::Statement::Conditional(
                    expression,
                    if_children_statements,
                    Some(else_children_statements),
                );
            }

            let else_children_statement = self
                .parse_statement()
                .expect("Expected statement after else");
            ast::Statement::Conditional(
                expression,
                if_children_statements,
                Some(else_children_statement),
            )
        } else {
            let if_children_statement = self
                .parse_statement()
                .expect("Expected expression after if");
            let else_token = self.lexer.peek().expect("This is invalid, there should be a curly brace or statement after the end of an if statement");
            if !matches!(else_token, Token::KeywordElse) {
                let _ = self.lexer.next();
                let potential_else_open_brace =
                    self.lexer.peek().expect("Else cannot be the last token :(");
                if let Token::OpenBrace = potential_else_open_brace {
                    let mut else_children_statements = vec![];
                    while let Some(mut statements) = self.parse_statement() {
                        else_children_statements.append(&mut statements);
                    }
                    let Token::CloseBrace = self.lexer.next().expect("Needs closing brace") else {unreachable!()};
                    return ast::Statement::Conditional(
                        expression,
                        if_children_statement,
                        Some(else_children_statements),
                    );
                } else {
                    let else_children_statement = self
                        .parse_statement()
                        .expect("Expected statement after else");
                    ast::Statement::Conditional(
                        expression,
                        if_children_statement,
                        Some(else_children_statement),
                    )
                }
            } else {
                ast::Statement::Conditional(expression, if_children_statement, None)
            }
        }
    }

    fn parse_expression_statement(&mut self) -> ast::Statement {
        let statement = self.parse_expression();
        let token = self.lexer.next().expect("Missing semicolon");
        if !matches!(token, Token::Semicolon) {
            fail(format!("(within expression)Needs semicolon, got {token:?}"));
        }
        ast::Statement::Expression(statement)
    }

    fn parse_return_statement(&mut self) -> ast::Statement {
        if !matches!(
            self.lexer
                .next()
                .expect("This should always be a return keyword"),
            Token::KeywordReturn
        ) {
            fail("This should always be the return keyword".into());
        }
        let exp = self.parse_expression();
        let statement = ast::Statement::Return(exp);
        let token = self.lexer.next().expect("Invalid token sequence");
        if !matches!(token, Token::Semicolon) {
            fail(format!("Needs semicolon, got {token:?}"));
        }
        statement
    }

    fn parse_assignment_statement(&mut self) -> Vec<ast::Statement> {
        // if !matches!(
        //     self.lexer
        //         .next()
        //         .expect("This should always be a int keyword"),
        //     Token::KeywordInt
        // ) {
        //     fail("This should always be the int keyword".into());
        // }
        let token = self.lexer.next().expect("Invalid token sequence");
        if let Token::Identifier(variable_name) = token {
            let token = self.lexer.next().expect("Invalid token sequence");
            if let Token::Semicolon = token {
                vec![ast::Statement::Declare(variable_name, None)]
            } else if let Token::Assign = token {
                let exp = self.parse_expression();
                let statement = ast::Statement::Declare(variable_name, Some(exp));
                let token = self.lexer.next().expect("Invalid token sequence");
                if matches!(token, Token::Semicolon) {
                    vec![statement]
                } else if matches!(token, Token::Comma) {
                    let mut statements = vec![statement];
                    statements.append(&mut self.parse_assignment_statement());
                    statements
                } else {
                    fail(format!(
                        "(within assignment) Needs semicolon, got {token:?}"
                    ));
                }
            } else if let Token::Comma = token {
                let mut statements = vec![ast::Statement::Declare(variable_name, None)];
                statements.append(&mut self.parse_assignment_statement());
                statements
            } else {
                fail(format!(
                    "Expected assignment operator[=] or end of statement[;], got {token:?}"
                ));
            }
        } else {
            fail(format!("Expected variable name, got {token:?}"));
        }
    }

    // fn parse_expression(&mut self) -> ast::Expression {
    //     let mut term = self.parse_ternary();
    //     loop {
    //         let operation = self.lexer.peek();
    //         if !matches!(operation, Some(Token::LogicalOr)) {
    //             return term;
    //         }
    //         let operation = Self::parse_to_op(self.lexer.next().unwrap());
    //         let next_term = self.parse_ternary();
    //         term = ast::Expression::BinaryOp(operation, Box::new(term), Box::new(next_term));
    //     }
    // }

    // TODO: Error messages, error messages
    fn parse_expression(&mut self) -> ast::Expression {
        let mut term = self.parse_logical_or();
        loop {
            let operation = self.lexer.peek();
            if !matches!(operation, Some(Token::LogicalOr)) {
                return term;
            }
            let operation = Self::parse_to_op(self.lexer.next().unwrap());
            let next_term = self.parse_logical_or();
            term = ast::Expression::BinaryOp(operation, Box::new(term), Box::new(next_term));
        }
    }

    fn parse_logical_or(&mut self) -> ast::Expression {
        let mut term = self.parse_logical_and();
        loop {
            let operation = self.lexer.peek();
            if !matches!(operation, Some(Token::LogicalAnd)) {
                return term;
            }
            let operation = Self::parse_to_op(self.lexer.next().unwrap());
            let next_term = self.parse_logical_and();
            term = ast::Expression::BinaryOp(operation, Box::new(term), Box::new(next_term));
        }
    }

    fn parse_logical_and(&mut self) -> ast::Expression {
        let mut term = self.parse_bit_or();
        loop {
            let operation = self.lexer.peek();
            if !matches!(operation, Some(Token::BitwiseOr)) {
                return term;
            }
            let operation = Self::parse_to_op(self.lexer.next().unwrap());
            let next_term = self.parse_bit_or();
            term = ast::Expression::BinaryOp(operation, Box::new(term), Box::new(next_term));
        }
    }

    fn parse_bit_or(&mut self) -> ast::Expression {
        let mut term = self.parse_bit_xor();
        loop {
            let operation = self.lexer.peek();
            if !matches!(operation, Some(Token::Xor)) {
                return term;
            }
            let operation = Self::parse_to_op(self.lexer.next().unwrap());
            let next_term = self.parse_bit_xor();
            term = ast::Expression::BinaryOp(operation, Box::new(term), Box::new(next_term));
        }
    }

    fn parse_bit_xor(&mut self) -> ast::Expression {
        let mut term = self.parse_bit_and();
        loop {
            let operation = self.lexer.peek();
            if !matches!(operation, Some(Token::BitwiseAnd)) {
                return term;
            }
            let operation = Self::parse_to_op(self.lexer.next().unwrap());
            let next_term = self.parse_bit_and();
            term = ast::Expression::BinaryOp(operation, Box::new(term), Box::new(next_term));
        }
    }

    fn parse_bit_and(&mut self) -> ast::Expression {
        let mut term = self.parse_equality_expr();
        loop {
            let operation = self.lexer.peek();
            if !matches!(operation, Some(Token::Equal) | Some(Token::NotEqual)) {
                return term;
            }
            let operation = Self::parse_to_op(self.lexer.next().unwrap());
            let next_term = self.parse_equality_expr();
            term = ast::Expression::BinaryOp(operation, Box::new(term), Box::new(next_term));
        }
    }

    fn parse_equality_expr(&mut self) -> ast::Expression {
        let mut term = self.parse_relational_expr();
        loop {
            let operation = self.lexer.peek();
            if !matches!(
                operation,
                Some(Token::LessEq)
                    | Some(Token::GreaterEq)
                    | Some(Token::Less)
                    | Some(Token::Greater)
            ) {
                return term;
            }
            let operation = Self::parse_to_op(self.lexer.next().unwrap());
            let next_term = self.parse_relational_expr();
            term = ast::Expression::BinaryOp(operation, Box::new(term), Box::new(next_term));
        }
    }

    fn parse_relational_expr(&mut self) -> ast::Expression {
        let mut term = self.parse_bit_shift();
        loop {
            let operation = self.lexer.peek();
            if !matches!(
                operation,
                Some(Token::BitwiseLeftShift) | Some(Token::BitwiseRightShift)
            ) {
                return term;
            }
            let operation = Self::parse_to_op(self.lexer.next().unwrap());
            let next_term = self.parse_bit_shift();
            term = ast::Expression::BinaryOp(operation, Box::new(term), Box::new(next_term));
        }
    }

    fn parse_bit_shift(&mut self) -> ast::Expression {
        let mut term = self.parse_add_sub();
        loop {
            let operation = self.lexer.peek();
            if !matches!(operation, Some(Token::LogicalAnd)) {
                return term;
            }
            let operation = Self::parse_to_op(self.lexer.next().unwrap());
            let next_term = self.parse_add_sub();
            term = ast::Expression::BinaryOp(operation, Box::new(term), Box::new(next_term));
        }
    }

    fn parse_add_sub(&mut self) -> ast::Expression {
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
                Some(Token::Multiply) | Some(Token::Divide) | Some(Token::Modulo)
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
            let next_token = self.lexer.peek();
            match next_token {
                Some(Token::Identifier(other_name)) => { let _ = self.lexer.next(); ast::Expression::UnaryOp(ast::UnaryOperator::BitwiseComplement, Box::new(ast::Expression::ReferenceVariable(other_name)))},
                Some(Token::IntegerLiteral(int)) => { let _ = self.lexer.next(); ast::Expression::UnaryOp(ast::UnaryOperator::BitwiseComplement, Box::new(ast::Expression::Constant(int.parse().unwrap()))) },
                Some(Token::OpenParen) => ast::Expression::UnaryOp(ast::UnaryOperator::BitwiseComplement, Box::new(self.parse_expression())),
                _ => fail(format!("Unary operator must be succeeded by an expression, found {next_token:?} instead")),
            }
        },
        Some(Token::LogicalNot) => {
            let next_token = self.lexer.peek();
            match next_token {
                Some(Token::Identifier(other_name)) => { let _ = self.lexer.next(); ast::Expression::UnaryOp(ast::UnaryOperator::LogicalNegation, Box::new(ast::Expression::ReferenceVariable(other_name)))},
                Some(Token::IntegerLiteral(int)) => { let _ = self.lexer.next(); ast::Expression::UnaryOp(ast::UnaryOperator::LogicalNegation, Box::new(ast::Expression::Constant(int.parse().unwrap()))) },
                Some(Token::OpenParen) => ast::Expression::UnaryOp(ast::UnaryOperator::LogicalNegation, Box::new(self.parse_expression())),
                _ => fail(format!("Unary operator must be succeeded by an expression, found {next_token:?} instead")),
            }
        },
        Some(Token::Minus) => {
            let next_token = self.lexer.peek();
            match next_token {
                Some(Token::Identifier(other_name)) => { let _ = self.lexer.next(); ast::Expression::UnaryOp(ast::UnaryOperator::Negation, Box::new(ast::Expression::ReferenceVariable(other_name)))},
                Some(Token::IntegerLiteral(int)) => { let _ = self.lexer.next(); ast::Expression::UnaryOp(ast::UnaryOperator::Negation, Box::new(ast::Expression::Constant(int.parse().unwrap()))) },
                Some(Token::OpenParen) => ast::Expression::UnaryOp(ast::UnaryOperator::Negation, Box::new(self.parse_expression())),
                _ => fail(format!("Unary operator must be succeeded by an expression, found {next_token:?} instead")),
            }
        },
        Some(Token::PrefixDecrement(name)) => {
            let next_token = self.lexer.peek();
            match next_token {
                Some(Token::Identifier(other_name)) => { let _ = self.lexer.next(); ast::Expression::UnaryOp(ast::UnaryOperator::PrefixDecrement(name), Box::new(ast::Expression::ReferenceVariable(other_name)))},
                Some(Token::IntegerLiteral(int)) => { let _ = self.lexer.next(); ast::Expression::UnaryOp(ast::UnaryOperator::PrefixDecrement(name), Box::new(ast::Expression::Constant(int.parse().unwrap()))) },
                Some(Token::OpenParen) => ast::Expression::UnaryOp(ast::UnaryOperator::PrefixDecrement(name), Box::new(self.parse_expression())),
                _ => fail(format!("Unary operator must be succeeded by an expression, found {next_token:?} instead")),
            }
        },
        Some(Token::PrefixIncrement(name)) => {
            let next_token = self.lexer.peek();
            match next_token {
                Some(Token::Identifier(other_name)) => { let _ = self.lexer.next(); ast::Expression::UnaryOp(ast::UnaryOperator::PrefixIncrement(name), Box::new(ast::Expression::ReferenceVariable(other_name)))},
                Some(Token::IntegerLiteral(int)) => { let _ = self.lexer.next(); ast::Expression::UnaryOp(ast::UnaryOperator::PrefixIncrement(name), Box::new(ast::Expression::Constant(int.parse().unwrap()))) },
                Some(Token::OpenParen) => ast::Expression::UnaryOp(ast::UnaryOperator::PrefixIncrement(name), Box::new(self.parse_expression())),
                _ => fail(format!("Unary operator must be succeeded by an expression, found {next_token:?} instead")),
            }
        },
        Some(Token::OpenParen) => {
            let expression = self.parse_expression();
            let next_token = self.lexer.next();
            if !matches!(next_token, Some(Token::CloseParen)) {
                fail(format!("Token must be an closing parentheses, instead got {next_token:?}"));
            }
            expression
        },
        Some(Token::Identifier(name)) => {
            let next_token = self.lexer.peek();
            match next_token {
                Some(Token::Assign) => {
                    let _ = self.lexer.next();
                    ast::Expression::Assign(name, Box::new(self.parse_expression()))
                }
                Some(Token::AddAssign) | Some(Token::MulAssign) | Some(Token::ModAssign) | Some(Token::MinusAssign) | Some(Token::BitwiseAndAssign) | Some(Token::BitwiseOrAssign) | Some(Token::BitwiseLeftShiftAssign) | Some(Token::BitwiseRightShift) | Some(Token::XorAssign) => {
                    let _ = self.lexer.next();
                    let binary_operator = match next_token { Some(Token::AddAssign) => BinaryOperator::Add  ,| Some(Token::MulAssign) => BinaryOperator::Multiply  ,| Some(Token::ModAssign) => BinaryOperator::Modulo  ,| Some(Token::MinusAssign) => BinaryOperator::Minus  ,| Some(Token::BitwiseAndAssign) => BinaryOperator::BitwiseAnd  ,| Some(Token::BitwiseOrAssign) => BinaryOperator::BitwiseOr  ,| Some(Token::BitwiseLeftShiftAssign) => BinaryOperator::BitwiseLeftShift  ,| Some(Token::BitwiseRightShiftAssign) => BinaryOperator::BitwiseRightShift  ,| Some(Token::XorAssign) => BinaryOperator::Xor, _ => unreachable!(), };
                    ast::Expression::Assign(name.clone(), Box::new(ast::Expression::BinaryOp(binary_operator, Box::new(ast::Expression::ReferenceVariable(name)), Box::new(self.parse_expression()))))
                }
                Some(Token::PostfixIncrement(other_name)) => {
                    let _ = self.lexer.next();
                    ast::Expression::UnaryOp(ast::UnaryOperator::PostfixIncrement(other_name), Box::new(ast::Expression::ReferenceVariable(name)))
                }
                Some(Token::PostfixDecrement(other_name)) => {
                    let _ = self.lexer.next();
                    ast::Expression::UnaryOp(ast::UnaryOperator::PostfixDecrement(other_name), Box::new(ast::Expression::ReferenceVariable(name)))
                }
                _ => {
                    // fail(format!("Token must be assignment[=], instead got {next_token:?}"));
                    ast::Expression::ReferenceVariable(name)
                }
            }
        }
        _ => fail(format!("Token must be an expression, instead got {token:?}")),
    }
    }

    fn parse_to_op(operation: Token) -> ast::BinaryOperator {
        match operation {
            Token::Add => ast::BinaryOperator::Add,
            Token::Minus => ast::BinaryOperator::Minus,
            Token::Multiply => ast::BinaryOperator::Multiply,
            Token::Divide => ast::BinaryOperator::Divide,
            Token::Modulo => ast::BinaryOperator::Modulo,
            Token::BitwiseAnd => ast::BinaryOperator::BitwiseOr,
            Token::BitwiseOr => ast::BinaryOperator::BitwiseAnd,
            Token::Xor => ast::BinaryOperator::Xor,
            Token::BitwiseRightShift => ast::BinaryOperator::BitwiseRightShift,
            Token::BitwiseLeftShift => ast::BinaryOperator::BitwiseLeftShift,
            Token::Greater => ast::BinaryOperator::Greater,
            Token::GreaterEq => ast::BinaryOperator::GreaterEq,
            Token::Less => ast::BinaryOperator::Less,
            Token::LessEq => ast::BinaryOperator::LessEq,
            Token::Equal => ast::BinaryOperator::Equal,
            Token::NotEqual => ast::BinaryOperator::NotEqual,
            Token::LogicalOr => ast::BinaryOperator::LogicalOr,
            Token::LogicalAnd => ast::BinaryOperator::LogicalAnd,
            _ => unreachable!(),
        }
    }
}
