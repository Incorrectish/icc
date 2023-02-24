use crate::{
    ast::{self, BinaryOperator},
    lexer::Lexer,
    token::Token,
};

// does exactly what you think
#[macro_export]
macro_rules! fail {
    ($fmt:expr, $($arg:tt)+) => (crate::parser::fail(format!($fmt, $($arg)+)));
    ($msg:expr) => (crate::parser::fail(format!($msg)));
}

pub fn fail(message: String) -> ! {
    eprintln!("{}: {message}", "Error".bold().red());
    std::process::exit(0);
}

use colored::Colorize;

#[derive(Debug)]
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
        while let Some(statement) = self.parse_block_item() {
            statements.push(statement);
        }

        let token = self.lexer.next().expect("Missing closing brace");
        if !matches!(token, Token::CloseBrace) {
            fail(format!("Needs closing brace, got {token:?}"));
        }

        ast::FuncDecl::Func(indentifier, statements)
    }

    fn parse_block_item(&mut self) -> Option<ast::BlockItem> {
        // let token = self.lexer.next().expect("Invalid token sequence");
        let token = self.lexer.peek().expect("Invalid token sequence");
        match token {
            Token::KeywordInt => {
                let _ = self.lexer.next();
                let ret = Some(ast::BlockItem::Declaration(
                    self.parse_assignment_statement(),
                ));
                let Some(Token::Semicolon) = self.lexer.next() else {
                    fail!("Missing semicolon, :(")
                };
                ret
            }
            _ => {
                if let Some(statement) = self.parse_statement() {
                    Some(ast::BlockItem::Statement(statement))
                } else {
                    None
                }
            }
        }
    }

    // This returns a collection of statements. It returns none if the the token is a curly brace
    // eg for something like
    //  int a = 5;
    //  int b = 7;
    // }
    // it will return Some(declare(a, 5)), Some(declare(a, 5)), None
    fn parse_statement(&mut self) -> Option<ast::Statement> {
        // let token = self.lexer.next().expect("Invalid token sequence");
        let token = self.lexer.peek().expect("Invalid token sequence");
        match token {
            Token::CloseBrace => None,
            Token::KeywordReturn => Some(self.parse_return_statement()),
            Token::KeywordIf => Some(self.parse_if_statement()),
            Token::KeywordInt => fail("Statements cannot be declarations!".into()),
            Token::Identifier(_)
            | Token::IntegerLiteral(_)
            | Token::PrefixDecrement(_)
            | Token::PrefixIncrement(_) => Some(self.parse_expression_statement()),
            Token::OpenBrace => {
                let _ = self.lexer.next();
                let mut block_statements = vec![];
                while let Some(statements) = self.parse_block_item() {
                    block_statements.push(statements);
                }
                let Token::CloseBrace = self.lexer.next().expect("Needs closing brace") else {unreachable!()};
                Some(ast::Statement::Block(block_statements))
            }
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
        let if_body = self
            .parse_statement()
            .expect("Body should not terminate with a curly brace");
        let potential_else = self.lexer.peek();
        let else_body = if let Some(Token::KeywordElse) = potential_else {
            let _ = self.lexer.next();
            self.parse_statement()
        } else {
            None
        };
        ast::Statement::Conditional(expression, Box::new(if_body), Box::new(else_body))
    }
    // fn parse_if_one_liner(&mut self, expression: Expression) -> ast::Statement {
    //     let if_children_statement = self
    //         .parse_statement()
    //         .expect("Expected expression after if");
    //     let else_token = self.lexer.peek().expect("This is invalid, there should be a curly brace or statement after the end of an if statement");
    //     if matches!(else_token, Token::KeywordElse) {
    //         let _ = self.lexer.next();
    //         let potential_else_open_brace =
    //             self.lexer.peek().expect("Else cannot be the last token :(");
    //         if let Token::OpenBrace = potential_else_open_brace {
    //             let _ = self.lexer.next();
    //             let else_children_statements = self.parse_block_of_statements();
    //             return ast::Statement::Conditional(
    //                 expression,
    //                 if_children_statement,
    //                 Some(else_children_statements),
    //             );
    //         } else {
    //             let else_children_statement = self
    //                 .parse_statement()
    //                 .expect("Expected statement after else");
    //             ast::Statement::Conditional(
    //                 expression,
    //                 if_children_statement,
    //                 Some(else_children_statement),
    //             )
    //         }
    //     } else {
    //         ast::Statement::Conditional(expression, if_children_statement, None)
    //     }
    // }

    // fn parse_if_curly_statement(&mut self, expression: Expression) -> ast::Statement {
    //     let _ = self.lexer.next();
    //     let if_children_statements = self.parse_block_of_statements();
    //     let conditional_else_token = self.lexer.peek();
    //     let Some(else_token) = conditional_else_token else {
    //             fail(format!("Wtf is this token: {conditional_else_token:?}, and this lexer: {:?}", self.lexer.get_rest_of_input()))
    //         };
    //     if !matches!(else_token, Token::KeywordElse) {
    //         return ast::Statement::Conditional(expression, if_children_statements, None);
    //     }
    //
    //     let _ = self.lexer.next();
    //
    //     let potential_else_open_brace =
    //         self.lexer.peek().expect("Else cannot be the last token :(");
    //     if let Token::OpenBrace = potential_else_open_brace {
    //         let _ = self.lexer.next();
    //         let else_children_statements = self.parse_block_of_statements();
    //         return ast::Statement::Conditional(
    //             expression,
    //             if_children_statements,
    //             Some(else_children_statements),
    //         );
    //     }
    //
    //     let else_children_statement = self
    //         .parse_statement()
    //         .expect("Expected statement after else");
    //     ast::Statement::Conditional(
    //         expression,
    //         if_children_statements,
    //         Some(else_children_statement),
    //     )
    // }

    fn parse_expression_statement(&mut self) -> ast::Statement {
        let statement = self.parse_expression();
        let token = self.lexer.next().expect("Missing semicolon");
        if !matches!(token, Token::Semicolon) {
            dbg!(self);
            fail!("(within expression)Needs semicolon, got {token:?}");
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

    fn parse_assignment_statement(&mut self) -> ast::Declaration {
        let token = self.lexer.next().expect("Invalid token sequence");
        if let Token::Identifier(variable_name) = token {
            let token = self.lexer.peek().expect("Invalid token sequence");
            let (expression, next_assignment) = match token {
                Token::Semicolon => (None, None),
                Token::Assign => {
                    let _ = self.lexer.next();
                    let exp = Some(self.parse_expression());
                    let next_tok = self.lexer.peek();
                    let next_decl = if let Some(Token::Comma) = next_tok {
                        let _ = self.lexer.next();
                        Some(self.parse_assignment_statement())
                    } else if let Some(Token::Semicolon) = next_tok {
                        None
                    } else {
                        fail(format!("Expected semicolon or comma"))
                    };
                    (exp, next_decl)
                },
                Token::Comma => {let _ = self.lexer.next(); (None, Some(self.parse_assignment_statement()))},
                _ => fail(format!(
                    "Expected assignment operator[=] or end of statement[;], got {token:?}, ast Node until now = :?"
                )),
            };
            ast::Declaration::Declare(variable_name, expression, Box::new(next_assignment))
        } else {
            fail(format!("Expected variable name, got {token:?}"));
        }
    }

    // fn parse_expression(&mut self) -> ast::Expression {
    //     let term_one = self.parse_ternary();
    //     let operation = self.lexer.peek();
    //     if !matches!(operation, Some(Token::Question)) {
    //         return term_one;
    //     }
    //     let _ = self.lexer.next();
    //     let mut term_two = self.parse_ternary();
    //     let operation = self.lexer.peek();
    //     if matches!(operation, Some(Token::Question)) {
    //         let _ = self.lexer.next();
    //         term_two = self.parse_partial_ternary(term_two);
    //         let operation_two = self.lexer.peek();
    //         // potential while loop
    //     } else if matches!(operation, Some(Token::Colon)) {
    //         // TODO:
    //         let _ = self.lexer.next();
    //         let term_three = self.parse_ternary();
    //         ast::Expression::Ternary(Box::new(term_one), Box::new(term_two), Box::new(term_three))
    //     } else {
    //         term_one.print();
    //         println!();
    //         term_two.print();
    //         println!();
    //         fail(format!(
    //         "Ternary must seperate conditional expressions with \":\". Instead got {operation:?}"
    //     ));
    //     }
    // }
    //
    // fn parse_partial_ternary(&mut self, term_one: ast::Expression) -> ast::Expression {
    //     let term_two = self.parse_expression();
    //     let operation = self.lexer.peek();
    //     if !matches!(operation, Some(Token::Colon)) {
    //         fail(format!("Ternary must seperate conditional expressions with \":\". Instead got {operation:?}"));
    //     }
    //     let _ = self.lexer.next();
    //     let term_three = self.parse_expression();
    //     ast::Expression::Ternary(Box::new(term_one), Box::new(term_two), Box::new(term_three))
    // }

    // TODO: Error messages, error messages
    fn parse_expression(&mut self) -> ast::Expression {
        // TODO:
        self.parse_conditional_expr()
    }

    fn parse_conditional_expr(&mut self) -> ast::Expression {
        let term_one = self.parse_logical_or_expr();
        let token = self.lexer.peek();
        if matches!(token, Some(Token::Question)) {
            let _ = self.lexer.next();
            let term_two = self.parse_expression();
            let token = self.lexer.peek();
            if !matches!(token, Some(Token::Colon)) {
                fail(format!(
                    "Expected \":\" after ternary expression, found {token:?}"
                ));
            }
            let _ = self.lexer.next();
            let term_three = self.parse_conditional_expr();
            ast::Expression::Ternary(Box::new(term_one), Box::new(term_two), Box::new(term_three))
        } else {
            term_one
        }
    }

    fn parse_logical_or_expr(&mut self) -> ast::Expression {
        let mut term = self.parse_logical_and_expr();
        loop {
            let operation = self.lexer.peek();
            if !matches!(operation, Some(Token::LogicalOr)) {
                return term;
            }
            let operation = Self::parse_to_op(self.lexer.next().unwrap());
            let next_term = self.parse_logical_and_expr();
            term = ast::Expression::BinaryOp(operation, Box::new(term), Box::new(next_term));
        }
    }

    fn parse_logical_and_expr(&mut self) -> ast::Expression {
        let mut term = self.parse_bit_or_expr();
        loop {
            let operation = self.lexer.peek();
            if !matches!(operation, Some(Token::LogicalAnd)) {
                return term;
            }
            let operation = Self::parse_to_op(self.lexer.next().unwrap());
            let next_term = self.parse_bit_or_expr();
            term = ast::Expression::BinaryOp(operation, Box::new(term), Box::new(next_term));
        }
    }

    fn parse_bit_or_expr(&mut self) -> ast::Expression {
        let mut term = self.parse_bit_xor_expr();
        loop {
            let operation = self.lexer.peek();
            if !matches!(operation, Some(Token::BitwiseOr)) {
                return term;
            }
            let operation = Self::parse_to_op(self.lexer.next().unwrap());
            let next_term = self.parse_bit_xor_expr();
            term = ast::Expression::BinaryOp(operation, Box::new(term), Box::new(next_term));
        }
    }

    fn parse_bit_xor_expr(&mut self) -> ast::Expression {
        let mut term = self.parse_bit_and_expr();
        loop {
            let operation = self.lexer.peek();
            if !matches!(operation, Some(Token::Xor)) {
                return term;
            }
            let operation = Self::parse_to_op(self.lexer.next().unwrap());
            let next_term = self.parse_bit_and_expr();
            term = ast::Expression::BinaryOp(operation, Box::new(term), Box::new(next_term));
        }
    }

    fn parse_bit_and_expr(&mut self) -> ast::Expression {
        let mut term = self.parse_equality_expr();
        loop {
            let operation = self.lexer.peek();
            if !matches!(operation, Some(Token::BitwiseAnd)) {
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
            if !matches!(operation, Some(Token::Equal) | Some(Token::NotEqual)) {
                return term;
            }
            let operation = Self::parse_to_op(self.lexer.next().unwrap());
            let next_term = self.parse_relational_expr();
            term = ast::Expression::BinaryOp(operation, Box::new(term), Box::new(next_term));
        }
    }

    fn parse_relational_expr(&mut self) -> ast::Expression {
        let mut term = self.parse_bit_shift_expr();
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
            let next_term = self.parse_bit_shift_expr();
            term = ast::Expression::BinaryOp(operation, Box::new(term), Box::new(next_term));
        }
    }

    fn parse_bit_shift_expr(&mut self) -> ast::Expression {
        let mut term = self.parse_additive_expr();
        loop {
            let operation = self.lexer.peek();
            if !matches!(
                operation,
                Some(Token::BitwiseLeftShift) | Some(Token::BitwiseRightShift)
            ) {
                return term;
            }
            let operation = Self::parse_to_op(self.lexer.next().unwrap());
            let next_term = self.parse_additive_expr();
            term = ast::Expression::BinaryOp(operation, Box::new(term), Box::new(next_term));
        }
    }

    fn parse_additive_expr(&mut self) -> ast::Expression {
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
