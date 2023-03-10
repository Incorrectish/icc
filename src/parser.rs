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
    panic!("{}: {message}", "Error".bold().red());
    // eprintln!("{}: {message}", "Error".bold().red());
    // std::process::exit(0);
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
        let mut functions = vec![];
        while let Some(function) = self.parse_func() {
            functions.push(function);
        }
        ast::Prog::Prog(functions)
    }

    // This parses a function such as ret_type ident(params...) {statements...}
    fn parse_func(&mut self) -> Option<ast::FuncDecl> {
        let _ = self.lexer.peek()?; // returns none if there are no tokens left
        let token = self.lexer.next();
        if !matches!(token, Some(Token::KeywordInt)) {
            fail!("Needs int return type, got {token:?}");
        }

        let token = self.lexer.next().expect("Missing function name");
        let indentifier = if let Token::Identifier(ident) = token {
            ident
        } else {
            fail!("Needs int return type, got {token:?}");
        };

        let token = self.lexer.next().expect("Missing opening parentheses");
        if !matches!(token, Token::OpenParen) {
            fail!("Needs opening parentheses, got {token:?}");
        }

        // parse arguments:
        let args = self.parse_arguments();

        let token = self.lexer.next().expect("Missing closing parentheses");
        if !matches!(token, Token::CloseParen) {
            fail!("Needs closing parentheses, got {token:?}");
        }

        let token = self.lexer.next().expect("Missing opening brace");
        match token {
            Token::Semicolon => {
                return Some(ast::FuncDecl::FuncPrototype(indentifier, args));
            }
            Token::OpenBrace => {}
            _ => {
                self.lexer.print_line_with_caret();
                fail!("Expected `{{` or `;`, found {token:?}")
            }
        }

        // TODO: fix this in case it is not working

        let mut statements = vec![];
        while let Some(statement) = self.parse_block_item() {
            statements.push(statement);
        }

        let token = self.lexer.next().expect("Missing closing brace");
        if !matches!(token, Token::CloseBrace) {
            fail!("Needs closing brace, got {token:?}");
        }

        Some(ast::FuncDecl::Func(indentifier, args, statements))
    }

    // gets all the arguments
    fn parse_arguments(&mut self) -> Vec<String> {
        let mut args = vec![];
        while let Some(Token::KeywordInt) = self.lexer.peek() {
            // dbg!("hi");
            // self.lexer.print_line_with_caret();
            let _ = self.lexer.next();
            let ident = self.lexer.next();
            match ident {
                Some(Token::Identifier(name)) => args.push(name),
                _ => {
                    self.lexer.print_line_with_caret();
                    fail!("Expected `identifier` found token {ident:?}");
                }
            }
            let next_tok = self.lexer.peek();
            match next_tok {
                None => {
                    self.lexer.print_line_with_caret();
                    fail!("Expected something found nothing");
                }
                Some(Token::CloseParen) => return args,
                Some(Token::Comma) => {
                    let _ = self.lexer.next();
                }
                _ => {
                    self.lexer.print_line_with_caret();
                    fail!("Expected `,` or `)`, found `{next_tok:?}` ")
                }
            }
        }
        args
    }

    fn parse_block_item(&mut self) -> Option<ast::BlockItem> {
        // let token = self.lexer.next().expect("Invalid token sequence");
        let token = self.lexer.peek().expect("Invalid token sequence");
        match token {
            Token::KeywordInt => {
                let _ = self.lexer.next();
                self.lexer.peek();
                let ret = Some(ast::BlockItem::Declaration(self.parse_declaration()));
                // let Some(Token::Semicolon) = self.lexer.next() else {
                //     fail!("Missing semicolon, :( {:?}", self.lexer.clone().collect::<Vec<_>>()
                // };
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
            Token::KeywordBreak => {
                let _ = self.lexer.next();
                let next = self.lexer.next();
                if !matches!(next, Some(Token::Semicolon)) {
                    fail!("Missing semicolon, got {token:?} instead");
                }
                Some(ast::Statement::Break)
            }
            Token::KeywordFor => self.parse_for_loop(),
            Token::KeywordDo => {
                let _ = self.lexer.next();
                let statement = self
                    .parse_statement()
                    .expect("Expected statement after `do`");
                let token = self.lexer.next();
                if !matches!(token, Some(Token::KeywordWhile)) {
                    fail!("Expected while, got {token:?}");
                }
                let token = self.lexer.next();
                if !matches!(token, Some(Token::OpenParen)) {
                    fail!("Expected obening parentheses, got {token:?}");
                }
                let expression = self.parse_expression();
                let token = self.lexer.next();
                if !matches!(token, Some(Token::CloseParen)) {
                    fail!("Expected closing parentheses, got {token:?}");
                }
                let token = self.lexer.next();
                if !matches!(token, Some(Token::Semicolon)) {
                    fail!("Expected semicolon, got {token:?}");
                }
                Some(ast::Statement::Do(Box::new(statement), expression))
            }
            Token::KeywordWhile => {
                let _ = self.lexer.next();
                let token = self.lexer.next();
                if !matches!(token, Some(Token::OpenParen)) {
                    fail!("Expected opening parentheses, got {token:?}");
                }
                let expression = self.parse_expression();
                let token = self.lexer.next();
                if !matches!(token, Some(Token::CloseParen)) {
                    fail!("Expected closing parentheses, got {token:?}");
                }
                let statement = self
                    .parse_statement()
                    .expect("Expected body, got closing curly brace");
                Some(ast::Statement::While(expression, Box::new(statement)))
            }
            Token::Semicolon => {
                let _ = self.lexer.next();
                Some(ast::Statement::Expression(ast::Expression::NullExp))
                // Token::Semicolon => todo!("Null statement"),
            }
            Token::KeywordContinue => {
                let _ = self.lexer.next();
                let next = self.lexer.next();
                if !matches!(next, Some(Token::Semicolon)) {
                    fail!("Missing semicolon, got {token:?} instead");
                }
                Some(ast::Statement::Continue)
            }
            _ => panic!("Is this the token: {token:?}?"),
        }
    }

    fn parse_for_loop(&mut self) -> Option<ast::Statement> {
        let _ = self.lexer.next();
        let open_paren = self.lexer.next();
        if !matches!(open_paren, Some(Token::OpenParen)) {
            fail!("Expected opening parentheses, got {open_paren:?}");
        }
        let potential_decl = self.lexer.peek();
        if matches!(potential_decl, Some(Token::KeywordInt)) {
            let _ = self.lexer.next();
            let declaration = self.parse_declaration();
            let expression = self.parse_expression();
            let token = self.lexer.next();
            if !matches!(token, Some(Token::Semicolon)) {
                fail!("Expected semicolon, got {token:?}")
            }
            let token = self.lexer.peek();
            if let Some(Token::CloseParen) = token {
                let _ = self.lexer.next();
                let statement = self
                    .parse_statement()
                    .expect("For loop needs a statement succeeding it");
                Some(ast::Statement::ForDecl(
                    declaration,
                    expression,
                    None,
                    Box::new(statement),
                ))
            } else {
                let exp2 = self.parse_expression();
                let token = self.lexer.next();
                if !matches!(token, Some(Token::CloseParen)) {
                    fail!("Expected semicolon, got {token:?}")
                }
                let statement = self
                    .parse_statement()
                    .expect("For loop needs a statement succeeding it");
                Some(ast::Statement::ForDecl(
                    declaration,
                    expression,
                    Some(exp2),
                    Box::new(statement),
                ))
            }
        } else {
            let exp1 = self.parse_expression();
            let token = self.lexer.next();
            if !matches!(token, Some(Token::Semicolon)) {
                fail!("Expected semicolon, got {token:?}")
            }
            let expression = self.parse_expression();
            let token = self.lexer.next();
            if !matches!(token, Some(Token::Semicolon)) {
                fail!("Expected semicolon, got {token:?}")
            }
            let token = self.lexer.peek();
            if let Some(Token::CloseParen) = token {
                let _ = self.lexer.next();
                let statement = self
                    .parse_statement()
                    .expect("For loop needs a statement succeeding it");
                Some(ast::Statement::For(
                    Some(exp1),
                    expression,
                    None,
                    Box::new(statement),
                ))
            } else {
                let exp2 = self.parse_expression();
                let token = self.lexer.next();
                if !matches!(token, Some(Token::CloseParen)) {
                    fail!("Expected closing parentheses, got {token:?}")
                }
                let statement = self
                    .parse_statement()
                    .expect("For loop needs a statement succeeding it");
                Some(ast::Statement::For(
                    Some(exp1),
                    expression,
                    Some(exp2),
                    Box::new(statement),
                ))
            }
        }
    }

    fn parse_if_statement(&mut self) -> ast::Statement {
        let Token::KeywordIf = self.lexer.next().expect("Should be if") else {
        unreachable!()
    };
        let Token::OpenParen = self.lexer.next().expect("Needs opening parentheses around if condition") else {
        fail!("Needed opening parens around if");
    };
        let expression = self.parse_expression();
        let Token::CloseParen = self.lexer.next().expect("Needs closing parentheses around if condition") else {
        fail!("Needed closing parens around if");
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
    //             fail!("Wtf is this token: {conditional_else_token:?}, and this lexer: {:?}", self.lexer.get_rest_of_input())
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
            self.lexer.print_line_with_caret();
            fail!(
                "Missing semicolon on line {}, got {token:?}",
                self.lexer.line_number()
            );
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
            fail!("Needs semicolon, got {token:?}");
        }
        statement
    }

    fn parse_declaration(&mut self) -> ast::Declaration {
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
                        Some(self.parse_declaration())
                    } else if let Some(Token::Semicolon) = next_tok {
                        let _semicolon = self.lexer.next();
                        // let see = self.lexer.next();
                        // dbg!(see);
                        None
                    } else {
                        self.lexer.print_line_with_caret();
                        fail!("Expected semicolon or comma")
                    };
                    (exp, next_decl)
                },
                Token::Comma => {let _ = self.lexer.next(); (None, Some(self.parse_declaration()))},
                _ => fail!(
                    "Expected assignment operator[=] or end of statement[;], got {token:?}, ast Node until now = :?"
                ),
            };
            ast::Declaration::Declare(variable_name, expression, Box::new(next_assignment))
        } else {
            fail!("Expected variable name, got {token:?}");
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
    //         fail!(
    //         "Ternary must seperate conditional expressions with `:`. Instead got {operation:?}"
    //     );
    //     }
    // }
    //
    // fn parse_partial_ternary(&mut self, term_one: ast::Expression) -> ast::Expression {
    //     let term_two = self.parse_expression();
    //     let operation = self.lexer.peek();
    //     if !matches!(operation, Some(Token::Colon)) {
    //         fail!("Ternary must seperate conditional expressions with `:`. Instead got {operation:?}");
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

    fn parse_function_call(&mut self, name: String) -> ast::Expression {
        let mut args = vec![];
        loop {
            if let Some(Token::CloseParen) = self.lexer.peek() {
                return ast::Expression::FunctionCall(name, args);
            }
            let expression = self.parse_expression();
            args.push(expression);
            let next = self.lexer.peek();
            match next {
                Some(Token::Comma) => {
                    let _ = self.lexer.next();
                }
                Some(Token::CloseParen) => {
                    let _ = self.lexer.next();
                    break;
                }
                _ => {
                    self.lexer.print_line_with_caret();
                    fail!("Expected `;` or `)`, found {next:?}");
                }
            }
        }
        ast::Expression::FunctionCall(name, args)
    }

    fn parse_conditional_expr(&mut self) -> ast::Expression {
        let term_one = self.parse_logical_or_expr();
        let token = self.lexer.peek();
        if matches!(token, Some(Token::Question)) {
            let _ = self.lexer.next();
            let term_two = self.parse_expression();
            let token = self.lexer.peek();
            if !matches!(token, Some(Token::Colon)) {
                fail!("Expected `:` after ternary expression, found {token:?}");
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
            if !matches!(operation, Some(Token::EqualTo) | Some(Token::NotEqual)) {
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
                Some(Token::LessThanOrEqualTo)
                    | Some(Token::GreaterThanOrEqualTo)
                    | Some(Token::LessThan)
                    | Some(Token::GreaterThan)
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
                _ => fail!("Unary operator must be succeeded by an expression, found {next_token:?} instead"),
            }
        },
        Some(Token::LogicalNot) => {
            let next_token = self.lexer.peek();
            match next_token {
                Some(Token::Identifier(other_name)) => { let _ = self.lexer.next(); ast::Expression::UnaryOp(ast::UnaryOperator::LogicalNegation, Box::new(ast::Expression::ReferenceVariable(other_name)))},
                Some(Token::IntegerLiteral(int)) => { let _ = self.lexer.next(); ast::Expression::UnaryOp(ast::UnaryOperator::LogicalNegation, Box::new(ast::Expression::Constant(int.parse().unwrap()))) },
                Some(Token::OpenParen) => ast::Expression::UnaryOp(ast::UnaryOperator::LogicalNegation, Box::new(self.parse_expression())),
                _ => fail!("Unary operator must be succeeded by an expression, found {next_token:?} instead"),
            }
        },
        Some(Token::Minus) => {
            let next_token = self.lexer.peek();
            match next_token {
                Some(Token::Identifier(other_name)) => { let _ = self.lexer.next(); ast::Expression::UnaryOp(ast::UnaryOperator::Negation, Box::new(ast::Expression::ReferenceVariable(other_name)))},
                Some(Token::IntegerLiteral(int)) => { let _ = self.lexer.next(); ast::Expression::UnaryOp(ast::UnaryOperator::Negation, Box::new(ast::Expression::Constant(int.parse().unwrap()))) },
                Some(Token::OpenParen) => ast::Expression::UnaryOp(ast::UnaryOperator::Negation, Box::new(self.parse_expression())),
                _ => fail!("Unary operator must be succeeded by an expression, found {next_token:?} instead"),
            }
        },
        Some(Token::PrefixDecrement(name)) => {
            let next_token = self.lexer.peek();
            match next_token {
                Some(Token::Identifier(other_name)) => {
                    let _ = self.lexer.next(); 
                    ast::Expression::UnaryOp(ast::UnaryOperator::PrefixDecrement(name), Box::new(ast::Expression::ReferenceVariable(other_name)))
                },
                Some(Token::IntegerLiteral(int)) => { let _ = self.lexer.next(); ast::Expression::UnaryOp(ast::UnaryOperator::PrefixDecrement(name), Box::new(ast::Expression::Constant(int.parse().unwrap()))) },
                Some(Token::OpenParen) => ast::Expression::UnaryOp(ast::UnaryOperator::PrefixDecrement(name), Box::new(self.parse_expression())),
                _ => fail!("Unary operator must be succeeded by an expression, found {next_token:?} instead"),
            }
        },
        Some(Token::PrefixIncrement(name)) => {
            let next_token = self.lexer.peek();
            match next_token {
                Some(Token::Identifier(other_name)) => { let _ = self.lexer.next(); ast::Expression::UnaryOp(ast::UnaryOperator::PrefixIncrement(name), Box::new(ast::Expression::ReferenceVariable(other_name)))},
                Some(Token::IntegerLiteral(int)) => { let _ = self.lexer.next(); ast::Expression::UnaryOp(ast::UnaryOperator::PrefixIncrement(name), Box::new(ast::Expression::Constant(int.parse().unwrap()))) },
                Some(Token::OpenParen) => ast::Expression::UnaryOp(ast::UnaryOperator::PrefixIncrement(name), Box::new(self.parse_expression())),
                _ => fail!("Unary operator must be succeeded by an expression, found {next_token:?} instead"),
            }
        },
        Some(Token::OpenParen) => {
            let expression = self.parse_expression();
            let next_token = self.lexer.next();
            if !matches!(next_token, Some(Token::CloseParen)) {
                fail!("Token must be an closing parentheses, instead got {next_token:?}");
            }
            expression
        },
        Some(Token::Identifier(name)) => {
            let next_token = self.lexer.peek();
            match next_token {
                Some(Token::OpenParen) => {
                    let _ = self.lexer.next();
                    self.parse_function_call(name)
                }
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
                    // fail!("Token must be assignment[=], instead got {next_token:?}");
                    ast::Expression::ReferenceVariable(name)
                }
            }
        }
        _ => fail!("Token must be an expression, instead got {token:?}, lexer: \n {:?}", self.lexer.clone().collect::<Vec<_>>()),
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
            Token::GreaterThan => ast::BinaryOperator::Greater,
            Token::GreaterThanOrEqualTo => ast::BinaryOperator::GreaterEq,
            Token::LessThan => ast::BinaryOperator::Less,
            Token::LessThanOrEqualTo => ast::BinaryOperator::LessEq,
            Token::EqualTo => ast::BinaryOperator::Equal,
            Token::NotEqual => ast::BinaryOperator::NotEqual,
            Token::LogicalOr => ast::BinaryOperator::LogicalOr,
            Token::LogicalAnd => ast::BinaryOperator::LogicalAnd,
            _ => unreachable!(),
        }
    }
}
