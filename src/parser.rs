use crate::{ast, lexer::Lexer, token::Token};

pub struct Parser {
    lexer: Lexer,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Parser { lexer }
    }

    // does exactly what you think
    fn fail(message: String) -> ! {
        panic!("{message}");
    }

    // begins the parsing process from the input lexer
    pub fn parse(&mut self) -> ast::Prog {
        ast::Prog::Prog(self.parse_func())
    }

    // This parses a function such as ret_type ident(params...) {statements...}
    fn parse_func(&mut self) -> ast::FuncDecl {
        let token = self.lexer.next().expect("Missing return type");
        if let Token::KeywordInt = token {
        } else {
            Self::fail(format!("Needs int return type, got {token:?}"));
        }

        let token = self.lexer.next().expect("Missing function name");
        let indentifier = if let Token::Identifier(ident) = token {
            ident
        } else {
            Self::fail(format!("Needs int return type, got {token:?}"));
        };

        let token = self.lexer.next().expect("Missing opening parentheses");
        if let Token::OpenParen = token {
        } else {
            Self::fail(format!("Needs opening parentheses, got {token:?}"));
        }

        let token = self.lexer.next().expect("Missing closing parentheses");
        if let Token::CloseParen = token {
        } else {
            Self::fail(format!("Needs closing parentheses, got {token:?}"));
        }

        let token = self.lexer.next().expect("Missing opening brace");
        if let Token::OpenBrace = token {
        } else {
            Self::fail(format!("Needs opening brace, got {token:?}"));
        }

        let statement = self.parse_statement();

        let token = self.lexer.next().expect("Missing closing brace");
        if let Token::CloseBrace = token {
        } else {
            Self::fail(format!("Needs closing brace, got {token:?}"));
        }

        ast::FuncDecl::Func(indentifier, statement)
    }

    fn parse_statement(&mut self) -> ast::Statement {
        let token = self.lexer.next().expect("Invalid token sequence");
        if let Token::KeywordReturn = token {
            let exp = self.parse_expression();
            let statement = ast::Statement::Return(exp);
            let token = self.lexer.next().expect("Invalid token sequence");
            if let Token::Semicolon = token {
                statement
            } else {
                Self::fail(format!("Needs semicolon, got {token:?}"));
            }
        } else {
            Self::fail(format!("Needs return keyword, got {token:?}"));
        }
    }

    fn parse_expression(&mut self) -> ast::Exp {
        let token = self.lexer.next();
        match token {
            Some(Token::IntegerLiteral(int_literal)) => {
                ast::Exp::Integer(int_literal.parse().expect("This is guaranteed to be a valid integer because it can only be turned into a token if it is"))
            }
            _ => Self::fail(format!("Token must be an expression, instead got {token:?}")),
        }
    }

    // Generates the assembly from the abstract syntax tree
    pub fn generate(ast: ast::Prog) -> String {
        let mut asm_output = String::new();
        match ast {
            // parses the program header
            ast::Prog::Prog(program) => match program {
                ast::FuncDecl::Func(indentifier, statement) => {
                    asm_output = format!("{asm_output}.globl {indentifier}\n{indentifier}:\n");
                    match statement {
                        // parses the statements
                        ast::Statement::Return(expr) => match expr {
                            ast::Exp::Integer(int) => {
                                asm_output = format!("{asm_output}movl ${int}, %eax\nret");
                            }
                        }
                    }
                }
            }
        }
        asm_output
    }
}
