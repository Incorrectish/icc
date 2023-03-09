use crate::{
    assembly::{Asm, AsmInstr},
    ast,
    ast::{BinaryOperator, Expression},
    fail,
    symbol_table::SymbolTable,
};

pub const LONG_SIZE: u64 = 8;
#[allow(unused)]
pub const INT_SIZE: u64 = 4;

#[allow(dead_code)]
pub struct AsmGenerator {
    jump_counter: u64,
    scope_counter: u64,
    symbol_table: SymbolTable,
    // Going to need a hashmap like variable names: stack position
    // Suggestion, create a wrapper struct with #[repr(transparent)], so that you
    // can add a push method, which doesn't modify existing elements, unlike hashmap insert
    // and restricts access to the inside so it cannot be modified
}

impl AsmGenerator {
    pub fn new() -> Self {
        AsmGenerator {
            jump_counter: 0,
            scope_counter: 0,
            symbol_table: SymbolTable::new(),
        }
    }

    // Generates the assembly from the abstract syntax tree
    pub fn generate(&mut self, ast: ast::Prog) -> Asm {
        let parent_scope = self.scope_counter;
        let child_scope = self.create_scope(parent_scope);
        let mut asm = Asm::default();
        match ast {
            // parses the program header
            ast::Prog::Prog(function_declarations) => {
                for function_declaration in function_declarations {
                    asm.add_instructions(self.gen_function(function_declaration, child_scope));
                }
            }
        }
        self.remove_scope(child_scope);
        asm
    }

    fn gen_function(&mut self, function_declaration: ast::FuncDecl, parent_scope: u64) -> Asm {
        match function_declaration {
            ast::FuncDecl::Func(indentifier, arguments, statements) => {
                let mut assembly = Asm::default();
                let child_scope = self.create_scope(parent_scope);
                for statement in statements {
                    assembly.add_instructions(self.gen_block_item(statement, child_scope));
                }
                assembly.add_instructions(self.remove_scope(child_scope));
                // TODO: this might not work with conditionals, as the last statement is like
                // ".L{\w}"
                // let last = assembly.last();
                // if let Some(last) = assembly.
                match assembly.last() {
                    Some(last) if last.command() == "ret" => {}
                    _ => {
                        assembly.append_instruction("xorq".into(), "%rax,%rax".into());
                        assembly.append_instruction("popq".into(), "%rbp".into());
                        assembly.append_instruction("ret".into(), String::new());
                    }
                }
                // todo!()
                // TODO: branch analysis is bad on this
                // if assembly.last().command() != "ret" {
                //     assembly.append_instruction("xorq".into(), "%rax,%rax".into());
                //     assembly.append_instruction("ret".into(), String::new());
                // }
                Asm::from_instr(
                    vec![
                        AsmInstr::new("movq".to_string(), "%rsp,%rbp".to_string()),
                        AsmInstr::new("pushq".to_string(), "%rbp".to_string()),
                        AsmInstr::new(format!("{indentifier}:"), String::new()),
                        AsmInstr::new(".globl".to_string(), indentifier),
                    ],
                    assembly,
                )
            }
        }
    }

    fn gen_block_item(&mut self, block_item: ast::BlockItem, parent_scope: u64) -> Asm {
        match block_item {
            ast::BlockItem::Statement(statement) => self.gen_statement(statement, parent_scope),
            ast::BlockItem::Declaration(declaration) => {
                self.gen_declaration(declaration, parent_scope)
            }
        }
    }

    fn gen_declaration(&mut self, declaration: ast::Declaration, parent_scope: u64) -> Asm {
        match declaration {
            ast::Declaration::Declare(name, optional_expression, optional_child_declaration) => {
                let mut declaration_assembly = if let Some(expression) = optional_expression {
                    let location = self.symbol_table.allocate(name, parent_scope, LONG_SIZE);
                    let constructed_assembly = Asm::new(
                        self.gen_expression(expression, parent_scope),
                        vec![
                            // AsmInstr::from("popq", "%rax"),
                            AsmInstr::new("movq".into(), format!("%rax,{location}")),
                        ],
                    );
                    constructed_assembly
                } else {
                    // let location = symbol_table.allocate(name, LONG_SIZE);
                    // TODO: figure out default for declare without initialization
                    // TODO: There may not need to be any instructions because we can just let it
                    // have whatever memory is already in the adress, like C stadard
                    // let asm = Asm::instruction("movq".into(), format!("$0, {location}"));
                    //asm
                    let _location = self.symbol_table.allocate(name, parent_scope, LONG_SIZE);
                    Asm::default()
                };
                if let Some(child_declaration) = *optional_child_declaration {
                    declaration_assembly
                        .add_instructions(self.gen_declaration(child_declaration, parent_scope));
                }
                declaration_assembly
            }
        }
    }

    fn gen_do_loop(
        &mut self,
        expression: ast::Expression,
        statement: Box<ast::Statement>,
        parent_scope: u64,
    ) -> Asm {
        // .startofdo:
        // gen_stmt
        // gen_expr
        // cmpq $0,%rax
        // je .endofwhile
        // jmp .startofwhile
        // .endofwhile:
        // let expression = dbg!(expression);
        let (start_of_while_name, start_of_while_label) = self.gen_new_label();
        let (end_of_while_name, end_of_while_label) = self.gen_new_label();
        let mut asm = Asm::instruction(start_of_while_label, String::new());
        let stmt = self.gen_statement(*statement, parent_scope);
        stmt.print();
        asm.add_instructions(stmt);
        let exp = self.gen_expression(expression, parent_scope);
        asm.add_instructions(exp);
        asm.append_instruction("cmpq".to_string(), "$0,%rax".to_string());
        // asm.append_instruction("xorq".to_string(), "%rax,%rax".to_string());
        // asm.append_instruction("sete".to_string(), "%al".to_string());
        asm.append_instruction("je".to_string(), end_of_while_name);
        asm.append_instruction("jmp".to_string(), start_of_while_name);
        asm.append_instruction(end_of_while_label, String::new());
        asm
    }

    fn gen_while_loop(
        &mut self,
        expression: ast::Expression,
        statement: Box<ast::Statement>,
        parent_scope: u64,
    ) -> Asm {
        // .startofwhile:
        // gen_expr
        // cmpq $0,%rax
        // je .endofwhile
        // gen_stmt
        // jmp .startofwhile
        // .endofwhile:
        // let expression = dbg!(expression);
        let (start_of_while_name, start_of_while_label) = self.gen_new_label();
        let (end_of_while_name, end_of_while_label) = self.gen_new_label();
        let mut asm = Asm::instruction(start_of_while_label, String::new());
        let exp = self.gen_expression(expression, parent_scope);
        asm.add_instructions(exp);
        asm.append_instruction("cmpq".to_string(), "$0,%rax".to_string());
        // asm.append_instruction("xorq".to_string(), "%rax,%rax".to_string());
        // asm.append_instruction("sete".to_string(), "%al".to_string());
        asm.append_instruction("je".to_string(), end_of_while_name);
        let statement = dbg!(statement);
        let stmt = self.gen_statement(*statement, parent_scope);
        stmt.print();
        asm.add_instructions(stmt);
        asm.append_instruction("jmp".to_string(), start_of_while_name);
        asm.append_instruction(end_of_while_label, String::new());
        asm
    }

    fn gen_statement(&mut self, statement: ast::Statement, parent_scope: u64) -> Asm {
        match statement {
            ast::Statement::Continue => todo!(),
            ast::Statement::Break => todo!(),
            ast::Statement::While(expression, statement) => {
                self.gen_while_loop(expression, statement, parent_scope)
            }
            ast::Statement::Do(statement, expression) => {
                self.gen_do_loop(expression, statement, parent_scope)
            }
            ast::Statement::ForDecl(decl, exp1, exp2, body) => {
                let child_scope = self.create_scope(parent_scope);
                let mut asm = self.gen_declaration(decl, child_scope);
                let (begin_for_name, begin_for_label) = self.gen_new_label();
                asm.append_instruction(begin_for_label, String::new());
                asm.add_instructions(self.gen_for_loop(exp1, exp2, body, child_scope, begin_for_name));
                asm.add_instructions(self.remove_scope(child_scope));
                asm
            }
            ast::Statement::For(optional_exp, exp1, exp2, body) => {
                // same as for decl generally, except the very first part
                let (begin_for_name, begin_for_label) = self.gen_new_label();
                let mut asm: Asm = if let Some(exp) = optional_exp {
                    self.gen_expression(exp, parent_scope)
                } else {
                    Asm::default()
                };
                asm.append_instruction(begin_for_label, String::new());
                asm.add_instructions(self.gen_for_loop(exp1, exp2, body, parent_scope, begin_for_name));
                asm
            }
            ast::Statement::Return(expression) => {
                let constructed_assembly = Asm::new(
                    self.gen_expression(expression, parent_scope),
                    vec![
                        /* AsmInstr::from("popq", "%rbx"),  */
                        // AsmInstr::from("popq", "%rax"),
                        // AsmInstr::new("cdq".to_string(), String::new()),
                        AsmInstr::from("popq", "%rbp"),
                        AsmInstr::from("ret", ""),
                    ],
                );
                constructed_assembly
            }
            ast::Statement::Expression(expression) => {
                // let mut asm = self.gen_expression(expression);
                // asm.add_instructions(Asm::instruction("popq".into(), "%rbx".into()));
                // asm
                self.gen_expression(expression, parent_scope)
            }
            ast::Statement::Conditional(expression, if_children, potential_else_children) => {
                let mut asm = self.gen_expression(expression, parent_scope);
                let (else_, else_label) = self.gen_new_label();
                asm.add_instructions(Asm::instructions(vec![
                    // AsmInstr::from("popq", "%rax"),
                    AsmInstr::from("cmpq", "$0,%rax"),
                    AsmInstr::new("je".into(), else_.clone()),
                ]));
                // for statement in if_children {
                asm.add_instructions(self.gen_statement(*if_children, parent_scope));
                // }
                if let Some(else_children) = *potential_else_children {
                    // TODO: get labels, else, and endofif
                    let (endif_, endif_label) = self.gen_new_label();
                    asm.add_instructions(Asm::instructions(vec![
                        AsmInstr::new("jmp".into(), endif_),
                        AsmInstr::new(else_label, String::new()),
                    ]));
                    // for statement in else_children {
                    asm.add_instructions(self.gen_statement(else_children, parent_scope));
                    // }
                    asm.add_instructions(Asm::instruction(endif_label, String::new()))
                } else {
                    asm.add_instructions(Asm::instruction(else_label, String::new()))
                }
                asm
                // For if statements, it should be as easy as generating
                // if exp {statement} [else {statement}]
                // gen(exp)
                // value is at the top of the stack
                // je else
                // for(statements) : gen(statement)
                // jmp end of else
                // if else.is_some()
                //  else:
                //  for (statements ) : gen(statement)
                // end_of_if:
                // if else.is_none
                // end_of_if
            }
            ast::Statement::Block(block) => self.gen_block(block, parent_scope),
        }
    }

    fn remove_scope(&mut self, parent_scope: u64) -> Asm {
        let scope_size = self.symbol_table.get_scope_size(parent_scope);
        Asm::instruction("addq".to_string(), format!("${scope_size},%rsp"))
    }

    // creates a new scope that is a child of the parent scope and returns it
    fn create_scope(&mut self, parent_scope: u64) -> u64 {
        self.scope_counter += 1;
        self.symbol_table
            .create_scope(self.scope_counter, parent_scope);
        let child_scope = self.scope_counter;
        child_scope
    }

    fn gen_for_loop(
        &mut self,
        exp: Expression,
        optional_exp: Option<Expression>,
        body: Box<ast::Statement>,
        parent_scope: u64,
        begin_for_name: String,
    ) -> Asm {
        let mut asm = Asm::default();
        let (end_for_name, end_for_label) = self.gen_new_label();
        if !matches!(exp, Expression::NullExp) {
            asm.add_instructions(self.gen_expression(exp, parent_scope));
            asm.append_instruction("cmpq".to_string(), "$0, %rax".to_string());
            asm.append_instruction("je".to_string(), end_for_name);
        }
        if let Some(expr) = optional_exp {
            asm.add_instructions(self.gen_expression(expr, parent_scope));
        }
        asm.add_instructions(self.gen_statement(*body, parent_scope));
        asm.append_instruction("jmp".to_string(), begin_for_name);
        asm.append_instruction(end_for_label, String::new());
        asm
        /*
         * create_new_scope
         * gen_declaration
         * label_begin_for
         * create_new_scope
         * if !expression == nullexpression:
         *  gen_expression1
         * cmpq $0, %rax
         * je endoffor
         * gen_expression
         * if exp1.is_some()
         * gen exp2
         * gen_body
         * jmp beginfor
         * endoffor:
         * {
         *  declaration
         *  begin_for
         *  {
         *      if !statement 1 {
         *          goto end of for
         *      }
         *      statement2;
         *      body
         *      goto begin_for
         *  }
         * }
         * end of for:
         */
    }

    fn gen_block(&mut self, block: Vec<ast::BlockItem>, parent_scope: u64) -> Asm {
        let mut asm = Asm::default();
        let _ = self.create_scope(parent_scope);
        let child_scope = self.scope_counter;
        // let parent_scope = self.scope_counter;
        for block_item in block {
            asm.add_instructions(self.gen_block_item(block_item, child_scope));
        }
        asm.add_instructions(self.remove_scope(child_scope));
        asm
    }

    fn gen_expression(&mut self, expr: ast::Expression, parent_scope: u64) -> Asm {
        match expr {
            ast::Expression::NullExp => Asm::default(),
            ast::Expression::Constant(int) => {
                Asm::instruction("movq".to_string(), format!("${int},%rax"))
            }
            ast::Expression::UnaryOp(operator, expression) => match operator {
                _ => {
                    let mut asm = self.gen_expression(*expression, parent_scope);
                    asm.add_instructions(self.operation(operator, parent_scope));
                    asm
                }
            },
            ast::Expression::BinaryOp(binary_operator, left_expr, right_expr) => self
                .gen_binary_operation_expression(
                    binary_operator,
                    left_expr,
                    right_expr,
                    parent_scope,
                ),
            ast::Expression::Assign(name, expression) => {
                let asm = self.gen_expression(*expression, parent_scope);
                let optional_location = self.symbol_table.get(&name, parent_scope);
                if let Some(location) = optional_location {
                    Asm::new(
                        asm,
                        vec![
                            // AsmInstr::from("popq", "%rax"),
                            AsmInstr::new("movq".into(), format!("%rax,{location}")),
                            // AsmInstr::from("pushq", "%rax"),
                        ],
                    )
                } else {
                    fail!(
                        "exp(assign)\nVariable \"{}\" in scope '{} referenced but not declared",
                        name,
                        parent_scope
                    );
                }
            }
            ast::Expression::ReferenceVariable(name) => {
                let optional_location = self.symbol_table.get(&name, parent_scope);
                if let Some(location) = optional_location {
                    // Asm::instruction("pushq".into(), location.clone())
                    Asm::instruction("movq".into(), format!("{location},%rax"))
                } else {
                    fail!(
                        "Variable \"{}\" in scope '{} referenced but not declared",
                        name,
                        parent_scope
                    );
                }
            }
            ast::Expression::Ternary(expression1, expression2, expression3) => {
                let (else_name, else_label) = self.gen_new_label();
                let (endternary_name, endternary_label) = self.gen_new_label();
                let mut asm = self.gen_expression(*expression1, parent_scope);
                asm.append_instruction("cmpq".into(), "$0,%rax".into());
                asm.append_instruction("je".into(), else_name);
                asm.add_instructions(self.gen_expression(*expression2, parent_scope));
                asm.append_instruction("jmp".into(), endternary_name);
                asm.append_instruction(else_label, String::new());
                asm.add_instructions(self.gen_expression(*expression3, parent_scope));
                asm.append_instruction(endternary_label, String::new());
                asm
            }
        }
    }

    fn gen_binary_operation_expression(
        &mut self,
        binary_operator: BinaryOperator,
        left_expr: Box<Expression>,
        right_expr: Box<Expression>,
        parent_scope: u64,
    ) -> Asm {
        let mut left_exp = self.gen_expression(*left_expr, parent_scope);
        // Left expression is in %rbx, right will be in %rax
        left_exp.append_instruction("movq".into(), "%rax,%rbx".into());
        let right_exp = self.gen_expression(*right_expr, parent_scope);
        left_exp.add_instructions(right_exp);
        // Moves left expression to %rax, and right to %rbx
        left_exp.append_instruction("xchg".into(), "%rbx,%rax".into());
        let binop = if !matches!(
            binary_operator,
            BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr
        ) {
            Self::binary_operation(&binary_operator)
        } else {
            String::new()
        };

        match binary_operator {
            BinaryOperator::Divide => {
                // we want to divide left by right expression, so we must exchange the contents of rbx
                // and rax
                // left_exp.append_instruction("popq".into(), "%rbx".into());
                // left_exp.append_instruction("popq".into(), "%rax".into());
                left_exp.append_instruction("cqo".into(), String::new());
                left_exp.append_instruction(binop, "%rbx".into());
                // left_exp.append_instruction("pushq".into(), "%rax".into());
            }
            BinaryOperator::Multiply => {
                // TODO: result is actually not just in rax, but in two registers potentially so
                // see that that is fixed
                // left_exp.append_instruction("popq".into(), "%rbx".into());
                // left_exp.append_instruction("popq".into(), "%rax".into());
                left_exp.append_instruction(binop, "%rbx,%rax".into());
                // left_exp.append_instruction("pushq".into(), "%rax".into());
            }
            BinaryOperator::Modulo => {
                todo!("This does not work yet!! Gives junk such as 1 % 5 = 4!");
                // left_exp.append_instruction("popq".into(), "%rbx".into());
                // left_exp.append_instruction("popq".into(), "%rax".into());
                left_exp.append_instruction("cqo".into(), String::new());
                left_exp.append_instruction(binop, "%rbx".into());
                left_exp.append_instruction("movq".into(), "%rbx,%rax".into());
                // left_exp.append_instruction("pushq".into(), "%rdx".into());
            }
            BinaryOperator::BitwiseLeftShift | BinaryOperator::BitwiseRightShift => {
                // TODO: maybe clear the rcx register?
                // left_exp.append_instruction("popq".into(), "%rcx".into());
                left_exp.append_instruction(binop, "%rbx,%rax".into());
            }
            // The following might need to be fixed
            BinaryOperator::LessEq | BinaryOperator::Less => {
                // left_exp.append_instruction("popq".into(), "%rbx".into());
                // left_exp.append_instruction("popq".into(), "%rax".into());
                left_exp.append_instruction("cmpq".into(), "%rbx,%rax".into());
                left_exp.append_instruction("movq".into(), "$0,%rax".into());
                left_exp.append_instruction(binop, "%al".into());
                // left_exp.append_instruction("pushq".into(), "%rax".into());
            }
            BinaryOperator::Equal
            | BinaryOperator::NotEqual
            | BinaryOperator::GreaterEq
            | BinaryOperator::Greater => {
                // left_exp.append_instruction("popq".into(), "%rbx".into());
                // left_exp.append_instruction("popq".into(), "%rax".into());
                left_exp.append_instruction("cmpq".into(), "%rax,%rbx".into());
                left_exp.append_instruction("movq".into(), "$0,%rax".into());
                left_exp.append_instruction(binop, "%al".into());
                // left_exp.append_instruction("pushq".into(), "%rax".into());
            }
            BinaryOperator::LogicalOr => {
                // left_exp.append_instruction("popq".into(), "%rbx".into());
                left_exp.append_instruction("cmpq".into(), "$0,%rax".into());
                let (label1_name, label1) = self.gen_new_label();
                let (label2_name, label2) = self.gen_new_label();
                let (label3_name, label3) = self.gen_new_label();
                left_exp.append_instruction("jne".into(), label1_name);
                left_exp.append_instruction("cmpq".into(), "$0,%rbx".into());
                left_exp.append_instruction("je".into(), label2_name);
                left_exp.append_instruction(label1, "".into());
                left_exp.append_instruction("movq".into(), "$1,%rax".into());
                left_exp.append_instruction("jmp".into(), label3_name);
                left_exp.append_instruction(label2, "".into());
                left_exp.append_instruction("xorq".into(), "%rax,%rax".into());
                left_exp.append_instruction(label3, "".into());
            }
            BinaryOperator::LogicalAnd => {
                // left_exp.append_instruction("popq".into(), "%rbx".into());
                left_exp.append_instruction("cmpq".into(), "$0,%rax".into());
                let (label1_name, label1) = self.gen_new_label();
                let (label2_name, label2) = self.gen_new_label();
                left_exp.append_instruction("je".into(), label1_name.clone());
                left_exp.append_instruction("cmpq".into(), "$0,%rbx".into());
                left_exp.append_instruction("je".into(), label1_name);
                left_exp.append_instruction("movq".into(), "$1,%rax".into());
                left_exp.append_instruction("jmp".into(), label2_name);
                left_exp.append_instruction(label1, "".into());
                left_exp.append_instruction("xorq".into(), "%rax,%rax".into());
                left_exp.append_instruction(label2, "".into());
            }
            _ => {
                // Note the following might be a bit of a premature optimization. I assume that the
                // size of the data is 4 bytes, so I can read the first two values off the stack
                // into a register, and then operate on them and write them back to the stack,
                // minimizing push/pop instructions
                // left_exp.append_instruction("popq".into(), "%rax".into());
                left_exp.append_instruction(binop, "%rbx,%rax".into());
                // format!(
                //     "{left_exp}\n{right_exp}\npopl %eax\npopl %ebx\n{binop} %ebx, %eax\npushl %eax"
                // )
            }
        }
        // if matches!(binary_operator, BinaryOperator::Divide) {
        // } else if matches!(binary_operator, BinaryOperator::Multiply) {
        // } else if matches!(binary_operator, BinaryOperator::Modulo) {
        // } else if matches!(
        //     binary_operator,
        //     BinaryOperator::BitwiseLeftShift | BinaryOperator::BitwiseRightShift
        // ) {
        // } else if matches!(
        //     binary_operator,
        //     BinaryOperator::Equal
        //         | BinaryOperator::NotEqual
        //         | BinaryOperator::LessEq
        //         | BinaryOperator::GreaterEq
        //         | BinaryOperator::Less
        //         | BinaryOperator::Greater
        // ) {
        // } else if matches!(binary_operator, BinaryOperator::LogicalOr) {
        // } else if matches!(binary_operator, BinaryOperator::LogicalAnd) {
        // } else {
        // }
        left_exp
    }

    fn operation(&self, operator: ast::UnaryOperator, parent_scope: u64) -> Asm {
        // Note the following might be a bit of a premature optimization. For all the cases,
        // instead of pushing/popping the values off the stack into the ecx register, I instead
        // copy them into a register then operate on them, then write to a register, minimizing
        // costly memory operations
        // Note for all of these, rcx might be overwritten so in the future, might need to copy out the
        // values in rcx
        match operator {
            ast::UnaryOperator::Negation => Asm::instructions(vec![AsmInstr::from("negq", "%rax")]),
            ast::UnaryOperator::BitwiseComplement => {
                Asm::instructions(vec![AsmInstr::from("notq", "%rax")])
            }
            ast::UnaryOperator::LogicalNegation => Asm::instructions(vec![
                AsmInstr::from("cmpq", "$0,%rax"),
                AsmInstr::from("xor", "%rax,%rax"),
                AsmInstr::from("sete", "%al"),
            ]),
            ast::UnaryOperator::PostfixIncrement(name) => {
                let location = self.symbol_table.get(&name, parent_scope).expect(&format!(
                    "Variable {} in scope '{} accessed but not declared",
                    name, parent_scope
                ));
                Asm::instructions(vec![
                    AsmInstr::new("movq".into(), format!("{location},%rax")),
                    AsmInstr::new("addq".into(), format!("$1,{location}")),
                ])
            }
            ast::UnaryOperator::PrefixIncrement(name) => {
                let location = self.symbol_table.get(&name, parent_scope).expect(&format!(
                    "Variable {} in scope '{} accessed but not declared",
                    name, parent_scope
                ));
                Asm::instructions(vec![
                    AsmInstr::new("addq".into(), format!("$1,{location}")),
                    AsmInstr::new("movq".into(), format!("{location},%rax")),
                ])
            }
            ast::UnaryOperator::PrefixDecrement(name) => {
                let location = self.symbol_table.get(&name, parent_scope).expect(&format!(
                    "Variable {} in scope '{} accessed but not declared",
                    name, parent_scope
                ));
                Asm::instructions(vec![
                    AsmInstr::new("subq".into(), format!("$1,{location}")),
                    AsmInstr::new("movq".into(), format!("{location},%rax")),
                ])
            }
            ast::UnaryOperator::PostfixDecrement(name) => {
                let location = self.symbol_table.get(&name, parent_scope).expect(&format!(
                    "Variable {} in scope '{} accessed but not declared",
                    name, parent_scope
                ));
                Asm::instructions(vec![
                    AsmInstr::new("movq".into(), format!("{location},%rax")),
                    AsmInstr::new("subq".into(), format!("$1,{location}")),
                ])
            }
            _ => unreachable!(),
            // format!("cmpl $0, {location}\nsete {location}"),
        }
    }

    fn binary_operation(operator: &ast::BinaryOperator) -> String {
        match operator {
            ast::BinaryOperator::Add => "addq".to_string(),
            ast::BinaryOperator::Minus => "subq".to_string(),
            ast::BinaryOperator::Multiply => "imulq".to_string(),
            ast::BinaryOperator::Divide => "idivq".to_string(),
            // TODO MODULO DOES NOT WORK
            ast::BinaryOperator::Modulo => "idivq".to_string(),
            ast::BinaryOperator::Xor => "xorq".to_string(),
            ast::BinaryOperator::BitwiseAnd => "andq".to_string(),
            ast::BinaryOperator::BitwiseOr => "orq".to_string(),
            ast::BinaryOperator::BitwiseRightShift => "sarq".to_string(),
            ast::BinaryOperator::BitwiseLeftShift => "salq".to_string(),
            ast::BinaryOperator::GreaterEq => "setge".to_string(),
            ast::BinaryOperator::Greater => "setg".to_string(),
            ast::BinaryOperator::LessEq => "setle".to_string(),
            ast::BinaryOperator::Less => "setl".to_string(),
            ast::BinaryOperator::NotEqual => "setne".to_string(),
            ast::BinaryOperator::Equal => "sete".to_string(),
            ast::BinaryOperator::LogicalAnd => {
                unreachable!("You should not use this function on a logical and input")
            }
            ast::BinaryOperator::LogicalOr => {
                unreachable!("You should not use this function on a logical or input")
            }
        }
    }

    // Returns (name, label), for example it could return (".L1", ".L1:") 
    fn gen_new_label(&mut self) -> (String, String) {
        let label = format!(".L{}", self.jump_counter);
        let name = format!("{label}:");
        self.jump_counter += 1;
        (label, name)
    }
}
