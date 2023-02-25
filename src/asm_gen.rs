use crate::{
    assembly::{Asm, AsmInstr},
    ast,
    ast::{BinaryOperator, Expression},
    symbol_table::SymbolTable,
    fail,
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
        self.scope_counter += 1;
        // let parent_scope = self.scope_counter;
        let parent_scope = vec![self.scope_counter];
        match ast {
            // parses the program header
            ast::Prog::Prog(function_declaration) => self.gen_function(function_declaration, parent_scope),
        }
    }

    fn gen_function(&mut self, function_declaration: ast::FuncDecl, parent_scope: Vec<u64>) -> Asm {
        match function_declaration {
            ast::FuncDecl::Func(indentifier, statements) => {
                let mut assembly = Asm::default();
                for statement in statements {
                    assembly.add_instructions(self.gen_block_item(statement, parent_scope.clone()));
                }
                // TODO: this might not work with conditionals, as the last statement is like
                // ".L{\w}"
                if assembly.last().command() != "ret" {
                    assembly.append_instruction("xorq".into(), "%rax,%rax".into());
                    assembly.append_instruction("ret".into(), String::new());
                }
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

    fn gen_block_item(&mut self, block_item: ast::BlockItem, parent_scope: Vec<u64>) -> Asm {
        match block_item {
            ast::BlockItem::Statement(statement) => self.gen_statement(statement, parent_scope),
            ast::BlockItem::Declaration(declaration) => self.gen_declaration(declaration, parent_scope)
        }
    }

    fn gen_declaration(&mut self, declaration: ast::Declaration, parent_scope: Vec<u64>) -> Asm {
        match declaration {
            ast::Declaration::Declare(name, optional_expression, optional_child_declaration) => {
                let mut decleration_assembly = if let Some(expression) = optional_expression {
                    let location = self.symbol_table.allocate(name, &parent_scope, LONG_SIZE);
                    let constructed_assembly = Asm::new(
                        self.gen_expression(expression, parent_scope.clone()),
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
                    Asm::default()
                };
                if let Some(child_declaration) = *optional_child_declaration {
                    decleration_assembly.add_instructions(self.gen_declaration(child_declaration, parent_scope));
                }
                decleration_assembly
            }
        }
    }

    fn gen_statement(&mut self, statement: ast::Statement, parent_scope: Vec<u64>) -> Asm {
        match statement {
            ast::Statement::ForDecl(_, _, _, _) => todo!(),
            ast::Statement::Continue => todo!(),
            ast::Statement::Break => todo!(),
            ast::Statement::While(_, _) => todo!(),
            ast::Statement::Do(_, _) => todo!(),
            ast::Statement::For(_, _, _, _) => todo!(),
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
                let mut asm = self.gen_expression(expression, parent_scope.clone());
                let else_ = self.gen_new_label();
                let else_label = format!("{else_}:");
                asm.add_instructions(Asm::instructions(vec![
                    // AsmInstr::from("popq", "%rax"),
                    AsmInstr::from("cmpq", "$0,%rax"),
                    AsmInstr::new("je".into(), else_.clone()),
                ]));
                // for statement in if_children {
                asm.add_instructions(self.gen_statement(*if_children, parent_scope.clone()));
                // }
                if let Some(else_children) = *potential_else_children {
                    // TODO: get labels, else, and endofif
                    let endif_ = self.gen_new_label();
                    let endif_label = format!("{endif_}:");
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
            ast::Statement::Block(block) => {
                self.gen_block(block, parent_scope.clone())
            },
        }
    }

    fn gen_block(&mut self, block: Vec<ast::BlockItem>, mut parent_scope: Vec<u64>) -> Asm {
        let mut asm = Asm::default();
        self.scope_counter += 1;
        parent_scope.push(self.scope_counter);
        // let parent_scope = self.scope_counter;
        for block_item in block {
            asm.add_instructions(self.gen_block_item(block_item, parent_scope.clone()));
        }
        asm
    }

    fn gen_expression(&mut self, expr: ast::Expression, parent_scope: Vec<u64>) -> Asm {
        match expr {
            ast::Expression::NullExp => todo!(),
            ast::Expression::Constant(int) => {
                Asm::instruction("movq".to_string(), format!("${int},%rax"))
            }
            ast::Expression::UnaryOp(operator, expression) => match operator {
                _ => {
                    let mut asm = self.gen_expression(*expression, parent_scope.clone());
                    asm.add_instructions(self.operation(operator, parent_scope.clone()));
                    asm
                }
            },
            ast::Expression::BinaryOp(binary_operator, left_expr, right_expr) => {
                self.gen_binary_operation_expression(binary_operator, left_expr, right_expr, parent_scope)
            }
            ast::Expression::Assign(name, expression) => {
                let asm = self.gen_expression(*expression, parent_scope.clone());
                let optional_location = self.symbol_table.get(&name, &parent_scope);
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
                        "exp(assign)\nVariable \"{}\" in scope '{} referenced but not declared", name, *parent_scope.last().expect("Vector should never be empty")
                    );
                }
            }
            ast::Expression::ReferenceVariable(name) => {
                let optional_location = self.symbol_table.get(&name, &parent_scope);
                if let Some(location) = optional_location {
                    // Asm::instruction("pushq".into(), location.clone())
                    Asm::instruction("movq".into(), format!("{location},%rax"))
                } else {
                    fail!("Variable \"{}\" in scope '{} referenced but not declared", name, *parent_scope.last().expect("Vector should never be empty"));
                }
            }
            ast::Expression::Ternary(expression1, expression2, expression3) => {
                let else_name = self.gen_new_label();
                let else_label = format!("{else_name}:");
                let endternary_name = self.gen_new_label();
                let endternary_label = format!("{endternary_name}:");
                let mut asm = self.gen_expression(*expression1, parent_scope.clone());
                asm.append_instruction("cmpq".into(), "$0,%rax".into());
                asm.append_instruction("je".into(), else_name);
                asm.add_instructions(self.gen_expression(*expression2, parent_scope.clone()));
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
        parent_scope: Vec<u64>
    ) -> Asm {
        let mut left_exp = self.gen_expression(*left_expr, parent_scope.clone());
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
        if matches!(binary_operator, BinaryOperator::Divide) {
            // we want to divide left by right expression, so we must exchange the contents of rbx
            // and rax
            // left_exp.append_instruction("popq".into(), "%rbx".into());
            // left_exp.append_instruction("popq".into(), "%rax".into());
            left_exp.append_instruction("cqo".into(), String::new());
            left_exp.append_instruction(binop, "%rbx".into());
            // left_exp.append_instruction("pushq".into(), "%rax".into());
        } else if matches!(binary_operator, BinaryOperator::Multiply) {
            // TODO: result is actually not just in rax, but in two registers potentially so
            // see that that is fixed
            // left_exp.append_instruction("popq".into(), "%rbx".into());
            // left_exp.append_instruction("popq".into(), "%rax".into());
            left_exp.append_instruction(binop, "%rbx,%rax".into());
            // left_exp.append_instruction("pushq".into(), "%rax".into());
        } else if matches!(binary_operator, BinaryOperator::Modulo) {
            todo!("This does not work yet!! Gives junk such as 1 % 5 = 4!");
            // left_exp.append_instruction("popq".into(), "%rbx".into());
            // left_exp.append_instruction("popq".into(), "%rax".into());
            left_exp.append_instruction("cqo".into(), String::new());
            left_exp.append_instruction(binop, "%rbx".into());
            left_exp.append_instruction("movq".into(), "%rbx,%rax".into());
            // left_exp.append_instruction("pushq".into(), "%rdx".into());
        } else if matches!(
            binary_operator,
            BinaryOperator::BitwiseLeftShift | BinaryOperator::BitwiseRightShift
        ) {
            // TODO: maybe clear the rcx register?
            // left_exp.append_instruction("popq".into(), "%rcx".into());
            left_exp.append_instruction(binop, "%rbx,%rax".into());
        } else if matches!(
            binary_operator,
            BinaryOperator::Equal
                | BinaryOperator::NotEqual
                | BinaryOperator::LessEq
                | BinaryOperator::GreaterEq
                | BinaryOperator::Less
                | BinaryOperator::Greater
        ) {
            // left_exp.append_instruction("popq".into(), "%rbx".into());
            // left_exp.append_instruction("popq".into(), "%rax".into());
            left_exp.append_instruction("cmpq".into(), "%rax,%rbx".into());
            left_exp.append_instruction("xorq".into(), "%rax,%rax".into());
            left_exp.append_instruction(binop, "%al".into());
            // left_exp.append_instruction("pushq".into(), "%rax".into());
        } else if matches!(binary_operator, BinaryOperator::LogicalOr) {
            // left_exp.append_instruction("popq".into(), "%rbx".into());
            left_exp.append_instruction("cmpq".into(), "$0,%rax".into());
            let label1_name = self.gen_new_label();
            let label1 = format!("{label1_name}:");
            let label2_name = self.gen_new_label();
            let label2 = format!("{label2_name}:");
            let label3_name = self.gen_new_label();
            let label3 = format!("{label3_name}:");
            left_exp.append_instruction("jne".into(), label1_name);
            left_exp.append_instruction("cmpq".into(), "$0,%rbx".into());
            left_exp.append_instruction("je".into(), label2_name);
            left_exp.append_instruction(label1, "".into());
            left_exp.append_instruction("movq".into(), "$1,%rax".into());
            left_exp.append_instruction("jmp".into(), label3_name);
            left_exp.append_instruction(label2, "".into());
            left_exp.append_instruction("xorq".into(), "%rax,%rax".into());
            left_exp.append_instruction(label3, "".into());
        } else if matches!(binary_operator, BinaryOperator::LogicalAnd) {
            // left_exp.append_instruction("popq".into(), "%rbx".into());
            left_exp.append_instruction("cmpq".into(), "$0,%rax".into());
            let label1_name = self.gen_new_label();
            let label1 = format!("{label1_name}:");
            let label2_name = self.gen_new_label();
            let label2 = format!("{label2_name}:");
            left_exp.append_instruction("je".into(), label1_name.clone());
            left_exp.append_instruction("cmpq".into(), "$0,%rbx".into());
            left_exp.append_instruction("je".into(), label1_name);
            left_exp.append_instruction("movq".into(), "$1,%rax".into());
            left_exp.append_instruction("jmp".into(), label2_name);
            left_exp.append_instruction(label1, "".into());
            left_exp.append_instruction("xorq".into(), "%rax,%rax".into());
            left_exp.append_instruction(label2, "".into());
        } else {
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
        left_exp
    }

    fn operation(&self, operator: ast::UnaryOperator, parent_scope: Vec<u64>) -> Asm {
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
                let location = self
                    .symbol_table
                    .get(&name, &parent_scope)
                    .expect(&format!("Variable {} in scope '{} accessed but not declared", name, *parent_scope.last().expect("Vector should never be empty")));
                Asm::instructions(vec![
                    AsmInstr::new("movq".into(), format!("{location},%rax")),
                    AsmInstr::new("addq".into(), format!("$1,{location}")),
                ])
            }
            ast::UnaryOperator::PrefixIncrement(name) => {
                let location = self
                    .symbol_table
                    .get(&name, &parent_scope)
                    .expect(&format!("Variable {} in scope '{} accessed but not declared", name, *parent_scope.last().expect("Vector should never be empty")));
                Asm::instructions(vec![
                    AsmInstr::new("addq".into(), format!("$1,{location}")),
                    AsmInstr::new("movq".into(), format!("{location},%rax")),
                ])
            }
            ast::UnaryOperator::PrefixDecrement(name) => {
                let location = self
                    .symbol_table
                    .get(&name, &parent_scope)
                    .expect(&format!("Variable {} in scope '{} accessed but not declared", name, *parent_scope.last().expect("Vector should never be empty")));
                Asm::instructions(vec![
                    AsmInstr::new("subq".into(), format!("$1,{location}")),
                    AsmInstr::new("movq".into(), format!("{location},%rax")),
                ])
            }
            ast::UnaryOperator::PostfixDecrement(name) => {
                let location = self
                    .symbol_table
                    .get(&name, &parent_scope)
                    .expect(&format!("Variable {} in scope '{} accessed but not declared", name, *parent_scope.last().expect("Vector should never be empty")));
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

    fn gen_new_label(&mut self) -> String {
        let label = format!(".L{}", self.jump_counter);
        self.jump_counter += 1;
        label
    }
}
