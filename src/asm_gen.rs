use crate::{
    assembly::{Asm, AsmInstr},
    ast,
    ast::{BinaryOperator, Expression},
    parser::{fail, Parser},
    symbol_table::SymbolTable,
};

pub const LONG_SIZE: u64 = 8;
pub const INT_SIZE: u64 = 4;

#[allow(dead_code)]
pub struct AsmGenerator {
    jump_counter: u64,
    // Going to need a hashmap like variable names: stack position
    // Suggestion, create a wrapper struct with #[repr(transparent)], so that you
    // can add a push method, which doesn't modify existing elements, unlike hashmap insert
    // and restricts access to the inside so it cannot be modified
    symbol_table: SymbolTable,
    newest_mem_location: u64,
}

impl AsmGenerator {
    pub fn new() -> Self {
        AsmGenerator {
            jump_counter: 0,
            symbol_table: SymbolTable::new(),
            newest_mem_location: 0,
        }
    }

    // Generates the assembly from the abstract syntax tree
    pub fn generate(&mut self, ast: ast::Prog) -> Asm {
        match ast {
            // parses the program header
            ast::Prog::Prog(function_decleration) => self.gen_function(function_decleration),
        }
    }

    fn gen_function(&mut self, function_declaration: ast::FuncDecl) -> Asm {
        match function_declaration {
            ast::FuncDecl::Func(indentifier, statements) => {
                let mut assembly = Asm::default();
                for statement in statements {
                    assembly.add_instructions(self.gen_statement(statement));
                }
                if assembly.last().command() != "ret" {
                    assembly.append_instruction("movq".into(), "$0,%eax".into());
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

    fn gen_statement(&mut self, statement: ast::Statement) -> Asm {
        match statement {
            // parses the statements
            ast::Statement::Return(expression) => {
                let constructed_assembly = Asm::new(
                    self.gen_expression(expression),
                    vec![
                        /* AsmInstr::from("popq", "%rbx"),  */
                        AsmInstr::from("popq", "%rax"),
                        AsmInstr::new("cdq".to_string(), String::new()),
                        AsmInstr::from("popq", "%rbp"),
                        AsmInstr::from("ret", ""),
                    ],
                );
                constructed_assembly
            }
            ast::Statement::Declare(name, optional_expression) => {
                if let Some(expression) = optional_expression {
                    let location = self.gen_location(LONG_SIZE);
                    let constructed_assembly = Asm::new(
                        self.gen_expression(expression),
                        vec![
                            AsmInstr::from("popq", "%rax"),
                            AsmInstr::from("movq", &format!("%rax,{location}")),
                        ],
                    );
                    self.symbol_table.add(name, location);
                    constructed_assembly
                } else {
                    let location = self.gen_location(LONG_SIZE);
                    // TODO: figure out default for declare without initialization
                    let asm = Asm::instruction("movq".into(), format!("$0, {location}"));
                    self.symbol_table.add(name, location);
                    asm
                }
            }
            ast::Statement::Expression(expression) => {
                let mut asm = self.gen_expression(expression);
                asm.add_instructions(Asm::instruction("popq".into(), "%rbx".into()));
                asm
            }
        }
    }

    fn gen_expression(&mut self, expr: ast::Expression) -> Asm {
        match expr {
            ast::Expression::Constant(int) => {
                Asm::instruction("pushq".to_string(), format!("${int}"))
            }
            ast::Expression::UnaryOp(operator, expression) => {
                let mut asm = self.gen_expression(*expression);
                asm.add_instructions(Self::operation(operator));
                asm
            }
            ast::Expression::BinaryOp(binary_operator, left_expr, right_expr) => {
                self.gen_binary_operation_expression(binary_operator, left_expr, right_expr)
            }
            ast::Expression::Assign(name, expression) => {
                let asm = self.gen_expression(*expression);
                let optional_location = self.symbol_table.get(&name);
                if let Some(location) = optional_location {
                    Asm::new(
                        asm,
                        vec![
                            AsmInstr::from("popq", "%rax"),
                            AsmInstr::new("movq".into(), format!("%rax,{location}")),
                            AsmInstr::from("pushq", "%rax"),
                        ],
                    )
                } else {
                    fail(format!(
                        "exp(assign)\nVariable \"{name}\" referenced but not declared"
                    ));
                }
            }
            ast::Expression::ReferenceVariable(name) => {
                let optional_location = self.symbol_table.get(&name);
                if let Some(location) = optional_location {
                    Asm::instruction("pushq".into(), location.clone())
                } else {
                    fail(format!("Variable \"{name}\" referenced but not declared"));
                }
            }
        }
    }

    fn gen_binary_operation_expression(
        &mut self,
        binary_operator: BinaryOperator,
        left_expr: Box<Expression>,
        right_expr: Box<Expression>,
    ) -> Asm {
        let mut left_exp = self.gen_expression(*left_expr);
        let right_exp = self.gen_expression(*right_expr);
        left_exp.add_instructions(right_exp);
        let binop = if !matches!(
            binary_operator,
            BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr
        ) {
            Self::binary_operation(&binary_operator)
        } else {
            String::new()
        };
        if matches!(binary_operator, BinaryOperator::Divide) {
            left_exp.append_instruction("popq".into(), "%rbx".into());
            left_exp.append_instruction("popq".into(), "%rax".into());
            left_exp.append_instruction("cqo".into(), String::new());
            left_exp.append_instruction(binop, "%rbx".into());
            left_exp.append_instruction("pushq".into(), "%rax".into());
        } else if matches!(binary_operator, BinaryOperator::Multiply) {
            // TODO: result is actually not just in rax, but in two registers potentially so
            // see that that is fixed
            left_exp.append_instruction("popq".into(), "%rbx".into());
            left_exp.append_instruction("popq".into(), "%rax".into());
            left_exp.append_instruction(binop, "%rbx,%rax".into());
            left_exp.append_instruction("pushq".into(), "%rax".into());
        } else if matches!(binary_operator, BinaryOperator::Modulo) {
            left_exp.append_instruction("popq".into(), "%rbx".into());
            left_exp.append_instruction("popq".into(), "%rax".into());
            left_exp.append_instruction("cqo".into(), String::new());
            left_exp.append_instruction(binop, "%rbx".into());
            left_exp.append_instruction("pushq".into(), "%rdx".into());
        } else if matches!(
            binary_operator,
            BinaryOperator::BitwiseLeftShift | BinaryOperator::BitwiseRightShift
        ) {
            // TODO: maybe clear the rcx register?
            left_exp.append_instruction("popq".into(), "%rcx".into());
            left_exp.append_instruction(binop, "%rcx,(%rsp)".into());
        } else if matches!(
            binary_operator,
            BinaryOperator::Equal
                | BinaryOperator::NotEqual
                | BinaryOperator::LessEq
                | BinaryOperator::GreaterEq
                | BinaryOperator::Less
                | BinaryOperator::Greater
        ) {
            left_exp.append_instruction("popq".into(), "%rbx".into());
            left_exp.append_instruction("popq".into(), "%rax".into());
            left_exp.append_instruction("cmpq".into(), "%rax,%rbx".into());
            left_exp.append_instruction(binop, "%rax".into());
            left_exp.append_instruction("pushq".into(), "%rax".into());
        } else if matches!(binary_operator, BinaryOperator::LogicalOr) {
            left_exp.append_instruction("popq".into(), "%rbx".into());
            left_exp.append_instruction("cmpq".into(), "$0,(%rsp)".into());
            let label1_name = self.gen_new_label("logicaljump");
            let label1 = format!("{label1_name}:");
            let label2_name = self.gen_new_label("logicaljump");
            let label2 = format!("{label2_name}:");
            let label3_name = self.gen_new_label("logicaljump");
            let label3 = format!("{label3_name}:");
            left_exp.append_instruction("jne".into(), label1_name);
            left_exp.append_instruction("cmpq".into(), "$0,%rbx".into());
            left_exp.append_instruction("je".into(), label2_name);
            left_exp.append_instruction(label1, "".into());
            left_exp.append_instruction("movq".into(), "$1,(%rsp)".into());
            left_exp.append_instruction("jmp".into(), label3_name);
            left_exp.append_instruction(label2, "".into());
            left_exp.append_instruction("movq".into(), "$0,(%rsp)".into());
            left_exp.append_instruction(label3, "".into());
        } else if matches!(binary_operator, BinaryOperator::LogicalAnd) {
            left_exp.append_instruction("popq".into(), "%rbx".into());
            left_exp.append_instruction("cmpq".into(), "$0,(%rsp)".into());
            let label1_name = self.gen_new_label("logicaljump");
            let label1 = format!("{label1_name}:");
            let label2_name = self.gen_new_label("logicaljump");
            let label2 = format!("{label2_name}:");
            left_exp.append_instruction("je".into(), label1_name.clone());
            left_exp.append_instruction("cmpq".into(), "$0,%rbx".into());
            left_exp.append_instruction("je".into(), label1_name);
            left_exp.append_instruction("movq".into(), "$1,(%rsp)".into());
            left_exp.append_instruction("jmp".into(), label2_name);
            left_exp.append_instruction(label1, "".into());
            left_exp.append_instruction("movq".into(), "$0,(%rsp)".into());
            left_exp.append_instruction(label2, "".into());
        } else {
            // Note the following might be a bit of a premature optimization. I assume that the
            // size of the data is 4 bytes, so I can read the first two values off the stack
            // into a register, and then operate on them and write them back to the stack,
            // minimizing push/pop instructions
            left_exp.append_instruction("popq".into(), "%rax".into());
            left_exp.append_instruction(binop, "%rax,(%rsp)".into());
            // format!(
            //     "{left_exp}\n{right_exp}\npopl %eax\npopl %ebx\n{binop} %ebx, %eax\npushl %eax"
            // )
        }
        left_exp
    }

    fn operation(operator: ast::UnaryOperator) -> Asm {
        // Note the following might be a bit of a premature optimization. For all the cases,
        // instead of pushing/popping the values off the stack into the ecx register, I instead
        // copy them into a register then operate on them, then write to a register, minimizing
        // costly memory operations
        // Note for all of these, rcx might be overwritten so in the future, might need to copy out the
        // values in rcx
        match operator {
            ast::UnaryOperator::Negation => Asm::instructions(vec![
                AsmInstr::from("movq", "(%rsp),%rcx"),
                AsmInstr::from("negq", "%rcx"),
                AsmInstr::from("movq", "%rcx,(%rsp)"),
            ]),
            ast::UnaryOperator::BitwiseComplement => Asm::instructions(vec![
                AsmInstr::from("movq", "(%rsp),%rcx"),
                AsmInstr::from("notq", "%rcx"),
                AsmInstr::from("movq", "%rcx,(%rsp)"),
            ]),
            ast::UnaryOperator::LogicalNegation => Asm::instructions(vec![
                AsmInstr::from("movq", "(%rsp),%rcx"),
                AsmInstr::from("cmpq", "$0,%rcx"),
                AsmInstr::from("movq", "$0,%rcx"),
                AsmInstr::from("sete", "%rcx"),
                AsmInstr::from("movq", "%rcx,(%rsp)"),
            ]),
            // format!("cmpl $0, {location}\nsete {location}"),
        }
    }

    fn binary_operation(operator: &ast::BinaryOperator) -> String {
        match operator {
            ast::BinaryOperator::Add => "addq".to_string(),
            ast::BinaryOperator::Minus => "subq".to_string(),
            ast::BinaryOperator::Multiply => "imulq".to_string(),
            ast::BinaryOperator::Divide => "idivq".to_string(),
            ast::BinaryOperator::Modulo => "idivq".to_string(),
            ast::BinaryOperator::Xor => "xorq".to_string(),
            ast::BinaryOperator::BitwiseAnd => "andq".to_string(),
            ast::BinaryOperator::BitwiseOr => "orq".to_string(),
            // TODO: fix these
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

    fn gen_new_label(&mut self, label_identifier: &str) -> String {
        let label = format!(".{label_identifier}{}", self.jump_counter);
        self.jump_counter += 1;
        label
    }

    fn gen_location(&mut self, allocation: u64) -> String {
        self.newest_mem_location += allocation;
        format!("-{}(%rbp)", self.newest_mem_location)
    }
}
