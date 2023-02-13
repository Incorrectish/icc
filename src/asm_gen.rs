use crate::{
    assembly::{Asm, AsmInstr},
    ast,
    ast::BinaryOperator,
};

pub struct AsmGenerator {
    jump_counter: u64,
}

impl AsmGenerator {
    pub fn new() -> Self {
        AsmGenerator { jump_counter: 0 }
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
            ast::FuncDecl::Func(indentifier, statement) => Asm::from_instr(
                vec![
                    AsmInstr::new(format!("{indentifier}:"), String::new()),
                    AsmInstr::new(".globl".to_string(), indentifier.clone()),
                ],
                self.gen_statement(statement),
            ),
        }
    }

    fn gen_statement(&mut self, statement: ast::Statement) -> Asm {
        match statement {
            // parses the statements
            ast::Statement::Return(expr) => {
                let constructed_assembly = Asm::new(
                    self.gen_expression(expr),
                    vec![AsmInstr::from("popq", "%rax"), AsmInstr::from("ret", "")],
                );
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
                let mut left_exp = self.gen_expression(*left_expr);
                let right_exp = self.gen_expression(*right_expr);
                left_exp.add_instructions(right_exp);
                let binop = if !matches!(binary_operator, BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr) {
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
                    let label3_name = self.gen_new_label("logicaljump");
                    let label3 = format!("{label3_name}:");
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
        }
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

            ast::BinaryOperator::LogicalAnd => unreachable!("You should not use this function on a logical and input"),
            ast::BinaryOperator::LogicalOr =>  unreachable!("You should not use this function on a logical or input"),
        }
    }

    fn gen_new_label(&mut self, label_identifier: &str) -> String {
        let label = format!(".{label_identifier}{}", self.jump_counter);
        self.jump_counter += 1;
        label
    }
}
