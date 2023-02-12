use crate::{
    assembly::{Asm, AsmInstr},
    ast,
    ast::BinaryOperator,
    asm_gen
};

// Generates the assembly from the abstract syntax tree
pub fn generate(ast: ast::Prog) -> Asm {
    match ast {
        // parses the program header
        ast::Prog::Prog(function_decleration) => asm_gen::gen_function(function_decleration),
    }
}

fn gen_function(function_declaration: ast::FuncDecl) -> Asm {
    match function_declaration {
        ast::FuncDecl::Func(indentifier, statement) => Asm::from_instr(
            vec![
                AsmInstr::new(".globl".to_string(), indentifier.clone()),
                AsmInstr::new(format!("{indentifier}:"), String::new()),
            ],
            asm_gen::gen_statement(statement),
        ),
    }
}

fn gen_statement(statement: ast::Statement) -> Asm {
    match statement {
        // parses the statements
        ast::Statement::Return(expr) => {
            let constructed_assembly = Asm::new(
                asm_gen::gen_expression(expr, "%rax"),
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

fn gen_expression(expr: ast::Expression, location: &str) -> Asm {
    match expr {
        ast::Expression::Constant(int) => Asm::instruction("pushq".to_string(), format!("${int}")),
        ast::Expression::UnaryOp(operator, expression) => {
            let mut asm = asm_gen::gen_expression(*expression, location);
            asm.add_instructions(asm_gen::operation(operator, location));
            asm
        }
        ast::Expression::BinaryOp(binary_operator, left_expr, right_expr) => {
            let mut left_exp = asm_gen::gen_expression(*left_expr, location);
            let right_exp = asm_gen::gen_expression(*right_expr, location);
            left_exp.add_instructions(right_exp);
            let binop = asm_gen::binary_operation(&binary_operator);
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
            } else if matches!(binary_operator, BinaryOperator::BitwiseLeftShift | BinaryOperator::BitwiseRightShift) {
                // TODO: maybe clear the rcx register?
                left_exp.append_instruction("popq".into(), "%rcx".into());
                left_exp.append_instruction(binop, "%rcx,(%rsp)".into());
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

fn operation(operator: ast::UnaryOperator, location: &str) -> Asm {
    // Note the following might be a bit of a premature optimization. For all the cases,
    // instead of pushing/popping the values off the stack into the ecx register, I instead
    // copy them into a register then operate on them, then write to a register, minimizing
    // costly memory operations
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
            AsmInstr::from("cmpl", "$0,%rcx"),
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
    }
}
