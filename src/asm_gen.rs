use std::collections::HashMap;

use crate::{
    assembly::{Asm, AsmInstr},
    ast,
    ast::{BinaryOperator, Expression},
    context::Context,
    fail,
    symbol_table::SymbolTable,
};

pub const LONG_SIZE: u64 = 8;
#[allow(unused)]
pub const INT_SIZE: u64 = 4;

pub const ARGUMENT_REGISTERS: [&'static str; 6] = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8d", "%r9d"];
pub enum OperationRegisters {
    Ebx,
    Ecx
}

impl OperationRegisters {
    pub fn str(&self) -> String {
        match self {
            Ebx => String::from("%ebx"),
            Ecx => String::from("%ecx"),
        }
    }

    pub fn not(&self) -> Self {
        match self {
            Ebx => Self::Ecx,
            Ecx => Self::Ebx,
        }
    }
}

#[allow(dead_code)]
pub struct AsmGenerator {
    jump_counter: u64,
    scope_counter: u64,
    symbol_table: SymbolTable,
    // maps function names to arguments
    function_table: HashMap<String, Vec<String>>,
    function_prototype_table: HashMap<String, Vec<String>>,
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
            function_table: HashMap::new(),
            function_prototype_table: HashMap::new(),
        }
    }

    // Generates the assembly from the abstract syntax tree
    pub fn generate(&mut self, ast: ast::Prog) -> Asm {
        let parent_scope = self.scope_counter;
        let child_scope = self.create_scope(parent_scope);
        let mut asm = Asm::default();
        match ast {
            // parses the program header
            ast::Prog::Prog(mut function_declarations) => {
                for function_declaration in &mut function_declarations {
                    match function_declaration {
                        ast::FuncDecl::Func(ref name, ref arguments, _) => {
                            let potential_args_length = self.function_prototype_table.get(name);
                            if let Some(args_length) = potential_args_length {
                                if arguments.len() != args_length.len() {
                                    fail!("Function `{name}` declared with `{}` arguments, but initialized with `{}` arguments", args_length.len(), arguments.len());
                                }
                            }
                            if self.function_table.contains_key(name) {
                                fail!("Function {name} already declared");
                            }
                            self.function_table.insert(name.clone(), arguments.clone());
                        }
                        ast::FuncDecl::FuncPrototype(name, args) => {
                            let potential_args_length = self.function_table.get(name);
                            if let Some(args_length) = potential_args_length {
                                if args.len() != args_length.len() {
                                    fail!("Function `{name}` initialized with `{}` arguments, but declared here with `{}` arguments", args_length.len(), args.len());
                                }
                            }
                            if self.function_prototype_table.contains_key(name) {
                                fail!("Function prototype {name} already declared");
                            }
                            self.function_prototype_table
                                .insert(std::mem::take(name), std::mem::take(args));
                        }
                    }
                }
                for function_declaration in function_declarations
                    .into_iter()
                    .filter(|function| matches!(function, ast::FuncDecl::Func(_, _, _)))
                {
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
                let identifier_label = format!("{indentifier}:");
                assembly.append_instruction(".globl".to_string(), indentifier);
                assembly.append_instruction(identifier_label, String::new());
                assembly.append_instruction("pushq".to_string(), "%rbp".to_string());
                assembly.append_instruction("movq".to_string(), "%rsp,%rbp".to_string());
                let child_scope = self.create_scope(parent_scope);
                self.symbol_table
                    .create_function_arguments(child_scope, &arguments);
                for statement in statements {
                    assembly.add_instructions(self.gen_block_item(statement, child_scope, &None));
                }
                // TODO: this might not work with conditionals, as the last statement is like
                // ".L{\w}"
                // let last = assembly.last();
                // if let Some(last) = assembly.
                match assembly.last() {
                    Some(last) if last.command() == "ret" => {
                        // assembly.add_instr_two_before_end(self.remove_scope(child_scope));
                    }
                    _ => {
                        assembly.add_instructions(self.remove_scope(child_scope));
                        assembly.append_instruction("xorq".into(), "%rax,%rax".into());
                        assembly.append_instruction("popq".into(), "%rbp".into());
                        assembly.append_instruction("ret".into(), String::new());
                    }
                }
                assembly
                // todo!()
                // TODO: branch analysis is bad on this
                // if assembly.last().command() != "ret" {
                //     assembly.append_instruction("xorq".into(), "%rax,%rax".into());
                //     assembly.append_instruction("ret".into(), String::new());
                // }
            }
            ast::FuncDecl::FuncPrototype(_, _) => {
                unreachable!()
            }
        }
    }

    fn gen_block_item(
        &mut self,
        block_item: ast::BlockItem,
        parent_scope: u64,
        potential_context: &Option<Context>,
    ) -> Asm {
        match block_item {
            ast::BlockItem::Statement(statement) => {
                self.gen_statement(statement, parent_scope, potential_context)
            }
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
                            AsmInstr::new("subq".into(), format!("${LONG_SIZE},%rsp")),
                        ],
                    );
                    constructed_assembly
                } else {
                    // let location = symbol_table.allocate(name, LONG_SIZE);
                    // TODO: figure out default for declare without initialization
                    // TODO: There may not need to be any instructions because we can just let it
                    // have whatever memory is already in the adress, like C stadard
                    // let asm = Asm::instruction("movq".into(), format!("$0, {location}"));
                    // WRONG WRONG MUST SUBTRACT SIZE :(
                    //asm
                    let _location = self.symbol_table.allocate(name, parent_scope, LONG_SIZE);
                    Asm::instruction("subq".to_string(), format!("${LONG_SIZE},%rsp"))
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
        let context = Context::new(start_of_while_name.clone(), end_of_while_name.clone());
        let mut asm = Asm::instruction(start_of_while_label, String::new());
        let stmt = self.gen_statement(*statement, parent_scope, &Some(context));
        stmt.print();
        asm.add_instructions(stmt);
        let exp = self.gen_expression(expression, parent_scope);
        asm.add_instructions(exp);
        asm.append_instruction("cmpq".to_string(), "$0,%rax".to_string());
        // asm.append_instruction("popq".to_string(), "%r9".to_string());
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
        let context = Context::new(start_of_while_name.clone(), end_of_while_name.clone());
        let mut asm = Asm::instruction(start_of_while_label, String::new());
        let exp = self.gen_expression(expression, parent_scope);
        asm.add_instructions(exp);
        asm.append_instruction("cmpq".to_string(), "$0,%rax".to_string());
        // asm.append_instruction("popq".to_string(), "%r9".to_string());
        // asm.append_instruction("xorq".to_string(), "%rax,%rax".to_string());
        // asm.append_instruction("sete".to_string(), "%al".to_string());
        asm.append_instruction("je".to_string(), end_of_while_name);
        let statement = dbg!(statement);
        let stmt = self.gen_statement(*statement, parent_scope, &Some(context));
        stmt.print();
        asm.add_instructions(stmt);
        asm.append_instruction("jmp".to_string(), start_of_while_name);
        asm.append_instruction(end_of_while_label, String::new());
        asm
    }

    fn gen_statement(
        &mut self,
        statement: ast::Statement,
        parent_scope: u64,
        potential_context: &Option<Context>,
    ) -> Asm {
        match statement {
            ast::Statement::Continue => {
                if let Some(context) = potential_context {
                    Asm::instruction("jmp".to_string(), context.continue_jump())
                } else {
                    fail!("Cannot continue from a non loop context");
                }
            }
            ast::Statement::Break => {
                if let Some(context) = potential_context {
                    Asm::instruction("jmp".to_string(), context.break_jump())
                } else {
                    fail!("Cannot break from a non loop context");
                }
            }
            ast::Statement::While(expression, statement) => {
                self.gen_while_loop(expression, statement, parent_scope)
            }
            ast::Statement::Do(statement, expression) => {
                self.gen_do_loop(expression, statement, parent_scope)
            }
            ast::Statement::ForDecl(decl, exp1, exp2, body) => {
                let child_scope = self.create_scope(parent_scope);
                let mut asm = self.gen_declaration(decl, child_scope);
                asm.add_instructions(self.gen_for_loop(exp1, exp2, body, child_scope));
                asm.add_instructions(self.remove_scope(child_scope));
                asm
            }
            ast::Statement::For(optional_exp, exp1, exp2, body) => {
                // same as for decl generally, except the very first part
                let mut asm: Asm = if let Some(exp) = optional_exp {
                    // let mut assembly = self.gen_expression(exp, parent_scope);
                    // assembly.append_instruction("popq".to_string(), "%r9".to_string());
                    // assembly
                    self.gen_expression(exp, parent_scope)
                } else {
                    Asm::default()
                };
                asm.add_instructions(self.gen_for_loop(exp1, exp2, body, parent_scope));
                asm
            }
            ast::Statement::Return(expression) => {
                let Some(size_of_current_stack_frame) = self.symbol_table.top_of_stack(parent_scope) else {
                    fail!("Error, scope `{parent_scope}` doesn't exist");
                };
                // let size_of_current_stack_frame = dbg!(size_of_current_stack_frame);
                // println!("Symbol table: \n{:#?}", self.symbol_table);
                let constructed_assembly = Asm::new(
                    self.gen_expression(expression, parent_scope),
                    vec![
                        /* AsmInstr::from("popq", "%rbx"),  */
                        // AsmInstr::from("popq", "%rax"),
                        // AsmInstr::new("cdq".to_string(), String::new()),
                        AsmInstr::new("#".into(), format!("${size_of_current_stack_frame} being returned")),
                        AsmInstr::new("addq".into(), format!("${size_of_current_stack_frame},%rsp")),
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
                // let mut assembly = self.gen_expression(expression, parent_scope);
                self.gen_expression(expression, parent_scope)
                // assembly.append_instruction("popq".to_string(), "%r9".to_string());
                // assembly
            }
            ast::Statement::Conditional(expression, if_children, potential_else_children) => {
                let mut asm = Asm::default();    
                asm.append_instruction("#".into(), format!("start of if"));
                asm.add_instructions(self.gen_expression(expression, parent_scope));
                // let mut asm = self.gen_expression(expression, parent_scope);
                let (else_, else_label) = self.gen_new_label();
                asm.add_instructions(Asm::instructions(vec![
                    // AsmInstr::from("popq", "%rax"),
                    AsmInstr::from("cmpq", "$0,%rax"),
                    AsmInstr::new("je".into(), else_.clone()),
                ]));
                // for statement in if_children {
                asm.append_instruction("#".into(), format!("start of conditional body"));
                asm.add_instructions(self.gen_statement(
                    *if_children,
                    parent_scope,
                    potential_context,
                ));
                // }
                if let Some(else_children) = *potential_else_children {
                    // TODO: get labels, else, and endofif
                    let (endif_, endif_label) = self.gen_new_label();
                    asm.append_instruction("#".into(), format!("start of else"));
                    asm.add_instructions(Asm::instructions(vec![
                        AsmInstr::new("jmp".into(), endif_),
                        AsmInstr::new(else_label, String::new()),
                    ]));
                    // for statement in else_children {
                    asm.append_instruction("#".into(), format!("start of else body"));
                    asm.add_instructions(self.gen_statement(
                        else_children,
                        parent_scope,
                        potential_context,
                    ));
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
            ast::Statement::Block(block) => self.gen_block(block, parent_scope, potential_context),
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
    ) -> Asm {
        // loop:
        //     <check_condition>
        //     <loop_body>
        // after_body:
        //     <post_expression>
        //     jump loop
        // after_loop:
        let mut asm = Asm::default();
        let (begin_for_name, begin_for_label) = self.gen_new_label();
        asm.append_instruction(begin_for_label, String::new());
        let (end_for_name, end_for_label) = self.gen_new_label();
        let (after_for_name, after_for_label) = self.gen_new_label();
        let context = Context::new(after_for_name, end_for_name.clone());
        if !matches!(exp, Expression::NullExp) {
            asm.add_instructions(self.gen_expression(exp, parent_scope));
            // asm.append_instruction("popq".to_string(), "%rax".to_string());
            asm.append_instruction("cmpq".to_string(), "$0,%rax".to_string());
            asm.append_instruction("je".to_string(), end_for_name);
        }
        asm.add_instructions(self.gen_statement(*body, parent_scope, &Some(context)));
        asm.append_instruction(after_for_label, String::new());
        if let Some(expr) = optional_exp {
            asm.add_instructions(self.gen_expression(expr, parent_scope));
            // asm.append_instruction("popq".to_string(), "%r9".to_string());
        }
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

    fn gen_block(
        &mut self,
        block: Vec<ast::BlockItem>,
        parent_scope: u64,
        potential_context: &Option<Context>,
    ) -> Asm {
        let mut asm = Asm::default();
        let _ = self.create_scope(parent_scope);
        let child_scope = self.scope_counter;
        // let parent_scope = self.scope_counter;
        for block_item in block {
            asm.add_instructions(self.gen_block_item(block_item, child_scope, potential_context));
        }
        asm.add_instructions(self.remove_scope(child_scope));
        asm
    }

    // The convention is that the result of the expression is stored at the top of the stack, as
    // such, the caller has the responsibility to properly pop the top element off of the stack
    // after calling this function. If this does not happen, there will be segfaults in the
    // generated assembly
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
                // asm.append_instruction("popq".into(), "%rax".into());
                asm.append_instruction("cmpq".into(), "$0,%rax".into());
                asm.append_instruction("je".into(), else_name);
                asm.add_instructions(self.gen_expression(*expression2, parent_scope));
                asm.append_instruction("jmp".into(), endternary_name);
                asm.append_instruction(else_label, String::new());
                asm.add_instructions(self.gen_expression(*expression3, parent_scope));
                asm.append_instruction(endternary_label, String::new());
                asm
            }
            ast::Expression::FunctionCall(name, arguments) => {
                if !self.function_table.contains_key(&name) && !self.function_prototype_table.contains_key(&name) {
                    fail!("Function `{name}` referenced but not created");
                }
                if let Some(prototype) = self.function_prototype_table.get(&name) {
                    if arguments.len() != prototype.len() {
                        // TODO make better error here: which vars are required etc
                        fail!(
                            "Function `{name}` requires {} arguments, but got {} arguments",
                            prototype.len(),
                            arguments.len()
                            );
                    }
                } else if let Some(args) = self.function_table.get(&name) {
                    if arguments.len() != args.len() {
                        // TODO make better error here: which vars are required etc
                        fail!(
                            "Function `{name}` requires {} arguments, but got {} arguments",
                            args.len(),
                            arguments.len()
                            );
                    }
                }
                // function has been created and has the right amount of arguments
                let mut asm = Asm::default();
                // let dealloc_amt = 8 * (arguments.len() - 6);
                let dealloc_amt = 8 * (arguments.len());
                for argument in arguments.into_iter()/* .rev() */ {
                    asm.add_instructions(self.gen_expression(argument, parent_scope));
                    // TODO get arguments in registers instead of stack
                    // suppossedly this isn't needed because each result is pushed onto the
                    // stack... we will see
                    asm.append_instruction("pushq".to_string(), "%rax".to_string());
                }
                asm.append_instruction("call".to_string(), name);
                // TODO when variables can have a differing size, change this into a for loop
                // to calculate the amount to deallocate
                asm.append_instruction("addq".to_string(), format!("${dealloc_amt},%rsp"));
                // return values of functions are generally stored in rax, so push them to the
                // top of the stack in order to ensure they are in the right place, maintain
                // top of stack invariant so to speak
                // asm.append_instruction("pushq".to_string(), format!("%rax"));
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
        match binary_operator {
            BinaryOperator::LogicalOr => {
                let mut left_exp = self.gen_expression(*left_expr, parent_scope);
                // left_exp.append_instruction("popq".into(), "%rbx".into());
                // left_exp.append_instruction("cmpq".into(), "$0,(%rsp)".into());
                left_exp.append_instruction("cmpq".into(), "$0,%rax".into());
                let (label1_name, label1) = self.gen_new_label();
                let (label2_name, label2) = self.gen_new_label();
                left_exp.append_instruction("je".into(), label1_name);
                // left_exp.append_instruction("movq".into(), "$1,(%rsp)".into());
                left_exp.append_instruction("movq".into(), "$1,%rax".into());
                left_exp.append_instruction("jmp".into(), label2_name);
                left_exp.append_instruction(label1, String::new());
                let right_exp = self.gen_expression(*right_expr, parent_scope);
                left_exp.add_instructions(right_exp);
                // left_exp.append_instruction("cmpq".into(), "$0,(%rsp)".into());
                left_exp.append_instruction("cmpq".into(), "$0,%rax".into());
                left_exp.append_instruction("movq".into(), "$0,%rax".into());
                left_exp.append_instruction("setne".into(), "%al".into());
                // left_exp.append_instruction("movq".into(), "%rax,(%rsp)".into());
                left_exp.append_instruction(label2, "".into());
                return left_exp;
            }
            BinaryOperator::LogicalAnd => {
                let mut left_exp = self.gen_expression(*left_expr, parent_scope);
                // left_exp.append_instruction("popq".into(), "%rbx".into());
                // left_exp.append_instruction("cmpq".into(), "$0,(%rsp)".into());
                left_exp.append_instruction("cmpq".into(), "$0,%rax".into());
                let (label1_name, label1) = self.gen_new_label();
                let (label2_name, label2) = self.gen_new_label();
                left_exp.append_instruction("jne".into(), label1_name);
                left_exp.append_instruction("jmp".into(), label2_name);
                left_exp.append_instruction(label1, "".into());
                let right_exp = self.gen_expression(*right_expr, parent_scope);
                left_exp.add_instructions(right_exp);
                // left_exp.append_instruction("cmpq".into(), "$0,(%rsp)".into());
                left_exp.append_instruction("cmpq".into(), "$0,%rax".into());
                left_exp.append_instruction("movq".into(), "$0,%rax".into());
                left_exp.append_instruction("setne".into(), "%al".into());
                // left_exp.append_instruction("movq".into(), "%rax,(%rsp)".into());
                left_exp.append_instruction(label2, "".into());
                return left_exp;
            }
            _ => {}
        }

        let mut left_exp = self.gen_expression(*left_expr, parent_scope);
        left_exp.append_instruction("pushq".into(), "%rax".into());
        // Left expression is in %rbx, right will be in %rax
        let right_exp = self.gen_expression(*right_expr, parent_scope);
        left_exp.add_instructions(right_exp);
        left_exp.append_instruction("movq".into(), "%rax,%rcx".into());
        left_exp.append_instruction("popq".into(), "%rax".into());
        // Moves left expression to %rax, and right to %rbx
        // todo!("This may not work because rbx may be overwritten when the right expression is itself an expressino and not a constant value");
        // left_exp.append_instruction("popq".into(), "%rbx".into());
        let binop = Self::binary_operation(&binary_operator);
        match binary_operator {
            BinaryOperator::Divide => {
                // we want to divide left by right expression, so we must exchange the contents of rbx
                // and rax
                // left_exp.append_instruction("popq".into(), "%rbx".into());
                // left_exp.append_instruction("popq".into(), "%rax".into());
                left_exp.append_instruction("cqo".into(), String::new());
                left_exp.append_instruction(binop, "%rcx".into());
                // left_exp.append_instruction("pushq".into(), "%rax".into());
                // left_exp.append_instruction("pushq".into(), "%rax".into());
            }
            BinaryOperator::Multiply => {
                // TODO: result is actually not just in rax, but in two registers potentially so
                // see that that is fixed
                // left_exp.append_instruction("popq".into(), "%rbx".into());
                // left_exp.append_instruction("popq".into(), "%rax".into());
                // left_exp.append_instruction(binop, "(%rsp),%rbx".into());
                left_exp.append_instruction(binop, "rcx,rax".into());
                // TODO, part of the result will be in rdx
                // left_exp.append_instruction("movq".into(), "%rbx,(%rsp)".into());
                // left_exp.append_instruction("pushq".into(), "%rax".into());
            }
            BinaryOperator::Modulo => {
                // left_exp.append_instruction("popq".into(), "%rbx".into());
                // left_exp.append_instruction("popq".into(), "%rax".into());
                left_exp.append_instruction("cqo".into(), String::new());
                left_exp.append_instruction(binop, "%rcx".into());
                left_exp.append_instruction("movq".into(), "%rdx,%rax".into());
                // left_exp.append_instruction("pushq".into(), "%rdx".into());
                // left_exp.append_instruction("pushq".into(), "%rax".into());

            }
            BinaryOperator::BitwiseLeftShift | BinaryOperator::BitwiseRightShift => {
                // TODO: maybe clear the rcx register?
                // left_exp.append_instruction("popq".into(), "%rcx".into());
                // left_exp.append_instruction(binop, "%rbx,(%rsp)".into());
                left_exp.append_instruction(binop, "%rcx,%rax".into());
            }
            // The following might need to be fixed
            // => {
            //     // left_exp.append_instruction("popq".into(), "%rbx".into());
            //     // left_exp.append_instruction("popq".into(), "%rax".into());
            //     left_exp.append_instruction("cmpq".into(), "%rbx,%rax".into());
            //     left_exp.append_instruction("movq".into(), "$0,%rax".into());
            //     left_exp.append_instruction(binop, "%al".into());
            //     // left_exp.append_instruction("pushq".into(), "%rax".into());
            // }
            BinaryOperator::Equal
            | BinaryOperator::NotEqual
            | BinaryOperator::GreaterEq
            | BinaryOperator::Greater
            | BinaryOperator::LessEq
            | BinaryOperator::Less => {
                // left_exp.append_instruction("popq".into(), "%rbx".into());
                // left_exp.append_instruction("popq".into(), "%rax".into());
                left_exp.append_instruction("cmpq".into(), "%rcx,%rax".into());
                left_exp.append_instruction("movq".into(), "$0,%rax".into());
                left_exp.append_instruction(binop, "%al".into());
                // left_exp.append_instruction("pushq".into(), "%rax".into());
            }
            BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr => unreachable!(),
            _ => {
                // Note the following might be a bit of a premature optimization. I assume that the
                // size of the data is 4 bytes, so I can read the first two values off the stack
                // into a register, and then operate on them and write them back to the stack,
                // minimizing push/pop instructions
                // left_exp.append_instruction("popq".into(), "%rax".into());
                left_exp.append_instruction(binop, "%rcx,%rax".into());
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
                AsmInstr::from("movq", "$0,%rax"),
                AsmInstr::from("sete", "%al"),
                // AsmInstr::from("movq", "%rax,(%rsp)"),
            ]),
            ast::UnaryOperator::PostfixIncrement(name) => {
                let location = self.symbol_table.get(&name, parent_scope).expect(&format!(
                    "Variable {} in scope '{} accessed but not declared",
                    name, parent_scope
                ));
                Asm::instructions(vec![
                    // AsmInstr::new("pushq".into(), format!("{location}")),
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
                    // AsmInstr::new("pushq".into(), format!("{location}")),
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
                    // AsmInstr::new("pushq".into(), format!("{location}")),
                ])
            }
            ast::UnaryOperator::PostfixDecrement(name) => {
                let location = self.symbol_table.get(&name, parent_scope).expect(&format!(
                    "Variable {} in scope '{} accessed but not declared",
                    name, parent_scope
                ));
                Asm::instructions(vec![
                    // AsmInstr::new("pushq".into(), format!("{location}")),
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
