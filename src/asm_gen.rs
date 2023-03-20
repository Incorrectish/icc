use std::collections::HashMap;

use crate::{
    assembly::{Asm, AsmInstr},
    ast,
    ast::{BinaryOperator, Expression},
    context::Context,
    expression::{Constant, ExpType},
    fail,
    symbol_table::{SymbolTable, TYPE_SIZES},
    variable::Var,
};


/*
 * IMPORTANT: for assembly comparison order 
    movq $8, %rax
    movq $7, %rcx
    cmpq %rax, %rcx
    movq $0,%rax 
    setl %al
 * %al is 1 in this case, which means cmpq rcx rax compares rcx to rax
 */


pub const TYPE_INT: &'static str = "int";
pub const TYPE_LONG: &'static str = "long";
pub const TYPE_VOID: &'static str = "void";

// pub const LONG_SIZE: u64 = 8;
#[allow(unused)]
// pub const INT_SIZE: u64 = 4;

pub const ARGUMENT_REGISTERS: [&'static str; 6] = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8d", "%r9d"];

#[allow(dead_code)]
pub struct AsmGenerator {
    jump_counter: u64,
    scope_counter: u64,
    symbol_table: SymbolTable,
    // maps function names to arguments
    function_table: HashMap<String, Vec<Var>>,
    function_prototype_table: HashMap<String, Vec<Var>>,
    function_return_types: HashMap<String, &'static str>,
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
            function_return_types: HashMap::new(),
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
                        ast::FuncDecl::Func(ref var, ref arguments, _) => {
                            let name = var.name();
                            let potential_args_length = self.function_prototype_table.get(name);
                            if let Some(args_length) = potential_args_length {
                                if arguments.len() != args_length.len() {
                                    fail!("Function `{name}` declared with `{}` arguments, but initialized with `{}` arguments", args_length.len(), arguments.len());
                                }
                            }
                            if self.function_table.contains_key(var.name()) {
                                fail!("Function {name} already declared");
                            }
                            self.function_table.insert(name.clone(), arguments.clone());
                            self.function_return_types.insert(name.clone(), var.type_());
                        }
                        ast::FuncDecl::FuncPrototype(var, args) => {
                            let name = var.name();
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
                                .insert(name.clone(), std::mem::take(args));
                            self.function_return_types
                                .insert(var.take_name(), var.type_());
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
            ast::FuncDecl::Func(mut var, arguments, statements) => {
                let mut assembly = Asm::default();
                let identifier_label = format!("{identifier}:", identifier = var.name());
                assembly.append_instruction(".globl".to_string(), var.name().clone());
                assembly.append_instruction(identifier_label, String::new());
                assembly.append_instruction("push".to_string(), "%rbp".to_string());
                assembly.append_instruction("mov".to_string(), "%rsp,%rbp".to_string());
                let child_scope = self.create_scope(parent_scope);
                self.symbol_table
                    .create_function_arguments(child_scope, &arguments);
                for statement in statements {
                    assembly.add_instructions(self.gen_block_item(statement, child_scope, &None, var.name()));
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
                        assembly.append_instruction("xor".into(), "%rax,%rax".into());
                        assembly.append_instruction("pop".into(), "%rbp".into());
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
        fn_name: &str
    ) -> Asm {
        match block_item {
            ast::BlockItem::Statement(statement) => {
                self.gen_statement(statement, parent_scope, potential_context, fn_name)
            }
            ast::BlockItem::Declaration(declaration) => {
                self.gen_declaration(declaration, parent_scope)
            }
        }
    }

    fn gen_declaration(&mut self, declaration: ast::Declaration, parent_scope: u64) -> Asm {
        match declaration {
            ast::Declaration::Declare(var, optional_expression, optional_child_declaration) => {
                let size = TYPE_SIZES[var.type_()];
                let (register, suffix) = Self::gen_register_a(var.type_());
                let mut declaration_assembly = if let Some(expression) = optional_expression {
                    let location = self.symbol_table.allocate(&var, parent_scope);
                    let expression_type = self.gen_expression(expression, parent_scope);
                    match expression_type {
                        ExpType::Type(mut asm, type_) => {
                            if type_ != var.type_() {
                                eprintln!("Type of {var} does not match expression type {type_:?}");
                            }
                            asm.append_instruction(
                                format!("mov{suffix}"),
                                format!("%{register},{location}"),
                            );
                            asm.append_instruction(format!("subq"), format!("${size},%rsp"));
                            asm
                        }
                        ExpType::Constant(constant) => match constant {
                            Constant::Int(int) => {
                                let mut asm = Asm::default();
                                asm.append_instruction(
                                    format!("mov{suffix}"),
                                    format!("${int},{location}"),
                                );
                                asm.append_instruction(format!("subq"), format!("${size},%rsp"));
                                asm
                            }
                        },
                    }
                } else {
                    // let location = symbol_table.allocate(name, LONG_SIZE);
                    // TODO: figure out default for declare without initialization
                    // TODO: There may not need to be any instructions because we can just let it
                    // have whatever memory is already in the adress, like C stadard
                    // let asm = Asm::instruction("movq".into(), format!("$0, {location}"));
                    // WRONG WRONG MUST SUBTRACT SIZE :(
                    //asm
                    let _location = self.symbol_table.allocate(&var, parent_scope);
                    Asm::instruction(
                        "sub".to_string(),
                        format!("${},%rsp", TYPE_SIZES[var.type_()]),
                    )
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
        fn_name: &str
    ) -> Asm {
        // .startofdo:
        // gen_stmt
        // gen_expr
        // cmpq $0,%rax
        // je .endofwhile
        // jmp .startofwhile
        // .endofwhile:
        // let expression = dbg!(expression);
        let (start_of_while_name, start_of_while_label) = dbg!(self.gen_new_label());
        let (end_of_while_name, end_of_while_label) = self.gen_new_label();
        let context = Context::new(start_of_while_name.clone(), end_of_while_name.clone());
        let mut asm = Asm::instruction(start_of_while_label, String::new());
        let stmt = self.gen_statement(*statement, parent_scope, &Some(context), fn_name);
        asm.add_instructions(stmt);
        let expression_type = self.gen_expression(expression, parent_scope);
        // TODO: Proofread
        asm.add_instructions(match expression_type {
            // TODO: fix this later
            ExpType::Constant(constant) => {
                match constant {
                    // If the constant value is 0, do not repeat the loop, jump to the end
                    Constant::Int(int) if int == 0 => {
                        Asm::instruction("jmp".to_string(), end_of_while_name)
                    }
                    Constant::Int(_) => Asm::default(),
                }
            }
            ExpType::Type(mut asm, type_) => {
                let (register, suffix) = Self::gen_register_a(type_);
                // TODO
                // asm.add_instructions(exp);
                asm.append_instruction(format!("cmp{suffix}"), format!("$0,%{register}"));
                asm.append_instruction("je".to_string(), end_of_while_name);
                asm
            }
        });
        asm.append_instruction("jmp".to_string(), start_of_while_name);
        asm.append_instruction(end_of_while_label, String::new());
        asm
    }

    fn gen_while_loop(
        &mut self,
        expression: ast::Expression,
        statement: Box<ast::Statement>,
        parent_scope: u64,
        fn_name: &str
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
        let expression_type = self.gen_expression(expression, parent_scope);
        asm.add_instructions(match expression_type {
            ExpType::Constant(constant) => {
                // If the constant value is 0, do not repeat the loop, jump to the end
                match constant {
                    Constant::Int(int) if int == 0 => {
                        Asm::instruction("jmp".to_string(), end_of_while_name)
                    }
                    _ => Asm::default(),
                }
            }
            ExpType::Type(mut asm, type_) => {
                let (register, suffix) = Self::gen_register_a(type_);
                // asm.add_instructions(exp);
                asm.append_instruction(format!("cmp{suffix}"), format!("$0,%{register}"));
                asm.append_instruction("je".to_string(), end_of_while_name);
                asm
            }
        });
        // let statement = dbg!(statement);
        let stmt = self.gen_statement(*statement, parent_scope, &Some(context), fn_name);
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
        fn_name: &str
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
                self.gen_while_loop(expression, statement, parent_scope, fn_name)
            }
            ast::Statement::Do(statement, expression) => {
                self.gen_do_loop(expression, statement, parent_scope, fn_name)
            }
            ast::Statement::ForDecl(decl, exp1, exp2, body) => {
                let child_scope = self.create_scope(parent_scope);
                let mut asm = self.gen_declaration(decl, child_scope);
                asm.add_instructions(self.gen_for_loop(exp1, exp2, body, child_scope, fn_name));
                asm.add_instructions(self.remove_scope(child_scope));
                asm
            }
            ast::Statement::For(optional_exp, exp1, exp2, body) => {
                // same as for decl generally, except the very first part
                let mut asm: Asm = if let Some(exp) = optional_exp {
                    let expression_type = self.gen_expression(exp, parent_scope);
                    match expression_type {
                        ExpType::Constant(constant) => Asm::default(),
                        ExpType::Type(asm, type_) => asm,
                    }
                } else {
                    Asm::default()
                };
                asm.add_instructions(self.gen_for_loop(exp1, exp2, body, parent_scope, fn_name));
                asm
            }
            ast::Statement::Return(expression) => {
                let Some(size_of_current_stack_frame) = self.symbol_table.top_of_stack(parent_scope) else {
                    fail!("Error, scope `{parent_scope}` doesn't exist");
                };
                // let size_of_current_stack_frame = dbg!(size_of_current_stack_frame);
                // println!("Symbol table: \n{:#?}", self.symbol_table);
                let expression_type = self.gen_expression(expression, parent_scope);
                // TODO check if function return type matches expression type
                let return_asm = match expression_type {
                    ExpType::Type(exp_asm, type_) => {
                        let return_type = self.function_return_types[fn_name];
                        if return_type != type_ {
                            fail!("Cannot return {type_}, when function return type is {return_type}");
                        }
                        exp_asm
                    },
                    ExpType::Constant(constant) => match constant {
                        Constant::Int(int) => {
                            Asm::instruction(format!("movl"), format!("${int},%eax"))
                        }
                    },
                };
                let constructed_assembly = Asm::new(
                    return_asm,
                    vec![
                        /* AsmInstr::from("popq", "%rbx"),  */
                        // AsmInstr::from("popq", "%rax"),
                        // AsmInstr::new("cdq".to_string(), String::new()),
                        AsmInstr::new(
                            "#".into(),
                            format!("${size_of_current_stack_frame} being deallocated"),
                        ),
                        AsmInstr::new("add".into(), format!("${size_of_current_stack_frame},%rsp")),
                        AsmInstr::from("pop", "%rbp"),
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
                let expression_type = self.gen_expression(expression, parent_scope);
                match expression_type {
                    ExpType::Constant(_) => Asm::default(),
                    ExpType::Type(asm, type_) => asm,
                }
                // assembly.append_instruction("popq".to_string(), "%r9".to_string());
                // assembly
            }
            ast::Statement::Conditional(expression, if_children, potential_else_children) => {
                let mut asm = Asm::default();
                asm.append_instruction("#".into(), format!("start of if"));
                let (else_, else_label) = self.gen_new_label();
                let exp_type = self.gen_expression(expression, parent_scope);
                match exp_type {
                    ExpType::Type(exp_asm, type_) => {
                        let (register, suffix) = Self::gen_register_a(type_);

                        // these instruction were for the expression
                        asm.add_instructions(exp_asm);
                        asm.add_instructions(Asm::instructions(vec![
                            // AsmInstr::from("popq", "%rax"),
                            AsmInstr::new(format!("cmp{suffix}"), format!("$0,%{register}")),
                            AsmInstr::new("je".into(), else_.clone()),
                        ]));
                    }
                    ExpType::Constant(constant) => {
                        match constant {
                            Constant::Int(int) => {
                                if int == 0 {
                                    asm.append_instruction("jmp".into(), else_.clone())
                                    // skip if condition
                                } else {
                                    // skip else condition, or remove the conditional check
                                }
                            }
                        }
                    }
                }
                // let mut asm = self.gen_expression(expression, parent_scope);
                // for statement in if_children {
                asm.append_instruction("#".into(), format!("start of conditional body"));
                asm.add_instructions(self.gen_statement(
                    *if_children,
                    parent_scope,
                    potential_context,
                    fn_name
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
                        fn_name
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
            ast::Statement::Block(block) => self.gen_block(block, parent_scope, potential_context, fn_name),
        }
    }

    fn remove_scope(&mut self, parent_scope: u64) -> Asm {
        let scope_size = self.symbol_table.get_scope_size(parent_scope);
        Asm::instruction("add".to_string(), format!("${scope_size},%rsp"))
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
        fn_name: &str
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
            let expression_type = self.gen_expression(exp, parent_scope);
            match expression_type {
                ExpType::Type(inner_asm, exp_type) => {
                    // let inner_asm = dbg!(inner_asm);
                    asm.add_instructions(inner_asm);
                    let (register, suffix) = Self::gen_register_a(exp_type);
                    asm.append_instruction(format!("cmp{suffix}"), format!("$0,%{register}"));
                    asm.append_instruction("je".to_string(), end_for_name);
                }
                ExpType::Constant(constant) => {
                    match constant {
                        Constant::Int(int) => {
                            if int == 0 {
                                asm.append_instruction("jmp".into(), end_for_name);
                            } else {
                                // generate no assembly
                            }
                        }
                    }
                }
            }
            // asm.append_instruction("popq".to_string(), "%rax".to_string());
        }
        // dbg!(&self.symbol_table);
        asm.add_instructions(self.gen_statement(*body, parent_scope, &Some(context), fn_name));
        asm.append_instruction(after_for_label, String::new());
        if let Some(expr) = optional_exp {
            let expression_type = self.gen_expression(expr, parent_scope);
            match expression_type {
                ExpType::Constant(_) => {} // do nothing
                ExpType::Type(inner_asm, type_) => {
                    // probably add the inner asm
                    asm.add_instructions(inner_asm);
                }
            }
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
        fn_name: &str
    ) -> Asm {
        let mut asm = Asm::default();
        let _ = self.create_scope(parent_scope);
        let child_scope = self.scope_counter;
        // let parent_scope = self.scope_counter;
        for block_item in block {
            asm.add_instructions(self.gen_block_item(block_item, child_scope, potential_context, fn_name));
        }
        asm.add_instructions(self.remove_scope(child_scope));
        asm
    }

    // The expression must have a type and returns its type, (generated_assembly, type)
    // must figure out the type of an expression somehow, only two real ways to determine,
    // from function calls or variable types
    fn gen_expression(&mut self, expr: ast::Expression, parent_scope: u64) -> ExpType {
        match expr {
            ast::Expression::NullExp => ExpType::Type(Asm::default(), TYPE_VOID),
            ast::Expression::Constant(constant) => ExpType::Constant(constant),
            ast::Expression::UnaryOp(operator, expression) => {
                let exp_type = self.gen_expression(*expression, parent_scope);
                match exp_type {
                    ExpType::Constant(constant) => {
                        ExpType::Constant(Self::eval_unary_op(operator, constant))
                    }
                    ExpType::Type(mut asm, type_) => {
                        asm.add_instructions(self.operation(operator, parent_scope, type_));
                        ExpType::Type(asm, type_)
                    }
                }
            }
            ast::Expression::BinaryOp(binary_operator, left_expr, right_expr) => self
                .gen_binary_operation_expression(
                    binary_operator,
                    left_expr,
                    right_expr,
                    parent_scope,
                ),
            ast::Expression::Assign(name, expression) => {
                let exp_type = self.gen_expression(*expression, parent_scope);
                let optional_information = self.symbol_table.get(&name, parent_scope);
                if let Some((location, var_type)) = optional_information {
                    match exp_type {
                        ExpType::Type(mut asm, type_) => {
                            let (register, suffix) = Self::gen_register_a(type_);
                            // TODO: this probably doesn't work, look at this bullshit later
                            asm.append_instruction(
                                format!("mov{suffix}"),
                                format!("%{register},{location}"),
                            );
                            if name == "sum" {
                                asm = dbg!(asm);
                            }
                            ExpType::Type(asm, type_)
                        }
                        ExpType::Constant(constant) => {
                            let (register, suffix) = Self::gen_register_a(var_type);
                            let args = match constant {
                                Constant::Int(int) => format!("${int}"),
                            };
                            let mut asm = Asm::instruction(
                                format!("mov{suffix}"),
                                format!("{args},{location}"),
                            );
                            asm.append_instruction(format!("mov{suffix}"), format!("{args},%{register}"));
                            ExpType::Type(asm, var_type)
                        }
                    }
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
                if let Some((location, type_)) = optional_location {
                    let (register, suffix) = Self::gen_register_a(type_);
                    // Asm::instruction("pushq".into(), location.clone())
                    ExpType::Type(
                        Asm::instruction(format!("mov{suffix}"), format!("{location},%{register}")),
                        type_,
                    )
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
                let expression_type = self.gen_expression(*expression1, parent_scope);
                match expression_type {
                    ExpType::Constant(constant) => {
                        // In this case, we evaluate the constant, if it is 0, only generate the
                        // second case, if it is anything else only generate the first
                        let case1 = match constant {
                            Constant::Int(int) => {
                                if int == 0 {
                                    false
                                } else {
                                    true
                                }
                            }
                        };
                        if case1 {
                            // TODO: I made this with midnight brain probably wrong
                            // I'm very dumb, I was thinking of something super complicated but it
                            // was literally one line
                            self.gen_expression(*expression2, parent_scope)
                        } else {
                            self.gen_expression(*expression3, parent_scope)
                        }
                        // most of the time midnight brain is right
                        // % I ended here
                    }

                    ExpType::Type(mut asm, type_) => {
                        // asm.append_instruction("popq".into(), "%rax".into());
                        // TODO look tomorrow, midnight brain
                        let (register, suffix) = Self::gen_register_a(type_);
                        asm.append_instruction(format!("cmp{suffix}"), format!("$0,%{register}"));
                        asm.append_instruction("je".into(), else_name);
                        let left_expression = self.gen_expression(*expression2, parent_scope);

                        // TODO  PLEASE DON'T USE THIS WITHOUT PROOFREADING, I'M SERIOUS DON'T
                        let left_type = match left_expression {
                            ExpType::Type(inner_asm, type_) => {
                                asm.add_instructions(inner_asm);
                                type_
                            }
                            ExpType::Constant(constant) => {
                                let args = match constant {
                                    Constant::Int(int) => {
                                        ("movl".to_string(), format!("${int},%eax"))
                                    }
                                };
                                asm.append_instruction(args.0, args.1);
                                ""
                            }
                        };
                        asm.append_instruction("jmp".into(), endternary_name);
                        asm.append_instruction(else_label, String::new());
                        let right_expression = self.gen_expression(*expression3, parent_scope);
                        let right_type = match right_expression {
                            ExpType::Type(inner_asm, type_) => {
                                asm.add_instructions(inner_asm);
                                type_
                            }
                            ExpType::Constant(constant) => {
                                let args = match constant {
                                    Constant::Int(int) => {
                                        ("movl".to_string(), format!("${int},%eax"))
                                    }
                                };
                                asm.append_instruction(args.0, args.1);
                                ""
                            }
                        };
                        if left_type != right_type {
                            fail!("Two branches of ternary expression should have the same types, but got different left type {left_type:?} and right type {right_type:?}");
                        }
                        // asm.add_instructions(self.gen_expression(*expression3, parent_scope));
                        asm.append_instruction(endternary_label, String::new());
                        ExpType::Type(asm, left_type)
                    }

                }
            }
            ast::Expression::FunctionCall(name, arguments) => {
                // TODO: eval constant functions?

                if !self.function_table.contains_key(&name)
                    && !self.function_prototype_table.contains_key(&name)
                {
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
                // let dealloc_amt = 8 * (arguments.len());
                let mut dealloc_amt = 0;
                for (index, argument) in arguments.into_iter().enumerate()
                /* .rev() */
                {
                    let expression_type = self.gen_expression(argument, parent_scope);
                    let function_argument_type = self.function_table[&name][index].type_();
                    match expression_type {
                        ExpType::Type(expression, type_) => {
                            if type_ != function_argument_type {
                                fail!("Type of expression is not the same as type at definition of function {name}");
                            }
                            asm.add_instructions(expression);
                            let size = TYPE_SIZES[type_];
                            let (register, suffix) = Self::gen_register_a(type_);
                            asm.append_instruction(format!("subq"), format!("${size},%rsp"));
                            asm.append_instruction(
                                format!("mov{suffix}"),
                                format!("%{register},(%rsp)"),
                            );
                            dealloc_amt += size;
                        }
                        ExpType::Constant(constant) => {
                            let size = TYPE_SIZES[function_argument_type];
                            let (_, suffix) = Self::gen_register_a(function_argument_type);
                            asm.append_instruction(format!("subq"), format!("${size},%rsp"));
                            let value = match constant {
                                Constant::Int(int) => format!("${int}"),
                            };
                            asm.append_instruction(
                                format!("mov{suffix}"),
                                format!("{value},(%rsp)"),
                            );
                            dealloc_amt += size;
                        }
                    }
                    // TODO get arguments in registers instead of stack
                    // suppossedly this isn't needed because each result is pushed onto the
                    // stack... we will see
                }
                asm.append_instruction("call".to_string(), name.clone());
                // TODO when variables can have a differing size, change this into a for loop
                // to calculate the amount to deallocate
                asm.append_instruction("addq".to_string(), format!("${dealloc_amt},%rsp"));
                // return values of functions are generally stored in rax, so push them to the
                // top of the stack in order to ensure they are in the right place, maintain
                // top of stack invariant so to speak
                // asm.append_instruction("pushq".to_string(), format!("%rax"));
                ExpType::Type(asm, self.function_return_types[&name])
            }
        }
    }

    fn gen_binary_operation_expression(
        &mut self,
        binary_operator: BinaryOperator,
        left_expr: Box<Expression>,
        right_expr: Box<Expression>,
        parent_scope: u64,
    ) -> ExpType {
        match binary_operator {
            BinaryOperator::LogicalOr => {
                return self.gen_logical_or(left_expr, right_expr, parent_scope);
            }
            BinaryOperator::LogicalAnd => {
                return self.gen_logical_and(left_expr, right_expr, parent_scope);
            }
            _ => {}
        }

        let left_expr_type = self.gen_expression(*left_expr, parent_scope);
        let right_expr_type = self.gen_expression(*right_expr, parent_scope);

        match (left_expr_type, right_expr_type) {
            (ExpType::Constant(left_constant), ExpType::Constant(right_constant)) => {
                ExpType::Constant(Self::eval_binop(
                    binary_operator,
                    left_constant,
                    right_constant,
                ))
            }
            (ExpType::Constant(left_constant), ExpType::Type(right_asm, right_type)) => {
                Self::gen_binop_left_constant_right_expr(binary_operator, left_constant, right_type, right_asm)
            }
            (ExpType::Type(left_asm, left_type), ExpType::Constant(right_constant)) => {
                Self::gen_binop_left_expr_right_constant(binary_operator, right_constant, left_type, left_asm)
            }
            (ExpType::Type(mut left_asm, left_type), ExpType::Type(right_asm, right_type)) => {
                // TODO: binop may not work
                if right_type != left_type {
                    eprintln!("Types {right_type:?} and {left_type:?} do not match");
                }
                // TODO: the following will work for all input sizes, but might want to refactor to
                // use eax, ecx etc
                let (register_a, suffix_a) = Self::gen_register_a(left_type);
                let (register_c, suffix_c) = Self::gen_register_c(left_type);
                let register_sizes = TYPE_SIZES[left_type]; 

                // left_asm.append_instruction("push".into(), "%rax".into());
                left_asm.append_instruction("subq".into(), format!("${register_sizes},%rsp"));
                left_asm.append_instruction(format!("mov{suffix_a}"), format!("%{register_a},(%rsp)"));
                left_asm.add_instructions(right_asm);
                left_asm.append_instruction(format!("mov{suffix_c}"), format!("%{register_a},%{register_c}"));
                left_asm.append_instruction(format!("mov{suffix_a}"), format!("(%rsp),%{register_a}"));
                left_asm.append_instruction("addq".into(), format!("${register_sizes},%rsp"));
                // let (register, suffix) = Self::gen_register(left_type);
                // temporary :: TODO: make gen register work with rax, rcx,
                let binop = Self::binary_operation(&binary_operator, suffix_a /* This should be suffix, but rcx doesn't work with gen register for now, only rax*/);
                match binary_operator {
                    BinaryOperator::Divide => {
                        // TODO: division might not work because the upper bits won't be elimated
                        // for 32 bit math
                        left_asm.append_instruction("cqo".into(), String::new());
                        left_asm.append_instruction(binop, "%rcx".into());
                    }
                    BinaryOperator::Multiply => {
                        left_asm.append_instruction(binop, format!("%{register_c},%{register_a}"));
                        // TODO, part of the result will be in rdx
                    }
                    BinaryOperator::Modulo => {
                        left_asm.append_instruction("cqo".into(), String::new());
                        left_asm.append_instruction(binop, "%rcx".into());
                        left_asm.append_instruction("movq".into(), "%rdx,%rax".into());
                    }
                    // BinaryOperator::BitwiseLeftShift | BinaryOperator::BitwiseRightShift => {
                    //     left_asm.append_instruction(binop, "%rcx,%rax".into());
                    // }
                    BinaryOperator::Equal
                    | BinaryOperator::NotEqual
                    | BinaryOperator::GreaterEq
                    | BinaryOperator::Greater
                    | BinaryOperator::LessEq
                    | BinaryOperator::Less => {
                        left_asm.append_instruction(format!("cmp{suffix_c}"), format!("%{register_c},%{register_a}"));
                        left_asm.append_instruction("movq".into(), "$0,%rax".into());
                        left_asm.append_instruction(binop, "%al".into());
                    }
                    BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr => unreachable!(),
                    _ => {
                        left_asm.append_instruction(binop, format!("%{register_c},%{register_a}"));
                    }
                }
                ExpType::Type(left_asm, left_type)
            }
        }
    }
 
    fn gen_binop_left_constant_right_expr(
        binary_operator: BinaryOperator,
        constant: Constant,
        type_: &'static str,
        mut expr_asm: Asm,
    ) -> ExpType {
        // let (register, suffix) = Self::gen_register_a(type_);
        // TODO: generify this code at some point
        let exp1 = match constant {
            Constant::Int(i) => format!("${i}"),
        };
        let (register_a, suffix_a) = Self::gen_register_a(type_);
        let (register_c, suffix_c) = Self::gen_register_c(type_);
        let register_sizes = TYPE_SIZES[type_]; 
        let binop = Self::binary_operation(&binary_operator, suffix_a);
        match binary_operator {
            BinaryOperator::Equal
                | BinaryOperator::NotEqual
                | BinaryOperator::GreaterEq
                | BinaryOperator::Greater
                | BinaryOperator::LessEq
                | BinaryOperator::Less => {
/*
 * IMPORTANT: for assembly comparison order 
    movq $8, %rax
    movq $7, %rcx
    cmpq %rax, %rcx
    movq $0,%rax 
    setl %al
 * %al is 1 in this case, which means cmpq rax rcx compares rcx to rax
 */
                    expr_asm.append_instruction(format!("cmp{suffix_a}"), format!("{exp1},%{register_a}"));
                    expr_asm.append_instruction("mov".into(), "$0,%rax".into());
                    expr_asm.append_instruction(binop, "%al".into());
                }
            BinaryOperator::Divide => {
                // Divide is still rax, but this is a TODO: potential bug
                expr_asm.append_instruction("movq".into(), format!("%rax,%rcx"));
                expr_asm.append_instruction("movq".into(), format!("{exp1},%rax"));
                expr_asm.append_instruction("cqo".into(), String::new());
                expr_asm.append_instruction(binop, format!("%rcx"));
            }
            BinaryOperator::Modulo => {
                expr_asm.append_instruction("movq".into(), format!("%rax,%rcx"));
                expr_asm.append_instruction("movq".into(), format!("{exp1},%rax"));
                expr_asm.append_instruction("cqo".into(), String::new());
                expr_asm.append_instruction(binop, format!("%rcx"));
                expr_asm.append_instruction("movq".into(), format!("%rdx,%rax"));
            }
            _ => {
                expr_asm.append_instruction(
                    format!("{binop}"),
                    format!("{exp1},%{register_a}"),
                    );
            }
        }
        // (right_exp, right_type)
        // self.gen_right_expr_left_constant(right_expr, binary_operator, exp1, parent_scope)
        ExpType::Type(expr_asm, type_)
    }

    fn gen_binop_left_expr_right_constant(
        binary_operator: BinaryOperator,
        constant: Constant,
        type_: &'static str,
        mut expr_asm: Asm,
    ) -> ExpType {
        // TODO
        let (register_a, suffix_a) = Self::gen_register_a(type_);
        let (register_c, suffix_c) = Self::gen_register_c(type_);
        let register_sizes = TYPE_SIZES[type_]; 
        // TODO: generify this code at some point
        let exp1 = match constant {
            Constant::Int(i) => format!("${i}"),
        };
        let binop = Self::binary_operation(&binary_operator, suffix_a);
        match binary_operator {
            BinaryOperator::Equal
                | BinaryOperator::NotEqual
                | BinaryOperator::GreaterEq
                | BinaryOperator::Greater
                | BinaryOperator::LessEq
                | BinaryOperator::Less => {
                    expr_asm.append_instruction(format!("cmp{suffix_a}"), format!("{exp1},%{register_a}"));
                    expr_asm.append_instruction("mov".into(), "$0,%rax".into());
                    expr_asm.append_instruction(binop, "%al".into());
                }
            BinaryOperator::Divide => {
                // TODO divide still has the same problems as before
                expr_asm.append_instruction("cqo".into(), String::new());
                expr_asm.append_instruction("movq".into(), format!("{exp1},%rcx"));
                expr_asm.append_instruction(binop, format!("%rcx"));
            }
            BinaryOperator::Modulo => {
                expr_asm.append_instruction("cqo".into(), String::new());
                expr_asm.append_instruction("movq".into(), format!("{exp1},%rcx"));
                expr_asm.append_instruction(binop, format!("%rcx"));
                expr_asm.append_instruction(format!("movq"), format!("%rdx,%rax"))
            }
            _ => {
                expr_asm.append_instruction(
                    format!("{binop}"),
                    format!("{exp1},%{register_a}"),
                    );
            }
        }
        // (right_exp, right_type)
        // self.gen_right_expr_left_constant(right_expr, binary_operator, exp1, parent_scope)
        ExpType::Type(expr_asm, type_)
    }

    fn gen_logical_or(
        &mut self,
        left_expr: Box<Expression>,
        right_expr: Box<Expression>,
        parent_scope: u64,
    ) -> ExpType {
        let left_expression_type = self.gen_expression(*left_expr, parent_scope);
        let (second_condition, second_condition_label) = self.gen_new_label();
        let (post_or, post_or_label) = self.gen_new_label();
        let (mut asm, type_) = match left_expression_type {
            ExpType::Type(mut left_exp, left_type) => {
                // if left_type != "bool" {
                //     fail!("Cannot perform a logical or operation on `{left_type}`, mut be on type `bool`")
                // }
                let (register, suffix) = Self::gen_register_a(left_type);
                left_exp.append_instruction(format!("cmp{suffix}"), format!("$0,%{register}"));
                left_exp.append_instruction("je".into(), second_condition);
                // left_exp.append_instruction("movq".into(), "$1,(%rsp)".into());
                left_exp.append_instruction("mov".into(), "$1,%rax".into());
                left_exp.append_instruction("jmp".into(), post_or);
                left_exp.append_instruction(second_condition_label, String::new());
                (left_exp, left_type)
            }
            ExpType::Constant(left_constant) => {
                match left_constant {
                    Constant::Int(int) => if int == 0 {
                        let mut left_exp = Asm::default();
                        left_exp.append_instruction("mov".into(), "$0,%rax".into());
                        left_exp.append_instruction("jmp".into(), second_condition);
                        left_exp.append_instruction(second_condition_label, String::new());
                        (left_exp, TYPE_INT)
                    } else {
                        let mut left_exp = Asm::default();
                        left_exp.append_instruction("mov".into(), "$1,%rax".into());
                        left_exp.append_instruction("jmp".into(), post_or);
                        left_exp.append_instruction(second_condition_label, String::new());
                        (left_exp, TYPE_INT)
                    }
                }
            }
        };

        let right_expression_type = self.gen_expression(*right_expr, parent_scope);
        match right_expression_type {
            ExpType::Constant(constant) => {
                match constant {
                    Constant::Int(int) => if int == 0 {
                        asm.append_instruction(format!("movq"), format!("$0,%rax"));
                        asm.append_instruction(post_or_label, "".into());
                    } else {
                        asm.append_instruction(format!("movq"), format!("$1,%rax"));
                        asm.append_instruction(post_or_label, "".into());
                    }
                }
            }
            ExpType::Type(right_exp, right_type) => {
                asm.add_instructions(right_exp);
                // asm.append_instruction("cmpq".into(), "$0,(%rsp)".into());
                asm.append_instruction("cmp".into(), "$0,%rax".into());
                asm.append_instruction("mov".into(), "$0,%rax".into());
                asm.append_instruction("setne".into(), "%al".into());
                // asm.append_instruction("movq".into(), "%rax,(%rsp)".into());
                asm.append_instruction(post_or_label, "".into());
            }
        }
        ExpType::Type(asm, type_)   
    }

    fn gen_logical_and(
        &mut self,
        left_expr: Box<Expression>,
        right_expr: Box<Expression>,
        parent_scope: u64,
    ) -> ExpType {
        let left_expression_type = self.gen_expression(*left_expr, parent_scope);
        let (second_condition, second_condition_label) = self.gen_new_label();
        let (post_and, post_and_label) = self.gen_new_label();
        let (mut asm, type_) = match left_expression_type {
            ExpType::Type(mut left_exp, left_type) => {
                // if left_type != "bool" {
                //     fail!("Cannot perform a logical or operation on `{left_type}`, mut be on type `bool`")
                // }
                let (register, suffix) = Self::gen_register_a(left_type);
                left_exp.append_instruction(format!("cmp{suffix}"), format!("$0,%{register}"));
                left_exp.append_instruction("jne".into(), second_condition);
                left_exp.append_instruction("movq".into(), "$0,%rax".into());
                left_exp.append_instruction("jmp".into(), post_and);
                left_exp.append_instruction(second_condition_label, String::new());
                (left_exp, left_type)
            }
            ExpType::Constant(left_constant) => {
                match left_constant {
                    Constant::Int(int) => if int == 0 {
                        let mut left_exp = Asm::default();
                        left_exp.append_instruction("movq".into(), "$0,%rax".into());
                        left_exp.append_instruction("jmp".into(), post_and);
                        left_exp.append_instruction(second_condition_label, String::new());
                        (left_exp, TYPE_INT)
                    } else {
                        let mut left_exp = Asm::default();
                        left_exp.append_instruction("mov".into(), "$1,%rax".into());
                        left_exp.append_instruction("jmp".into(), second_condition);
                        left_exp.append_instruction(second_condition_label, String::new());
                        (left_exp, TYPE_INT)
                    }
                }
            }
        };

        let right_expression_type = self.gen_expression(*right_expr, parent_scope);
        match right_expression_type {
            ExpType::Constant(constant) => {
                match constant {
                    Constant::Int(int) => if int == 0 {
                        asm.append_instruction(format!("movq"), format!("$0,%rax"));
                        asm.append_instruction(post_and_label, "".into());
                    } else {
                        asm.append_instruction(format!("movq"), format!("$1,%rax"));
                        asm.append_instruction(post_and_label, "".into());
                    }
                }
            }
            ExpType::Type(right_exp, right_type) => {
                asm.add_instructions(right_exp);
                // asm.append_instruction("cmpq".into(), "$0,(%rsp)".into());
                asm.append_instruction("cmpq".into(), "$0,%rax".into());
                asm.append_instruction("movq".into(), "$0,%rax".into());
                asm.append_instruction("setne".into(), "%al".into());
                // asm.append_instruction("movq".into(), "%rax,(%rsp)".into());
                asm.append_instruction(post_and_label, "".into());
            }
        }
        ExpType::Type(asm, type_)   
    }

    // fn gen_two_expr(&mut self, binary_operator: BinaryOperator, left_expr: Box<Expression>, right_expr: Box<Expression>, parent_scope: u64) -> ExpType {
    // }

    // fn gen_two_constants(binary_operator: BinaryOperator, exp1: Constant, exp2: Constant) -> Constant {
    //     ExpType::Constant(Self::eval_binop(binary_operator, exp1, exp2)))
    // }

    // fn gen_right_expr_left_constant(&mut self, right_expr: Box<Expression>, binary_operator: BinaryOperator, exp1: Constant, parent_scope: u64) -> ExpType {
    //     let (mut right_exp, right_type) = self.gen_expression(*right_expr, parent_scope);
    //     match right_type {
    //         ExpType::Constant(exp2) => {
    //             (Asm::default(), ExpType::Constant(Self::eval_binop(binary_operator, exp1, exp2)))
    //         }
    //         ExpType::Type(right_type_inner) => {
    //             todo!()
    //         }
    //     }
    // }

    // fn gen_left_expr_right_constant(&mut self, binary_operator: BinaryOperator, left_expr: Box<Expression>, exp2: Constant, parent_scope: u64) -> ExpType {
    //     let left_exp = self.gen_expression(*left_expr, parent_scope);
    //     match left_exp {
    //         ExpType::Constant(exp1) => {
    //             ExpType::Constant(Self::eval_binop(binary_operator, exp1, exp2))
    //         }
    //         ExpType::Type(exp, left_type) => {
    //             let (register, suffix) = Self::gen_register(left_type);
    //             // TODO: generify this code at some point
    //             let arg = match exp2 {
    //                 Constant::Int(i) => format!("${i},%{register}"),
    //             };
    //             exp.append_instruction(Self::binary_operation(&binary_operator), arg);
    //             ExpType::Type(exp, left_type)
    //         }
    //     }
    // }

    fn eval_binop(binary_operator: BinaryOperator, exp1: Constant, exp2: Constant) -> Constant {
        match (exp1, exp2) {
            (Constant::Int(exp1), Constant::Int(exp2)) => {
                let result = match binary_operator {
                    BinaryOperator::Add => exp1 + exp2,
                    BinaryOperator::Minus => exp1 - exp2,
                    BinaryOperator::Multiply => exp1 * exp2,
                    BinaryOperator::Divide => exp1 / exp2,
                    BinaryOperator::Modulo => exp1 % exp2,
                    BinaryOperator::Xor => exp1 ^ exp2,
                    BinaryOperator::BitwiseOr => exp1 | exp2,
                    BinaryOperator::BitwiseAnd => exp1 & exp2,
                    BinaryOperator::BitwiseLeftShift => exp1 << exp2,
                    BinaryOperator::BitwiseRightShift => exp1 >> exp2,
                    // boolean stuffs
                    BinaryOperator::Less => {
                        if exp1 < exp2 {
                            1
                        } else {
                            0
                        }
                    }
                    BinaryOperator::Equal => {
                        if exp1 == exp2 {
                            1
                        } else {
                            0
                        }
                    }
                    BinaryOperator::LessEq => {
                        if exp1 <= exp2 {
                            1
                        } else {
                            0
                        }
                    }
                    BinaryOperator::GreaterEq => {
                        if exp1 >= exp2 {
                            1
                        } else {
                            0
                        }
                    }
                    BinaryOperator::Greater => {
                        if exp1 > exp2 {
                            1
                        } else {
                            0
                        }
                    }
                    BinaryOperator::NotEqual => {
                        if exp1 != exp2 {
                            1
                        } else {
                            0
                        }
                    }
                    BinaryOperator::LogicalOr | BinaryOperator::LogicalAnd => unreachable!(),
                };
                Constant::Int(result)
            }
        }
    }

    fn operation(
        &self,
        operator: ast::UnaryOperator,
        parent_scope: u64,
        type_: &'static str,
    ) -> Asm {
        self.eval_typed_unop(operator, parent_scope, type_)
        // Note the following might be a bit of a premature optimization. For all the cases,
        // instead of pushing/popping the values off the stack into the ecx register, I instead
        // copy them into a register then operate on them, then write to a register, minimizing
        // costly memory operations
        // Note for all of these, rcx might be overwritten so in the future, might need to copy out the
        // values in rcx
    }

    fn eval_typed_unop(
        &self,
        operator: ast::UnaryOperator,
        parent_scope: u64,
        type_: &'static str,
    ) -> Asm {
        let (register, suffix) = Self::gen_register_a(type_);
        match operator {
            ast::UnaryOperator::Negation => {
                Asm::instruction(format!("neg{suffix}"), format!("%{register}"))
            }
            ast::UnaryOperator::BitwiseComplement => {
                Asm::instruction(format!("not{suffix}"), format!("%{register}"))
            }
            ast::UnaryOperator::LogicalNegation => Asm::instructions(vec![
                AsmInstr::new(format!("cmp{suffix}"), format!("$0,%{register}")),
                AsmInstr::new(format!("mov{suffix}"), format!("$0,%{register}")),
                AsmInstr::from("sete", "%al"),
                // AsmInstr::from("movq", "%rax,(%rsp)"),
            ]),
            ast::UnaryOperator::PostfixIncrement(name) => {
                let (location, _) = self.symbol_table.get(&name, parent_scope).expect(&format!(
                    "Variable {} in scope '{} accessed but not declared",
                    name, parent_scope
                ));
                Asm::instructions(vec![
                    // AsmInstr::new("pushq".into(), format!("{location}")),
                    AsmInstr::new(format!("mov{suffix}"), format!("{location},%{register}")),
                    AsmInstr::new(format!("add{suffix}"), format!("$1,{location}")),
                ])
            }
            ast::UnaryOperator::PrefixIncrement(name) => {
                let (location, _) = self.symbol_table.get(&name, parent_scope).expect(&format!(
                    "Variable {} in scope '{} accessed but not declared",
                    name, parent_scope
                ));
                Asm::instructions(vec![
                    AsmInstr::new(format!("add{suffix}"), format!("$1,{location}")),
                    AsmInstr::new(format!("mov{suffix}"), format!("{location},%{register}")),
                ])
            }
            ast::UnaryOperator::PrefixDecrement(name) => {
                let (location, _) = self.symbol_table.get(&name, parent_scope).expect(&format!(
                    "Variable {} in scope '{} accessed but not declared",
                    name, parent_scope
                ));
                Asm::instructions(vec![
                    AsmInstr::new(format!("sub{suffix}"), format!("$1,{location}")),
                    AsmInstr::new(format!("mov{suffix}"), format!("{location},%{register}")),
                ])
            }
            ast::UnaryOperator::PostfixDecrement(name) => {
                let (location, _) = self.symbol_table.get(&name, parent_scope).expect(&format!(
                    "Variable {} in scope '{} accessed but not declared",
                    name, parent_scope
                ));
                Asm::instructions(vec![
                    AsmInstr::new(format!("mov{suffix}"), format!("{location},%{register}")),
                    AsmInstr::new(format!("sub{suffix}"), format!("$1,{location}")),
                ])
            }
            _ => unreachable!(),
            // format!("cmpl $0, {location}\nsete {location}"),
        }
    }

    fn eval_unary_op(operator: ast::UnaryOperator, value: Constant) -> Constant {
        match operator {
            ast::UnaryOperator::Negation => match value {
                Constant::Int(int) => Constant::Int(-int),
            },
            ast::UnaryOperator::LogicalNegation => match value {
                Constant::Int(int) => {
                    let result = if int == 0 { 1 } else { 0 };
                    Constant::Int(result)
                }
            },
            ast::UnaryOperator::BitwiseComplement => {
                match value {
                    // ! for ints is the same as ~ in C
                    Constant::Int(int) => Constant::Int(!int),
                }
            }
            _ => unreachable!(),
        }
    }

    // generates the binary operation with the suffix
    fn binary_operation(operator: &ast::BinaryOperator, suffix: char) -> String {
        match operator {
            ast::BinaryOperator::Add => format!("add{suffix}"),
            ast::BinaryOperator::Minus => format!("sub{suffix}"),
            ast::BinaryOperator::Multiply => format!("imul{suffix}"),
            ast::BinaryOperator::Divide => format!("idiv"),
            ast::BinaryOperator::Modulo => format!("idiv"),
            ast::BinaryOperator::Xor => format!("xor{suffix}"),
            ast::BinaryOperator::BitwiseAnd => format!("and{suffix}"),
            ast::BinaryOperator::BitwiseOr => format!("or{suffix}"),
            ast::BinaryOperator::BitwiseRightShift => format!("sar{suffix}"),
            ast::BinaryOperator::BitwiseLeftShift => format!("sal{suffix}"),
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

    // returns the (register, suffix)
    fn gen_register_a(type_: &'static str) -> (&str, char) {
        match type_ {
            TYPE_LONG => ("rax", 'q'),
            TYPE_INT => ("eax", 'l'),
            _ => todo!(),
        }
    }

    // returns the (register, suffix)
    fn gen_register_c(type_: &'static str) -> (&str, char) {
        match type_ {
            TYPE_LONG => ("rcx", 'q'),
            TYPE_INT => ("ecx", 'l'),
            _ => todo!(),
        }
    }
}
