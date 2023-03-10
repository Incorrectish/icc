use std::{collections::VecDeque, fs, io::Write, mem, path::Path};

#[repr(transparent)]
#[derive(Debug)]
pub struct Asm {
    instructions: VecDeque<AsmInstr>,
}

#[derive(Debug)]
pub struct AsmInstr {
    command: String,
    arguments: String,
}

impl Default for AsmInstr {
    fn default() -> Self {
        Self {
            command: String::new(),
            arguments: String::new(),
        }
    }
}

impl AsmInstr {
    pub fn new(command: String, arguments: String) -> Self {
        Self { command, arguments }
    }

    pub fn from(command: &str, arguments: &str) -> Self {
        Self {
            command: command.to_string(),
            arguments: arguments.to_string(),
        }
    }

    pub fn command(&self) -> &str {
        &self.command
    }

    pub fn print(&self) {
        print!("{} {}", self.command, self.arguments);
    }
}

impl Default for Asm {
    fn default() -> Self {
        Asm {
            instructions: VecDeque::new(),
        }
    }
}

impl Asm {
    pub fn print(&self) {
        println!();
        for instr in &self.instructions {
            instr.print();
            println!();
        }
        println!();
    }

    pub fn add_instr_two_before_end(&mut self, assembly: Asm) {
        let end = self
            .instructions
            .remove(self.instructions.len() - 1)
            .expect("Should have these elements :(");
        let end2 = self
            .instructions
            .remove(self.instructions.len() - 1)
            .expect("Should have these elements :(");
        self.add_instructions(assembly);
        self.instructions.push_back(end2);
        self.instructions.push_back(end);
    }

    pub fn from_instr(instructions: Vec<AsmInstr>, mut assembly: Asm) -> Self {
        let mut result = Asm {
            instructions: mem::take(&mut assembly.instructions),
        };

        // source of potential errors
        for instruction in instructions {
            result.instructions.push_front(instruction);
        }

        result
    }

    pub fn new(mut assembly: Asm, instructions: Vec<AsmInstr>) -> Self {
        let mut result = Asm {
            instructions: mem::take(&mut assembly.instructions),
        };

        // source of potential errors
        for instruction in instructions {
            result.instructions.push_back(instruction);
        }

        result
    }

    pub fn instructions(instructions: Vec<AsmInstr>) -> Self {
        Self {
            instructions: VecDeque::from(instructions),
        }
    }

    pub fn instruction(command: String, arguments: String) -> Self {
        let mut assembly = Self {
            instructions: VecDeque::with_capacity(1),
            // VecDeque::from(AssemblyInstruction::new(command, arguments)),
        };
        assembly
            .instructions
            .push_back(AsmInstr { command, arguments });
        assembly
    }

    pub fn last(&self) -> Option<&AsmInstr> {
        if self.instructions.len() > 0 {
            Some(&self.instructions[self.instructions.len() - 1])
        } else {
            None
        }
    }

    pub fn write(mut assembly: Asm, to: &Path) -> std::io::Result<()> {
        assembly.optimize_push_pop();
        assembly.optimize_useless_moves();
        let mut file = fs::File::create(to)?;
        for instruction in &assembly.instructions {
            let Ok(_) = file.write_all(
                format!("{} {}\n", instruction.command, instruction.arguments).as_bytes(),
            ) else {
                panic!("Couldn't write to assembly file");
            };
        }
        Ok(())
    }

    // TODO: two potential optimizations, check if there is something like push, mov, pop, that can
    // be optimized to two moves
    // In addition, if you see a movq ${int},{register}
    // then immediately binop {register},{location}, you could optimize that to binop
    // ${int},{location}
    // Can also optimize double moves:
    // movq $3,%rax
    // movq %rax,-8(%rbp)

    pub fn add_instructions(&mut self, mut more_instructions: Asm) {
        self.instructions
            .append(&mut mem::take(&mut more_instructions.instructions));
    }

    pub fn append_instruction(&mut self, command: String, arguments: String) {
        self.instructions.push_back(AsmInstr { command, arguments })
    }

    #[allow(unused)]
    pub fn prepend_instruction(&mut self, command: String, arguments: String) {
        self.instructions
            .push_front(AsmInstr { command, arguments })
    }

    // Removes any pushes immediately followed by a pop
    fn optimize_push_pop(&mut self) {
        // The entire following function assumes no command has whitespace after it. eg: it breaks
        // if the command is "pushq " vs "pushq", which is intentional
        let mut last_push = false;
        // TODO: make this an option
        let mut postfix = '\0';
        for i in (0..self.instructions.len()).rev() {
            let instruction = &self.instructions[i];
            if instruction.command.starts_with("pop") {
                last_push = true;
                postfix = instruction.command.as_bytes()[instruction.command.len() - 1] as char;
            } else if last_push && instruction.command.starts_with("push") {
                let register = mem::take(&mut self.instructions[i + 1].arguments);
                self.instructions[i].command = format!("mov{postfix}");
                self.instructions[i].arguments =
                    format!("{},{register}", self.instructions[i].arguments);
                self.instructions.remove(i + 1);
                last_push = false;
            } else {
                last_push = false;
            }
        }
    }

    #[allow(unused)]
    // TODO: This function turns the instruction at index2 into new_command arg1,arg2 then removes
    // index1
    // for example if instruction at index1 is `pushl $5`, instruction at index2 is `popl %eax`,
    // and new_command is `movq`, index1 will be removed, and the instruction at index2 becomes
    // `movq $5, %rax`
    // THIS WILL BREAK IF THE ARGUMENT FOR one of them is something like `value, location`, IT
    // CANNOT HANDLE ARGUMENTS THAT HAVE > 1 arg
    // This also assumes index1 and index2 are in bounds, it will panic if it is out of bounds
    fn combine(&mut self, index1: usize, index2: usize, new_command: String) {
        let instruction1 = self
            .instructions
            .remove(index1)
            .expect("index1 MUST be in bounds");
        let instruction2 = &self.instructions[index2];
        self.instructions[index2] = AsmInstr::new(
            new_command,
            format!("{},{}", instruction1.arguments, instruction2.arguments),
        );
    }

    fn optimize_useless_moves(&mut self) {
        // This entire loop simply goes through and removes any movq %reg,%reg
        for i in (0..self.instructions.len()).rev() {
            let instruction = &self.instructions[i];
            if instruction.command.starts_with("mov") {
                if let [arg1, arg2] =
                    instruction.arguments.clone().split(",").collect::<Vec<_>>()[..]
                {
                    if arg1 == arg2 {
                        self.instructions.remove(i);
                    }
                }
            }
        }
    }
}
