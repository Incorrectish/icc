use std::{collections::VecDeque, fs, io::Write, mem, path::Path};

#[repr(transparent)]
pub struct Asm {
    instructions: VecDeque<AsmInstr>,
}

pub struct AsmInstr {
    command: String,
    arguments: String,
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
}

impl Asm {
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

    pub fn add_instructions(&mut self, mut more_instructions: Asm) {
        self.instructions
            .append(&mut mem::take(&mut more_instructions.instructions));
    }

    pub fn append_instruction(&mut self, command: String, arguments: String) {
        self.instructions.push_back(AsmInstr { command, arguments })
    }

    pub fn prepend_instruction(&mut self, command: String, arguments: String) {
        self.instructions
            .push_front(AsmInstr { command, arguments })
    }

    // Removes any pushes immediately followed by a pop
    fn optimize_push_pop(&mut self) {
        // The entire following function assumes no command has whitespace after it. eg: it breaks
        // if the command is "pushq " vs "pushq", which is intentional
        let mut last_push = false;
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

    // TODO: This function turns the instruction at index2 into new_command arg1,arg2 then removes
    // index1
    fn combine(&mut self, index1: usize, index2: usize, new_command: String) {}

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
