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
        assembly.instructions.push_back(AsmInstr { command, arguments });
        assembly
    }

    pub fn write(assembly: Asm, to: &Path) -> std::io::Result<()> {
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

    pub fn add_instructions(&mut self, mut more_instructions: Asm) {
        self.instructions
            .append(&mut mem::take(&mut more_instructions.instructions));
    }

    pub fn append_instruction(&mut self, command: String, arguments: String) {
        self.instructions
            .push_back(AsmInstr { command, arguments })
    }

    pub fn prepend_instruction(&mut self, command: String, arguments: String) {
        self.instructions
            .push_front(AsmInstr { command, arguments })
    }
}
