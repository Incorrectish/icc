use std::{collections::VecDeque, fs, io::Write, path::Path};

#[repr(transparent)]
pub struct Assembly {
    instructions: VecDeque<AssemblyInstruction>,
}

pub struct AssemblyInstruction {
    command: String,
    arguments: String,
}

impl Assembly {
    fn write(assembly: Assembly, to: &Path) -> std::io::Result<()> {
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
}
