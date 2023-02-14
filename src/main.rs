use crate::{asm_gen::AsmGenerator, assembly::*, lexer::Lexer, parser::Parser};
use std::{env, fs, path::Path, process::Command};

mod asm_gen;
mod assembly;
mod ast;
mod lexer;
mod parser;
mod symbol_table;
mod token;

fn main() {
    // Command args contain the binary directory relative path as the first argument, so the second
    // argument will be the filename given to the compiler
    let filename = env::args().nth(1).expect("Please provide a file to lex");
    // test_lexer(Path::new(&filename));
    // test_ast(Path::new(&filename));
    compile(Path::new(&filename));
}

fn compile(filename: &Path) {
    let file_string = fs::read_to_string(&filename).expect("Couldn't open file");
    // This is getting the file name without the extension

    // Generating the abstract syntax tree, and the output assembly from it
    let ast = Parser::new(Lexer::new(file_string)).parse();
    let asm = AsmGenerator::new().generate(ast);

    let asm_file = filename.with_extension("s");
    let Ok(_) = Asm::write(asm, &asm_file) else {panic!("Could not write to assembly file")};

    let parent_path = filename.parent().unwrap_or(Path::new(""));
    dbg!(Command::new("gcc")
        .arg(&asm_file)
        .arg("-o")
        .arg(parent_path.join("out"))
        .output()
        .expect("Failed to run assembler"));

    // fs::remove_file(&asm_file).expect("Failed to delete file");
}

fn test_lexer(filename: &Path) {
    let file_string = fs::read_to_string(filename).expect("Couldn't open file");
    println!("{file_string}");
    dbg!(Lexer::new(file_string).collect::<Vec<_>>());
}

fn test_ast(filename: &Path) {
    let file_string = fs::read_to_string(filename).expect("Couldn't open file");
    println!("{file_string}");
    Parser::new(Lexer::new(file_string)).parse().print();
}
