use crate::{lexer::Lexer, parser::Parser};
use ::std::env;
use std::{
    fs::{self, write},
    process::Command,
};

mod ast;
mod lexer;
mod parser;
mod token;

fn main() {
    // Command args contain the binary directory relative path as the first argument, so the second
    // argument will be the filename given to the compiler
    let filename = env::args().nth(1).expect("Please provide a file to lex");
    // test_lexer(filename);
    test_ast(filename);
    // compile(filename);
}

fn compile(filename: String) {
    let file_string = std::fs::read_to_string(&filename).expect("Couldn't open file");
    // This is getting the file name without the extension
    let clone = filename;
    let parts = clone.split('.').collect::<Vec<_>>();
    let name = parts[0..parts.len() - 1].join(".");

    // Generating the abstract syntax tree, and the output assembly from it
    let ast = Parser::new(Lexer::new(file_string)).parse();
    let asm = Parser::generate(ast);

    let asm_file = format!("{name}.s");
    write(&asm_file, asm).expect("Failed to write to the assembly file");

    Command::new("gcc")
        .arg(&asm_file)
        .arg("-o")
        .arg("out")
        .output()
        .expect("Failed to run assembler");

    fs::remove_file(&asm_file).expect("Failed to delete file");
}

fn test_lexer(filename: String) {
    let file_string = std::fs::read_to_string(&filename).expect("Couldn't open file");
    println!("{file_string}");
    dbg!(Lexer::new(file_string).collect::<Vec<_>>());
}

fn test_ast(filename: String) {
    let file_string = std::fs::read_to_string(&filename).expect("Couldn't open file");
    println!("{file_string}");
    Parser::new(Lexer::new(file_string)).parse().print();
}
