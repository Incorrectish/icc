use crate::{lexer::Lexer, parser::Parser};
use ::std::env;

mod lexer;
mod token;
mod ast;
mod parser;

fn main() {
    // Command args contain the binary directory relative path as the first argument, so the second
    // argument will be the filename given to the compiler
    let filename = env::args().nth(1).expect("Please provide a file to lex");
    parse(filename);
    // println!("{:?}", parse(filename));
}

fn parse(filename: String) {
    let file_string = std::fs::read_to_string(filename).expect("Couldn't open file");
    Parser::new(Lexer::new(file_string)).parse().print();
}
