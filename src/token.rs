use lazy_static::lazy_static;
use regex::Regex;

// Compiles the regular expressions once at the start of the program for use anywhere
lazy_static! {
    pub static ref IDENTIFIER_REGEX: Regex = Regex::new(r"^[a-zA-Z]\w*$").unwrap();
    pub static ref INTEGER_LITERAL_REGEX: Regex = Regex::new(r"^[0-9]+$").unwrap();
}

// These are all the types of tokens so far
#[derive(Debug, Clone)]
pub enum Token {
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    Semicolon,
    KeywordInt,
    KeywordReturn,
    Identifier(String),
    IntegerLiteral(String),
    Negation,
    BitwiseComplement,
    LogicalNegation,
}
