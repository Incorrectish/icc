use std::collections::HashSet;

use lazy_static::lazy_static;
use regex::Regex;

// Compiles the regular expressions once at the start of the program for use anywhere
lazy_static! {
    pub static ref IDENTIFIER_REGEX: Regex = Regex::new(r"^[a-zA-Z]\w*$").unwrap();
    pub static ref INTEGER_LITERAL_REGEX: Regex = Regex::new(r"^[0-9]+$").unwrap();
    pub static ref TYPES: HashSet<&'static str> =
        vec!["int", "bool", "char", "long"].into_iter().collect();
}

// #[allow(unused)]
// These are all the types of tokens so far
#[derive(Debug, Clone)]
pub enum Token {
    OpenBrace,                        // {
    CloseBrace,                       // }
    OpenParen,                        // (
    CloseParen,                       // )
    Semicolon,                        // ;
    KeywordType(&'static str),        // int
    KeywordIf,                        // if
    KeywordElse,                      // else
    KeywordReturn,                    // return
    KeywordDo,                        // do
    KeywordFor,                       // for
    KeywordBreak,                     // break
    KeywordWhile,                     // while
    KeywordContinue,                  // continue
    Identifier(String), // [a-zA-Z]\w+
    IntegerLiteral(String),           // [0-9]+
    BitwiseComplement,                // ~
    LogicalNot,                       // !
    Minus,                            // -
    Add,                              // +
    Multiply,                         // *
    Divide,                           // /
    Modulo,                           // %
    BitwiseAnd,                       // &
    BitwiseOr,                        // |
    Xor,                              // ^
    GreaterThan,                      // >
    LessThan,                         // <
    Assign,                           // =
    Colon,                            // :
    Question,                         //?
    Comma,                            // ,
    PrefixIncrement(String),          // ++
    PostfixIncrement(String),         // ++
    PrefixDecrement(String),          // --
    PostfixDecrement(String),         // --
    AddAssign,                        // +=
    MinusAssign,                      // -=
    MulAssign,                        // *=
    DivAssign,                        // /=
    ModAssign,                        // %=
    XorAssign,                        // ^=
    BitwiseAndAssign,                 // &=
    BitwiseOrAssign,                  // |=
    BitwiseLeftShiftAssign,           // <<=
    BitwiseRightShiftAssign,          // >>=
    LogicalAnd,                       // &&
    LogicalOr,                        // ||
    EqualTo,                          // ==
    GreaterThanOrEqualTo,             // >=
    LessThanOrEqualTo,                // <=
    NotEqual,                         // !=
    BitwiseLeftShift,                 // <<
    BitwiseRightShift,                // >>
}
