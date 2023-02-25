use lazy_static::lazy_static;
use regex::Regex;

// Compiles the regular expressions once at the start of the program for use anywhere
lazy_static! {
    pub static ref IDENTIFIER_REGEX: Regex = Regex::new(r"^[a-zA-Z]\w*$").unwrap();
    pub static ref INTEGER_LITERAL_REGEX: Regex = Regex::new(r"^[0-9]+$").unwrap();
}

// #[allow(unused)]
// These are all the types of tokens so far
#[derive(Debug, Clone)]
pub enum Token {
    OpenBrace,                // {
    CloseBrace,               // }
    OpenParen,                // (
    CloseParen,               // )
    Semicolon,                // ;
    KeywordInt,               // int
    KeywordIf,                // if
    KeywordElse,              // else
    KeywordReturn,            // return
    KeywordDo,                // do
    KeywordFor,               // for
    KeywordBreak,             // break
    KeywordWhile,             // while
    KeywordContinue,          // continue
    Identifier(String),       // [a-zA-Z]\w+
    IntegerLiteral(String),   // [0-9]+
    BitwiseComplement,        // ~
    LogicalNot,               // !
    Minus,                    // -
    Add,                      // +
    Multiply,                 // *
    Divide,                   // /
    Modulo,                   // %
    BitwiseAnd,               // &
    BitwiseOr,                // |
    Xor,                      // ^
    Greater,                  // >
    Less,                     // <
    Assign,                   // =
    Colon,                    // :
    Question,                 //?
    Comma,                    // ,
    PrefixIncrement(String),  // ++
    PostfixIncrement(String), // ++
    PrefixDecrement(String),  // --
    PostfixDecrement(String), // --
    AddAssign,                // +=
    MinusAssign,              // -=
    MulAssign,                // *=
    DivAssign,                // /=
    ModAssign,                // %=
    XorAssign,                // ^=
    BitwiseAndAssign,         // &=
    BitwiseOrAssign,          // |=
    BitwiseLeftShiftAssign,   // <<=
    BitwiseRightShiftAssign,  // >>=
    LogicalAnd,               // &&
    LogicalOr,                // ||
    Equal,                    // ==
    GreaterEq,                // >=
    LessEq,                   // <=
    NotEqual,                 // !=
    BitwiseLeftShift,         // <<
    BitwiseRightShift,        // >>
}
