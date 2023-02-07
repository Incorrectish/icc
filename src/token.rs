use regex::Regex;

use lazy_static::lazy_static;

// pub const TOKEN_VARIANTS: [Token; 9] = [
//     OpenBrace,
//     CloseBrace,
//     OpenParen,
//     CloseParen,
//     Semicolon,
//     KeywordInt,
//     KeywordReturn,
//     Identifier,
//     IntegerLiteral,
// ];

lazy_static! {
    pub static ref IDENTIFIER_REGEX: Regex = Regex::new(r"^[a-zA-Z]\w*$").unwrap();
    pub static ref INTEGER_LITERAL_REGEX: Regex = Regex::new(r"^[0-9]+$").unwrap();
}

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
}

// pub const fn from_token(token: Token) -> &'static str {
//     match token {
//         Token::OpenBrace => r"\{",
//         Token::CloseBrace => r"\}",
//         Token::OpenParen => r"\(",
//         Token::CloseParen => r"\)",
//         Token::Semicolon => r";",
//         Token::KeywordInt => r"int",
//         Token::KeywordReturn => r"return",
//         Token::Identifier => r"[a-zA-Z]\w*",
//         Token::IntegerLiteral => r"[0-9]+",
//     }
// }
