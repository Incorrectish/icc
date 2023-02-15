use crate::token::{Token, IDENTIFIER_REGEX, INTEGER_LITERAL_REGEX};
use std::mem;

// The struct simply transforms an input file into a token stream
#[derive(Debug, Clone)]
pub struct Lexer {
    pos: usize,
    input: String,
    curr_substr: String,
    curr_token: Option<Token>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Self {
            pos: 0,
            input,
            curr_substr: String::new(),
            curr_token: None,
        };
        lexer.curr_token = lexer.next_token();
        lexer
    }

    // This transforms the current substr into it's corresponding token
    fn consume(&mut self) -> Option<Token> {
        let curr_substr = &mem::take(&mut self.curr_substr) as &str;
        match curr_substr {
            "int" => Some(Token::KeywordInt),
            "return" => Some(Token::KeywordReturn),
            _ if INTEGER_LITERAL_REGEX.is_match(curr_substr) => {
                Some(Token::IntegerLiteral(curr_substr.to_string()))
            }
            _ if IDENTIFIER_REGEX.is_match(curr_substr) => {
                Some(Token::Identifier(curr_substr.to_string()))
            }
            _ => {
                unreachable!("Invalid token: `{curr_substr}`");
            }
        }
    }

    // This gets the next token
    fn next_token(&mut self) -> Option<Token> {
        if self.pos >= self.input.len() {
            return None;
        }
        let curr_char = self.input.as_bytes()[self.pos] as char;
        self.pos += 1;
        let next_char = self.input.as_bytes().get(self.pos);
        match curr_char {
            '{' => Some(Token::OpenBrace),
            '}' => Some(Token::CloseBrace),
            '(' => Some(Token::OpenParen),
            ')' => Some(Token::CloseParen),
            ';' => Some(Token::Semicolon),
            '~' => Some(Token::BitwiseComplement),
            ',' => Some(Token::Comma),
            ':' => Some(Token::Colon),
            '?' => Some(Token::Question),
            '-' => {
                if next_char == Some(&('=' as u8)) {
                    self.pos += 1;
                    Some(Token::MinusAssign)
                } else {
                    Some(Token::Minus)
                }
            }
            '%' => {
                if next_char == Some(&('=' as u8)) {
                    self.pos += 1;
                    Some(Token::ModAssign)
                } else {
                    Some(Token::Modulo)
                }
            }
            '+' => {
                if next_char == Some(&('=' as u8)) {
                    self.pos += 1;
                    Some(Token::AddAssign)
                } else {
                    Some(Token::Add)
                }
            }
            '*' => {
                if next_char == Some(&('=' as u8)) {
                    self.pos += 1;
                    Some(Token::MulAssign)
                } else {
                    Some(Token::Multiply)
                }
            }
            '/' => {
                if next_char == Some(&('=' as u8)) {
                    self.pos += 1;
                    Some(Token::DivAssign)
                } else {
                    Some(Token::Divide)
                }
            }
            '^' => {
                if next_char == Some(&('=' as u8)) {
                    self.pos += 1;
                    Some(Token::XorAssign)
                } else {
                    Some(Token::Xor)
                }
            }
            '!' => {
                if next_char == Some(&('=' as u8)) {
                    self.pos += 1;
                    Some(Token::NotEqual)
                } else {
                    Some(Token::LogicalNot)
                }
            }
            '&' => {
                if next_char == Some(&('&' as u8)) {
                    self.pos += 1;
                    Some(Token::LogicalAnd)
                } else if next_char == Some(&('=' as u8)) {
                    self.pos += 1;
                    Some(Token::BitwiseAndAssign)
                } else {
                    Some(Token::BitwiseAnd)
                }
            }
            '|' => {
                if next_char == Some(&('|' as u8)) {
                    self.pos += 1;
                    Some(Token::LogicalOr)
                } else if next_char == Some(&('=' as u8)) {
                    self.pos += 1;
                    Some(Token::BitwiseOrAssign)
                } else {
                    Some(Token::BitwiseOr)
                }
            }
            '>' => {
                if next_char == Some(&('=' as u8)) {
                    self.pos += 1;
                    Some(Token::GreaterEq)
                } else if next_char == Some(&('>' as u8)) {
                    self.pos += 1;
                    let third_char = self.input.as_bytes().get(self.pos);
                    if third_char == Some(&('=' as u8)) {
                        self.pos += 1;
                        Some(Token::BitwiseRightShiftAssign)
                    } else {
                        Some(Token::BitwiseRightShift)
                    }
                } else {
                    Some(Token::Greater)
                }
            }
            '<' => {
                if next_char == Some(&('=' as u8)) {
                    self.pos += 1;
                    Some(Token::LessEq)
                } else if next_char == Some(&('<' as u8)) {
                    self.pos += 1;
                    let third_char = self.input.as_bytes().get(self.pos);
                    if third_char == Some(&('=' as u8)) {
                        self.pos += 1;
                        Some(Token::BitwiseLeftShiftAssign)
                    } else {
                        Some(Token::BitwiseLeftShift)
                    }
                } else {
                    Some(Token::Less)
                }
            }
            '=' => {
                if next_char == Some(&('=' as u8)) {
                    self.pos += 1;
                    Some(Token::Equal)
                } else {
                    Some(Token::Assign)
                }
            }
            _ if curr_char.is_whitespace() => {
                self.skip_whitespace();
                self.next_token()
            }
            _ => {
                self.curr_substr.push(curr_char);
                self.consume_until_next_token();
                self.consume()
            }
        }
    }

    // This reads the string until the next token or whitespace is reached, and adds that to the
    // current substring
    fn consume_until_next_token(&mut self) {
        loop {
            if self.pos >= self.input.len() {
                break;
            }
            let curr_char = self.input.as_bytes()[self.pos] as char;
            if curr_char.is_whitespace() || Self::is_token(curr_char) {
                break;
            }
            self.curr_substr.push(curr_char);
            self.pos += 1;
        }
    }

    pub fn peek(&mut self) -> Option<Token> {
        self.curr_token.clone()
    }

    // Checks if a character matches any of the single character tokens to ensure they don't show
    // up in the multi character tokens
    fn is_token(curr_char: char) -> bool {
        matches!(
            curr_char,
            '{' | '}'
                | '('
                | ')'
                | ';'
                | '!'
                | '~'
                | '-'
                | '+'
                | '*'
                | '/'
                | '%'
                | '>'
                | '='
                | '<'
                | '^'
                | '&'
                | '|'
                | ','
                | ':'
                | '?'
        )
    }

    // Moves position from `pos` to the next non whitespace character
    fn skip_whitespace(&mut self) {
        while self.pos < self.input.len()
            && (self.input.as_bytes()[self.pos] as char).is_whitespace()
        {
            self.pos += 1;
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let temp = self.curr_token.take();
        self.curr_token = self.next_token();
        temp
    }
}
