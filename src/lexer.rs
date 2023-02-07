use crate::token::{Token, IDENTIFIER_REGEX, INTEGER_LITERAL_REGEX};
use std::mem;

#[derive(Debug, Clone)]
pub struct Lexer {
    pos: usize,
    input: String,
    curr_substr: String,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Self {
            pos: 0,
            input,
            curr_substr: String::new(),
        }
    }

    fn consume(&mut self) -> Option<Token> {
        let curr_substr = &mem::take(&mut self.curr_substr) as &str;
        match curr_substr {
            "int" => Some(Token::KeywordInt),
            "return" => Some(Token::KeywordReturn),
            _ if INTEGER_LITERAL_REGEX.is_match(curr_substr) => Some(Token::IntegerLiteral(curr_substr.to_string())),
            _ if IDENTIFIER_REGEX.is_match(curr_substr) => Some(Token::Identifier(curr_substr.to_string())),
            _ => {
                unreachable!("Invalid token: `{curr_substr}`");
            }
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        if self.pos >= self.input.len() {
            return None;
        }
        let curr_char = self.input.as_bytes()[self.pos] as char;
        self.pos += 1;
        match curr_char {
            '{' => Some(Token::OpenBrace),
            '}' => Some(Token::CloseBrace),
            '(' => Some(Token::OpenParen),
            ')' => Some(Token::CloseParen),
            ';' => Some(Token::Semicolon),
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

    fn consume_until_next_token(&mut self) {
        loop {
            if self.pos >= self.input.len() {
                break;
            }
            let curr_char = self.input.as_bytes()[self.pos] as char;
            if curr_char.is_whitespace()
                || Self::is_token(curr_char)
            {
                break;
            }
            self.curr_substr.push(curr_char);
            self.pos += 1;
        }
    }

    fn is_token(curr_char: char) -> bool {
        matches!(curr_char, '{' | '}' | '(' | ')' | ';')
    }

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
        self.next_token()
    }
}
