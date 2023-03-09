use crate::{
    fail,
    token::{Token, IDENTIFIER_REGEX, INTEGER_LITERAL_REGEX},
};
use std::mem;

// The struct simply transforms an input file into a token stream
#[derive(Debug, Clone)]
pub struct Lexer {
    prev_prev_pos: usize,
    prev_pos: usize,
    curr_pos: usize,
    prev_prev_line_number: usize,
    prev_line_number: usize,
    line_number: usize,
    input: String,
    file_name: String,
    curr_substr: String,
    curr_token: Option<Token>,
    prev_token: Option<Token>,
}

impl Lexer {
    pub fn line_number(&self) -> usize {
        self.line_number
    }

    pub fn print_line_with_caret(&self) {
        // Find the start and end positions of the line that contains the given position
        // TODO make sure this works
        /*
         * something like this
         * error: expected `;`, found keyword `let`
        --> src/main.rs:9:13
           |
        9  |    let x = 5
           |             ^ help: add `;` here
        ...
        19 |     let i = 7;
           |     --- unexpected token
         */
        let long_string = &self.input;
        let pos = self.prev_prev_pos;
        let line_number = self.prev_prev_line_number + 1;
        let start_pos = long_string[..pos].rfind('\n').map_or(0, |i| i + 1);
        let end_pos = long_string[pos..]
            .find('\n')
            .map_or(long_string.len(), |i| i + pos);

        // Extract the line and the substring that corresponds to the caret (^)
        let line = &long_string[start_pos..end_pos];
        let line_number_width = line_number.to_string().len();
        let caret_str = " ".repeat(pos - start_pos) + "^";
        let line_num_spaces = " ".repeat(line_number_width);
        let pos_on_line = pos - start_pos;
        // Print the line with the caret (^) underneath the given position
        println!("{} --> {}:{}", self.file_name, line_number, pos_on_line);
        println!("{line_num_spaces} | ");
        println!("{line_number} | {line}");
        println!("{line_num_spaces} | {caret_str}");
        // println!("...");
        // println!("{} | ", self.prev_line_number + 1);
        // println!()
    }

    pub fn new(input: String, file_name: String) -> Self {
        let mut lexer = Self {
            prev_prev_pos: 0,
            prev_pos: 0,
            curr_pos: 0,
            prev_prev_line_number: 0,
            prev_line_number: 0,
            line_number: 0,
            input,
            file_name,
            curr_substr: String::new(),
            curr_token: None,
            prev_token: None,
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
            "else" => Some(Token::KeywordElse),
            "if" => Some(Token::KeywordIf),
            "for" => Some(Token::KeywordFor),
            "break" => Some(Token::KeywordBreak),
            "while" => Some(Token::KeywordWhile),
            "do" => Some(Token::KeywordDo),
            "continue" => Some(Token::KeywordContinue),
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
        if self.curr_pos >= self.input.len() {
            return None;
        }
        self.prev_prev_line_number = self.prev_line_number;
        self.prev_line_number = self.line_number;
        self.prev_prev_pos = self.prev_pos;
        self.prev_pos = self.curr_pos;
        let curr_char = self.input.as_bytes()[self.curr_pos] as char;
        self.curr_pos += 1;
        let next_char = self.input.as_bytes().get(self.curr_pos);
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
                    self.curr_pos += 1;
                    Some(Token::MinusAssign)
                } else if next_char == Some(&('-' as u8)) {
                    self.curr_pos += 1;
                    match self.prev_token {
                        Some(Token::Identifier(ref name)) => {
                            Some(Token::PostfixDecrement(name.clone()))
                        }
                        _ => {
                            // let cached_pos = self.curr_pos;
                            // let cached_prev_token = self.prev_token.take();
                            // let cached_curr_token = self.curr_token.take();
                            // let cached_substr = std::mem::take(&mut self.curr_substr);
                            let lex = self.clone();
                            let next_token = self.next_token();
                            *self = lex;
                            // self.curr_pos = cached_pos;
                            // self.prev_token = cached_prev_token;
                            // self.curr_token = cached_curr_token;
                            // self.curr_substr = cached_substr;
                            match next_token {
                                Some(Token::Identifier(name)) => Some(Token::PrefixDecrement(name)),
                                Some(Token::IntegerLiteral(int)) => {
                                    fail!("Cannot increment a temporary value {}", int)
                                }
                                _ => {
                                    unreachable!("Cannot increment something other than a variable")
                                }
                            }
                        }
                    }
                } else {
                    Some(Token::Minus)
                }
            }
            '%' => {
                if next_char == Some(&('=' as u8)) {
                    self.curr_pos += 1;
                    Some(Token::ModAssign)
                } else {
                    Some(Token::Modulo)
                }
            }
            '+' => {
                if next_char == Some(&('=' as u8)) {
                    self.curr_pos += 1;
                    Some(Token::AddAssign)
                } else if next_char == Some(&('+' as u8)) {
                    self.curr_pos += 1;
                    match self.prev_token {
                        Some(Token::Identifier(ref name)) => {
                            Some(Token::PostfixIncrement(name.clone()))
                        }
                        _ => {
                            // let cached_pos = self.curr_pos;
                            // let cached_prev_token = self.prev_token.take();
                            // let cached_curr_token = self.curr_token.take();
                            // let cached_substr = std::mem::take(&mut self.curr_substr);
                            let lex = self.clone();
                            let next_token = self.next_token();
                            *self = lex;
                            // self.curr_pos = cached_pos;
                            // self.prev_token = cached_prev_token;
                            // self.curr_token = cached_curr_token;
                            // self.curr_substr = cached_substr;
                            match next_token {
                                Some(Token::Identifier(name)) => Some(Token::PrefixIncrement(name)),
                                Some(Token::IntegerLiteral(int)) => {
                                    fail!("Cannot increment a temporary value {}", int)
                                }
                                _ => {
                                    unreachable!("Cannot increment something other than a variable")
                                }
                            }
                        }
                    }
                } else {
                    Some(Token::Add)
                }
            }
            '*' => {
                if next_char == Some(&('=' as u8)) {
                    self.curr_pos += 1;
                    Some(Token::MulAssign)
                } else {
                    Some(Token::Multiply)
                }
            }
            '/' => {
                if next_char == Some(&('=' as u8)) {
                    self.curr_pos += 1;
                    Some(Token::DivAssign)
                } else {
                    Some(Token::Divide)
                }
            }
            '^' => {
                if next_char == Some(&('=' as u8)) {
                    self.curr_pos += 1;
                    Some(Token::XorAssign)
                } else {
                    Some(Token::Xor)
                }
            }
            '!' => {
                if next_char == Some(&('=' as u8)) {
                    self.curr_pos += 1;
                    Some(Token::NotEqual)
                } else {
                    Some(Token::LogicalNot)
                }
            }
            '&' => {
                if next_char == Some(&('&' as u8)) {
                    self.curr_pos += 1;
                    Some(Token::LogicalAnd)
                } else if next_char == Some(&('=' as u8)) {
                    self.curr_pos += 1;
                    Some(Token::BitwiseAndAssign)
                } else {
                    Some(Token::BitwiseAnd)
                }
            }
            '|' => {
                if next_char == Some(&('|' as u8)) {
                    self.curr_pos += 1;
                    Some(Token::LogicalOr)
                } else if next_char == Some(&('=' as u8)) {
                    self.curr_pos += 1;
                    Some(Token::BitwiseOrAssign)
                } else {
                    Some(Token::BitwiseOr)
                }
            }
            '>' => {
                if next_char == Some(&('=' as u8)) {
                    self.curr_pos += 1;
                    Some(Token::GreaterThanOrEqualTo)
                } else if next_char == Some(&('>' as u8)) {
                    self.curr_pos += 1;
                    let third_char = self.input.as_bytes().get(self.curr_pos);
                    if third_char == Some(&('=' as u8)) {
                        self.curr_pos += 1;
                        Some(Token::BitwiseRightShiftAssign)
                    } else {
                        Some(Token::BitwiseRightShift)
                    }
                } else {
                    Some(Token::GreaterThan)
                }
            }
            '<' => {
                if next_char == Some(&('=' as u8)) {
                    self.curr_pos += 1;
                    Some(Token::LessThanOrEqualTo)
                } else if next_char == Some(&('<' as u8)) {
                    self.curr_pos += 1;
                    let third_char = self.input.as_bytes().get(self.curr_pos);
                    if third_char == Some(&('=' as u8)) {
                        self.curr_pos += 1;
                        Some(Token::BitwiseLeftShiftAssign)
                    } else {
                        Some(Token::BitwiseLeftShift)
                    }
                } else {
                    Some(Token::LessThan)
                }
            }
            '=' => {
                if next_char == Some(&('=' as u8)) {
                    self.curr_pos += 1;
                    Some(Token::EqualTo)
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
            if self.curr_pos >= self.input.len() {
                break;
            }
            let curr_char = self.input.as_bytes()[self.curr_pos] as char;
            if curr_char.is_whitespace() || Self::is_token(curr_char) {
                break;
            }
            self.curr_substr.push(curr_char);
            self.curr_pos += 1;
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
        while self.curr_pos < self.input.len()
            && (self.input.as_bytes()[self.curr_pos] as char).is_whitespace()
        {
            // TODO: windows compatibility???????
            if self.input.as_bytes()[self.curr_pos] as char == '\n' {
                self.line_number += 1;
            }
            self.curr_pos += 1;
        }
    }

    pub fn get_rest_of_input(&self) -> String {
        self.input.split_at(self.curr_pos).1.into()
    }

    fn get_line_at_position(s: &str, position: usize) -> Option<&str> {
        // Search backwards from the position to the previous newline character
        let start = s[..position].rfind('\n').map_or(0, |i| i + 1);
        // Search forwards from the position to the next newline character
        let end = s[position..]
            .find('\n')
            .map_or(s.len(), |i| position + i + 1);
        if start < end {
            Some(&s[start..end])
        } else {
            None
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.prev_token = self.curr_token.clone();
        let temp = self.curr_token.take();
        self.curr_token = self.next_token();
        temp
    }
}
