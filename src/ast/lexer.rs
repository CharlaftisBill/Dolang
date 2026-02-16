use std::fs;

use crate::ast::token::{Tag, Token};

#[derive(Debug)]
pub struct Lexer {
    pub src: Vec<char>,
    pub line: usize,
    pub at: usize,
    pub last_new_line_at: usize,
}

impl Lexer {
    pub fn new(src_path: &str) -> Result<Self, std::io::Error> {
        let src = fs::read_to_string(src_path)?;

        Ok(Self {
            src: src.chars().collect(),
            line: 0,
            at: 0,
            last_new_line_at: 0,
        })
    }

    pub fn new_line(&mut self) {
        self.line += 1;
        self.last_new_line_at = self.at;
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, ()> {
        let mut tokens = vec![];

        loop {
            let next_char = match self.peak() {
                Some(character) => character,
                None => break,
            };

            match next_char {
                // keep the order
                '#' => tokens.push(self.consume_comment()),
                '\n' => tokens.push(self.consume_new_lines()),
                ' ' | '\t' | '\r' => self.at += 1,
                '=' | ':' | '+' | '-' | '*' | '%' | '/' | '^' | '!' | '|' | '&' | '.' | '{'
                | '}' | '(' | ')' | '[' | ']' | '<' | '>' | ',' => tokens.push(self.consume_symbol()),
                '0'..='9' => tokens.push(self.consume_number()),
                '"' => tokens.push(self.consume_string_literal()),
                '\'' => tokens.push(self.consume_character_literal()),
                _ => tokens.push(self.consume_ident_or_keyword()),
            };

            if tokens.len() == self.at + 1 {
                break;
            }
        }

        // println!("---> tokens: '{:?}'", tokens.len());

        tokens.push(Token::new(self, Tag::EOF));
        Ok(tokens)
    }

    // tokenizers
    fn consume_comment(&mut self) -> Token {
        let mut comment_body = String::new();

        loop {
            match self.peak() {
                Some(character) => {
                    if character != '\n' {
                        self.at += 1;
                        comment_body.push(character)
                    } else {
                        break;
                    }
                }
                None => break,
            };
        }

        Token::new(self, super::token::Tag::COMMENT(comment_body))
    }

    fn consume_symbol(&mut self) -> Token {
        let mut symbol = String::new();

        let symbols: Vec<char> = vec![
            '=', ':', '+', '-', '*', '%', '/', '^', '!', '|', '&', '.', '{', '}', '(', ')', '[',
            ']', '<', '>', ',',
        ];
        let operators: Vec<char> = vec![
            '*', '/', '%', '<', '>', '&', '+', '-', '|', '^', '=', '!', '<', '>', '&',
        ];

        let operators_may_doubled: Vec<char> = vec!['|', '&', '<', '>', '='];

        match self.peak() {
            Some(character) => {
                if symbols.contains(&character) {
                    self.at += 1;
                    symbol.push(character);
                }

                let is_operator = operators.contains(&character);
                let is_doubled_operator = operators_may_doubled.contains(&character);

                if is_operator {
                    if let Some(next_char) = self.peak() {
                        if next_char == '='
                            || next_char == ':'
                            || (is_doubled_operator && (next_char == character))
                        {
                            symbol.push(self.src[self.at]);
                            self.at += 1;
                        } else if symbols.contains(&next_char)
                            && !(character == '!' && next_char == '(')
                        {
                            return Token::new_invalid(
                                self,
                                format!(
                                    "Arithmetic Operator Symbol '{symbol}' can only be followed by either ':' or '=' symbols."
                                ),
                            );
                        }
                    } else {
                        panic!(
                            "L{}:{}  Unexpected 'None' while peaking character!",
                            self.line,
                            self.at - self.last_new_line_at
                        );
                    }
                }
            }
            None => panic!(
                "L{}:{}  Expected symbol got None!",
                self.line,
                self.at - self.last_new_line_at
            ),
        };

        Token::new(self, super::token::Tag::SYMBOL(symbol))
    }

    fn consume_ident_or_keyword(&mut self) -> Token {
        let mut word: String = self.consume_word();

        let keywords = vec![
            "success",
            "failure",
            "catch",
            "on",
            "do",
            "if",
            "else",
            "while",
            "for",
            "loop",
            "match",
            "break",
            "continue",
            "enum",
            "union",
            "type",
            "of",
            "interface",
            "import",
        ];

        if word == "true" || word == "false" {
            return Token::new(self, super::token::Tag::BOOL(word));
        } else if keywords.contains(&word.as_str()) {
            return Token::new(self, super::token::Tag::KEYWORD(word));
        }

        match self.peak() {
            Some(character) => {
                if character == '.' {
                    self.at += 1;
                    let method_or_field = self.consume_word();
                    word.push_str(".");
                    word.push_str(&method_or_field);
                }
            }
            None => {
                //proceed with canonical flow
            }
        }

        Token::new(self, super::token::Tag::IDENT(word))
    }

    fn consume_number(&mut self) -> Token {
        let mut number = String::new();

        let mut is_integer = true;
        loop {
            match self.peak() {
                Some(character) => {
                    if character.is_ascii_digit() {
                        self.at += 1;
                        number.push(character)
                    } else if character == '.' && is_integer {
                        self.at += 1;
                        is_integer = false;
                        number.push(character)
                    } else if character == '.' && !is_integer {
                        return Token::new_invalid(
                            self,
                            "Number cannot have decimal points.".to_string(),
                        );
                    } else {
                        break;
                    }
                }
                None => break,
            };
        }

        if is_integer {
            return Token::new(self, super::token::Tag::INTEGER(number));
        }

        Token::new(self, super::token::Tag::FLOAT(number))
    }

    fn consume_string_literal(&mut self) -> Token {
        let mut string_body = String::new();

        self.at += 1; // avoid include quotes on string body
        loop {
            match self.peak() {
                Some(character) => {
                    if character == '"' && self.src[self.at - 1] != '\\' {
                        break;
                    }
                    self.at += 1;
                    string_body.push(character);
                }
                None => break,
            };
        }
        self.at += 1; // ignore the closing quotes
        Token::new(self, super::token::Tag::STRING(string_body))
    }

    fn consume_character_literal(&mut self) -> Token {
        let mut char_body = String::new();

        self.at += 1; // avoid include quotes on character body
        loop {
            match self.peak() {
                Some(character) => {
                    if character == '\'' && self.src[self.at - 1] != '\\' {
                        break;
                    }
                    self.at += 1;
                    char_body.push(character);
                }
                None => break,
            };
        }
        self.at += 1;

        if char_body.len() > 2 {
            return Token::new_invalid(
                self,
                format!(
                    "Unexpected character length (:{}). A character can only be a single byte.",
                    char_body.len()
                ),
            );
        }

        Token::new(self, super::token::Tag::CHARACTER(char_body))
    }

    fn consume_new_lines(&mut self) -> Token {
        let mut new_lines = String::new();

        loop {
            match self.peak() {
                Some(character) => {
                    if character == '\n' {
                        new_lines += "\n";
                        self.at += 1;
                        self.new_line();
                    } else {
                        break;
                    }
                }
                None => break,
            }
        }

        Token::new(self, super::token::Tag::NEWLINE(new_lines))
    }

    // helpers
    fn consume_word(&mut self) -> String {
        let mut word = String::new();
        loop {
            match self.peak() {
                Some(character) => {
                    if !character.is_ascii_whitespace()
                        && (!character.is_ascii_punctuation() || character == '_')
                    {
                        self.at += 1;
                        word.push(character)
                    } else {
                        break;
                    }
                }
                None => break,
            };
        }
        word
    }

    fn peak(&self) -> Option<char> {
        if self.at == 0 {
            return Some(self.src[0]);
        }

        if self.at < self.src.len() {
            return Some(self.src[self.at]);
        }
        None
    }
}
