// Copyright (C) 2025 Devin Rockwell
//
// This file is part of graphene.
//
// graphene is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// graphene is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with graphene.  If not, see <https://www.gnu.org/licenses/>.

use unicode_categories::UnicodeCategories;
use unicode_segmentation::{Graphemes, UnicodeSegmentation};

use crate::token::{Token, TokenType};

pub struct Lexer<'a> {
    source: &'a str,
    line: usize,
    column: usize,
    tokens: Vec<Token>,
    errors: Vec<LexError>,
}

#[derive(Debug, Clone)]
pub struct LexError {
    line: String,
    line_number: usize,
    column: usize,
    message: String,
}

impl LexError {
    #[must_use]
    pub fn new(line: String, line_number: usize, column: usize, message: String) -> Self {
        LexError {
            line,
            line_number,
            column,
            message,
        }
    }
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} | {}", self.line_number, self.line)?;
        let mut i = 0;
        while i < self.column + 3 {
            write!(f, " ")?;
            i += 1;
        }

        write!(f, "^ {}", self.message)
    }
}

fn is_name_char(c: char) -> bool {
    c.is_alphanumeric() || c.is_mark() || c == '_'
}

fn is_operator_char(c: char) -> bool {
    c.is_symbol() || c.is_punctuation()
}

impl<'a> Lexer<'a> {
    #[must_use]
    pub fn new(source: &'a str) -> Self {
        Lexer {
            source,
            line: 1,
            column: 1,
            tokens: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn tokenize(&mut self) -> Result<&Vec<Token>, &Vec<LexError>> {
        let lines: Vec<Graphemes<'a>> = self
            .source
            .split('\n')
            .map(|line| line.graphemes(true))
            .collect();

        for line in lines {
            self.column = 1;
            let graphemes: Vec<&str> = line.collect();
            while self.column - 1 < graphemes.len() {
                let grapheme = graphemes[self.column - 1];
                self.column += 1;
                match grapheme {
                    "{" => {
                        self.tokens.push(Token::new(
                            TokenType::Lbrace,
                            grapheme.to_string(),
                            self.line,
                            self.column - 1,
                        ));
                    }
                    "}" => {
                        self.tokens.push(Token::new(
                            TokenType::Rbrace,
                            grapheme.to_string(),
                            self.line,
                            self.column - 1,
                        ));
                    }
                    "(" => {
                        self.tokens.push(Token::new(
                            TokenType::Lparen,
                            grapheme.to_string(),
                            self.line,
                            self.column - 1,
                        ));
                    }
                    ")" => {
                        self.tokens.push(Token::new(
                            TokenType::Rparen,
                            grapheme.to_string(),
                            self.line,
                            self.column - 1,
                        ));
                    }
                    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" => {
                        self.number(&graphemes);
                    }
                    "'" => {
                        self.character(&graphemes);
                    }
                    "\"" => {
                        self.string(&graphemes);
                    }
                    _ => {
                        let c = grapheme.chars().next().unwrap_or('\0');
                        if c.is_whitespace() {
                        } else if is_name_char(c) {
                            self.name_or_keyword(&graphemes);
                        } else if is_operator_char(c) {
                            self.operator(&graphemes);
                        } else {
                            self.errors.push(LexError::new(
                                graphemes.concat(),
                                self.line,
                                self.column - 1,
                                format!("Unexpected character: {grapheme}"),
                            ));
                        }
                    }
                }
            }

            self.line += 1;
        }

        if self.errors.is_empty() {
            Ok(&self.tokens)
        } else {
            Err(&self.errors)
        }
    }

    fn number(&mut self, graphemes: &[&str]) {
        let token_col = self.column - 1;
        let mut is_float = false;
        let mut is_scientific = false;
        let mut number = graphemes[self.column - 2].to_string();
        while self.column - 1 < graphemes.len() {
            let grapheme = graphemes[self.column - 1];
            if grapheme.chars().next().unwrap_or('\0').is_ascii_digit() {
                number.push_str(grapheme);
                self.column += 1;
            } else if grapheme == "." {
                if is_float {
                    self.errors.push(LexError::new(
                        graphemes.concat(),
                        self.line,
                        self.column,
                        "number may not contain multiple fractional parts".to_owned(),
                    ));
                    return;
                }
                is_float = true;
                number.push_str(grapheme);
                self.column += 1;
                if self.column > graphemes.len()
                    || !(graphemes[self.column - 1]
                        .chars()
                        .next()
                        .unwrap_or('\0')
                        .is_ascii_digit())
                {
                    self.errors.push(LexError::new(
                        graphemes.concat(),
                        self.line,
                        self.column - 1,
                        "expected at least one digit after '.'".to_owned(),
                    ));
                    return;
                }
            } else if grapheme == "_" {
                self.column += 1;
            } else if grapheme == "e" || grapheme == "E" {
                if is_scientific {
                    self.errors.push(LexError::new(
                        graphemes.concat(),
                        self.line,
                        self.column,
                        "number may not contain multiple exponential parts".to_owned(),
                    ));
                    return;
                }
                is_float = true;
                is_scientific = true;
                number.push_str(grapheme);
                self.column += 1;
                if self.column > graphemes.len() {
                    self.errors.push(LexError::new(
                        graphemes.concat(),
                        self.line,
                        self.column - 1,
                        "expected at least one digit after 'e'".to_owned(),
                    ));
                    return;
                }
                if graphemes[self.column - 1] == "+" || graphemes[self.column - 1] == "-" {
                    number.push_str(graphemes[self.column - 1]);
                    self.column += 1;
                }
                if self.column < graphemes.len()
                    && graphemes[self.column - 1]
                        .chars()
                        .next()
                        .unwrap_or('\0')
                        .is_ascii_digit()
                {
                    number.push_str(graphemes[self.column - 1]);
                    self.column += 1;
                } else {
                    self.errors.push(LexError::new(
                        graphemes.concat(),
                        self.line,
                        self.column - 1,
                        "expected at least one digit after in exponent".to_owned(),
                    ));
                    return;
                }
            } else {
                break;
            }
        }
        self.tokens.push(Token::new(
            if is_float {
                TokenType::Float
            } else {
                TokenType::Integer
            },
            number,
            self.line,
            token_col,
        ));
    }

    fn name_or_keyword(&mut self, graphemes: &[&str]) {
        let token_col = self.column - 2;
        let mut name = graphemes[self.column - 2].to_string();
        while self.column - 1 < graphemes.len() {
            let grapheme = graphemes[self.column - 1];
            if is_name_char(grapheme.chars().next().unwrap_or('\0')) {
                name.push_str(grapheme);
                self.column += 1;
            } else {
                break;
            }
        }

        let token_type = match name.as_str() {
            "const" => TokenType::Const,
            "entity" => TokenType::Entity,
            "noinit" => TokenType::NoInit,
            "enum" => TokenType::Enum,
            "system" => TokenType::System,
            "fn" => TokenType::Fn,
            "extern" => TokenType::Extern,
            "decorator" => TokenType::Decorator,
            "macro" => TokenType::Macro,
            "for" => TokenType::For,
            "in" => TokenType::In,
            "break" => TokenType::Break,
            "continue" => TokenType::Continue,
            "while" => TokenType::While,
            "match" => TokenType::Match,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "elseif" => TokenType::ElseIf,
            "true" => TokenType::True,
            "false" => TokenType::False,
            "null" => TokenType::Null,
            _ => TokenType::Name,
        };

        self.tokens
            .push(Token::new(token_type, name, self.line, token_col));
    }

    fn operator(&mut self, graphemes: &[&str]) {
        let token_col = self.column - 2;
        let mut operator = graphemes[self.column - 2].to_string();
        while self.column - 1 < graphemes.len() {
            let grapheme = graphemes[self.column - 1];
            if is_operator_char(grapheme.chars().next().unwrap_or('\0')) {
                operator.push_str(grapheme);
                self.column += 1;
            } else {
                break;
            }
        }

        if operator.as_str() == "//" {
            self.comment(graphemes);
            return;
        }

        let token_type = match operator.as_str() {
            "=" => TokenType::Equal,
            ":" => TokenType::Colon,
            "," => TokenType::Comma,
            "." => TokenType::Dot,
            "@" => TokenType::At,
            _ => TokenType::Operator,
        };

        self.tokens
            .push(Token::new(token_type, operator, self.line, token_col));
    }

    fn comment(&mut self, graphemes: &[&str]) {
        while self.column - 1 < graphemes.len() {
            if graphemes[self.column - 1] == "\n" {
                break;
            }
            self.column += 1;
        }
    }

    fn character(&mut self, graphemes: &[&str]) {
        let token_col = self.column - 1;
        if self.column > graphemes.len() {
            self.errors.push(LexError::new(
                graphemes.concat(),
                self.line,
                self.column - 1,
                "expected character after '".to_owned(),
            ));
        }
        let mut char_value = String::with_capacity(1);
        let mut char_len = 0;
        while self.column - 1 < graphemes.len() {
            let grapheme = graphemes[self.column - 1];
            if grapheme == "\\" {
                if self.column < graphemes.len() {
                    self.column += 1;
                    match graphemes[self.column - 1] {
                        "a" => {
                            char_value.push('\x07');
                            self.column += 1;
                        }
                        "b" => {
                            char_value.push('\x08');
                            self.column += 1;
                        }
                        "f" => {
                            char_value.push('\x0C');
                            self.column += 1;
                        }
                        "n" => {
                            char_value.push('\n');
                            self.column += 1;
                        }
                        "r" => {
                            char_value.push('\r');
                            self.column += 1;
                        }
                        "t" => {
                            char_value.push('\t');
                            self.column += 1;
                        }
                        "v" => {
                            char_value.push('\x0B');
                            self.column += 1;
                        }
                        "\\" => {
                            char_value.push('\\');
                            self.column += 1;
                        }
                        "\"" => {
                            char_value.push('"');
                            self.column += 1;
                        }
                        "'" => {
                            char_value.push('\'');
                            self.column += 1;
                        }
                        "0" => {
                            char_value.push('\0');
                            self.column += 1;
                        }
                        "u" => {
                            let mut escape = String::with_capacity(6);
                            self.column += 1;
                            while self.column - 1 < graphemes.len()
                                && graphemes[self.column - 1]
                                    .chars()
                                    .next()
                                    .unwrap_or('\0')
                                    .is_ascii_hexdigit()
                            {
                                if escape.len() == 6 {
                                    break;
                                }
                                escape.push_str(graphemes[self.column - 1]);
                                self.column += 1;
                            }
                            if escape.len() != 4 && escape.len() != 6 {
                                self.errors.push(LexError::new(
                                    graphemes.concat(),
                                    self.line,
                                    self.column - 1,
                                    "expected 4 or 6 hex digits after \\u".to_owned(),
                                ));
                                return;
                            }
                            let codepoint = u32::from_str_radix(&escape, 16).unwrap_or(0);
                            char_value.push(char::from_u32(codepoint).unwrap_or('\0'));
                        }
                        _ => {
                            self.errors.push(LexError::new(
                                graphemes.concat(),
                                self.line,
                                self.column - 1,
                                format!(
                                    "invalid escape sequence: \\{}",
                                    graphemes[self.column - 1]
                                ),
                            ));
                            return;
                        }
                    }
                    char_len += 1;
                } else {
                    self.errors.push(LexError::new(
                        graphemes.concat(),
                        self.line,
                        self.column - 1,
                        "expected character after '\\'".to_owned(),
                    ));
                    return;
                }
            } else if grapheme == "'" {
                break;
            } else {
                char_value.push_str(grapheme);
                self.column += 1;
                char_len += grapheme.chars().count();
            }
        }

        if self.column > graphemes.len() || graphemes[self.column - 1] != "'" {
            self.errors.push(LexError::new(
                graphemes.concat(),
                self.line,
                self.column - 1,
                "unterminated character literal".to_owned(),
            ));
            return;
        }

        if char_len != 1 {
            self.errors.push(LexError::new(
                graphemes.concat(),
                self.line,
                self.column - 1,
                "character literal must consist of a single character or escape sequence"
                    .to_owned(),
            ));
        }

        self.column += 1;

        self.tokens.push(Token::new(
            TokenType::Char,
            char_value,
            self.line,
            token_col,
        ));
    }

    fn string(&mut self, graphemes: &[&str]) {
        let token_col = self.column - 1;

        let mut string_value = String::with_capacity(16);
        while self.column - 1 < graphemes.len() {
            let grapheme = graphemes[self.column - 1];
            if grapheme == "\\" {
                if self.column < graphemes.len() {
                    self.column += 1;
                    match graphemes[self.column - 1] {
                        "a" => {
                            string_value.push('\x07');
                            self.column += 1;
                        }
                        "b" => {
                            string_value.push('\x08');
                            self.column += 1;
                        }
                        "f" => {
                            string_value.push('\x0C');
                            self.column += 1;
                        }
                        "n" => {
                            string_value.push('\n');
                            self.column += 1;
                        }
                        "r" => {
                            string_value.push('\r');
                            self.column += 1;
                        }
                        "t" => {
                            string_value.push('\t');
                            self.column += 1;
                        }
                        "v" => {
                            string_value.push('\x0B');
                            self.column += 1;
                        }
                        "\\" => {
                            string_value.push('\\');
                            self.column += 1;
                        }
                        "\"" => {
                            string_value.push('"');
                            self.column += 1;
                        }
                        "'" => {
                            string_value.push('\'');
                            self.column += 1;
                        }
                        "0" => {
                            string_value.push('\0');
                            self.column += 1;
                        }
                        "u" => {
                            let mut escape = String::with_capacity(6);
                            self.column += 1;
                            while self.column - 1 < graphemes.len()
                                && graphemes[self.column - 1]
                                    .chars()
                                    .next()
                                    .unwrap_or('\0')
                                    .is_ascii_hexdigit()
                            {
                                if escape.len() == 6 {
                                    break;
                                }
                                escape.push_str(graphemes[self.column - 1]);
                                self.column += 1;
                            }
                            if escape.len() != 4 && escape.len() != 6 {
                                self.errors.push(LexError::new(
                                    graphemes.concat(),
                                    self.line,
                                    self.column - 1,
                                    "expected 4 or 6 hex digits after \\u".to_owned(),
                                ));
                                return;
                            }
                            let codepoint = u32::from_str_radix(&escape, 16).unwrap_or(0);
                            string_value.push(char::from_u32(codepoint).unwrap_or('\0'));
                        }
                        _ => {
                            self.errors.push(LexError::new(
                                graphemes.concat(),
                                self.line,
                                self.column - 1,
                                format!(
                                    "invalid escape sequence: \\{}",
                                    graphemes[self.column - 1]
                                ),
                            ));
                            return;
                        }
                    }
                } else {
                    self.errors.push(LexError::new(
                        graphemes.concat(),
                        self.line,
                        self.column - 1,
                        "expected character after '\\'".to_owned(),
                    ));
                    return;
                }
            } else if grapheme == "\"" {
                break;
            } else {
                self.column += 1;
                string_value.push_str(grapheme);
            }
        }

        if self.column > graphemes.len() || graphemes[self.column - 1] != "\"" {
            self.errors.push(LexError::new(
                graphemes.concat(),
                self.line,
                self.column - 1,
                "unterminated string literal".to_owned(),
            ));
        }

        self.column += 1;

        self.tokens.push(Token::new(
            TokenType::String,
            string_value,
            self.line,
            token_col,
        ));
    }
}
