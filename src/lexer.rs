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
use unicode_normalization::UnicodeNormalization;
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
        write!(f, "{} | {}\n", self.line_number, self.line)?;
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

impl<'a> Lexer<'a> {
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
            .split("\n")
            .map(|line| line.graphemes(true))
            .collect();

        for line in lines {
            self.column = 1;
            let graphemes: Vec<&str> = line.clone().collect();
            while self.column - 1 < graphemes.len() {
                let grapheme = graphemes[self.column - 1];
                self.column += 1;
                match grapheme {
                    "@" => {
                        self.tokens.push(Token::new(
                            TokenType::At,
                            grapheme.to_string(),
                            self.line,
                            self.column - 1,
                        ));
                    }
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
                    "," => {
                        self.tokens.push(Token::new(
                            TokenType::Comma,
                            grapheme.to_string(),
                            self.line,
                            self.column - 1,
                        ));
                    }
                    "=" => {
                        self.tokens.push(Token::new(
                            TokenType::Equal,
                            grapheme.to_string(),
                            self.line,
                            self.column - 1,
                        ));
                    }
                    ":" => {
                        self.tokens.push(Token::new(
                            TokenType::Colon,
                            grapheme.to_string(),
                            self.line,
                            self.column - 1,
                        ));
                    }
                    "." => {
                        self.tokens.push(Token::new(
                            TokenType::Dot,
                            grapheme.to_string(),
                            self.line,
                            self.column - 1,
                        ));
                    }
                    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" => {
                        self.number(&graphemes);
                    }
                    _ => {
                        let c = grapheme.chars().next().unwrap_or('\0');
                        if is_name_char(c) {
                            self.name_or_keyword(&graphemes);
                        } else {
                            self.errors.push(LexError::new(
                                line.as_str().to_string(),
                                self.line,
                                self.column - 1,
                                format!("Unexpected character: {}", grapheme),
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

    fn number(&mut self, graphemes: &Vec<&str>) {
        let token_col = self.column - 1;
        let mut is_float = false;
        let mut is_scientific = false;
        let mut number = graphemes[self.column - 2].to_string();
        while self.column - 1 < graphemes.len() {
            let grapheme = graphemes[self.column - 1];
            if grapheme.chars().next().unwrap_or('\0').is_digit(10) {
                number.push_str(grapheme);
                self.column += 1;
            } else if grapheme == "." {
                if is_float {
                    self.errors.push(LexError::new(
                        graphemes.concat(),
                        self.line,
                        self.column,
                        format!("number may not contain multiple fractional parts"),
                    ));
                    return;
                }
                is_float = true;
                number.push_str(grapheme);
                self.column += 1;
                if self.column - 1 >= graphemes.len() {
                    self.errors.push(LexError::new(
                        graphemes.concat(),
                        self.line,
                        self.column - 1,
                        format!("expected at least one digit after '.'"),
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
                        format!("number may not contain multiple exponential parts"),
                    ));
                    return;
                }
                is_float = true;
                is_scientific = true;
                number.push_str(grapheme);
                self.column += 1;
                if self.column - 1 >= graphemes.len() {
                    self.errors.push(LexError::new(
                        graphemes.concat(),
                        self.line,
                        self.column - 1,
                        format!("expected at least one digit after 'e'"),
                    ));
                    return;
                }
                if graphemes[self.column - 1] == "+" || graphemes[self.column - 1] == "-" {
                    number.push_str(graphemes[self.column - 1]);
                    self.column += 1;
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
        ))
    }

    fn name_or_keyword(&mut self, graphemes: &Vec<&str>) {
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
}
