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

use unicode_segmentation::{Graphemes, UnicodeSegmentation};

use crate::token::{self, Token, TokenType};

pub struct Lexer<'a> {
    lines: Vec<Graphemes<'a>>,
    line: usize,
    column: usize,
    tokens: Vec<Token>,
    errors: Vec<LexError<'a>>,
}

#[derive(Debug, Clone)]
pub struct LexError<'a> {
    line: &'a str,
    line_number: usize,
    column: usize,
    message: String,
}

impl<'a> LexError<'a> {
    pub fn new(line: &'a str, line_number: usize, column: usize, message: String) -> Self {
        LexError {
            line,
            line_number,
            column,
            message,
        }
    }
}

impl<'a> std::fmt::Display for LexError<'a> {
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

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        let lines: Vec<Graphemes<'a>> = source
            .split("\n")
            .map(|line| line.graphemes(true))
            .collect();

        Lexer {
            lines,
            line: 1,
            column: 1,
            tokens: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn tokenize(&mut self) -> Result<&Vec<Token>, &Vec<LexError>> {
        for line in self.lines.clone() {
            let graphemes: Vec<&str> = line.collect();
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
                        self.errors.push(LexError::new(
                            self.lines[self.line - 1].as_str(),
                            self.line,
                            self.column - 1,
                            format!("Unexpected character: {}", grapheme),
                        ));
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
        let mut float = false;
        let mut number = graphemes[self.column - 2].to_string();
        while self.column - 1 < graphemes.len() {
            let grapheme = graphemes[self.column - 1];
            if grapheme.chars().next().unwrap_or('\0').is_digit(10) {
                number.push_str(grapheme);
                self.column += 1;
            } else if grapheme == "." {
                if float {
                    self.errors.push(LexError::new(
                        self.lines[self.line - 1].as_str(),
                        self.line,
                        self.column,
                        format!("number may not contain multiple '.'"),
                    ));
                    return;
                }
                float = true;
                number.push_str(grapheme);
                self.column += 1;
                if self.column - 1 >= graphemes.len() {
                    self.errors.push(LexError::new(
                        self.lines[self.line - 1].as_str(),
                        self.line,
                        self.column,
                        format!("expected at least one digit after '.'"),
                    ));
                    return;
                }
            } else if grapheme == "_" {
                self.column += 1;
            } else {
                break;
            }
        }
        self.tokens.push(Token::new(
            if float {
                TokenType::Float
            } else {
                TokenType::Integer
            },
            number,
            self.line,
            token_col,
        ))
    }
}
