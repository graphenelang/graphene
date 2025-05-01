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

use crate::token::{Token, TokenType};

pub struct Lexer<'a> {
    lines: Vec<Graphemes<'a>>,
    line: usize,
    column: usize,
}

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
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, Vec<LexError>> {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();

        for line in self.lines.clone() {
            for grapheme in line {
                match grapheme {
                    "@" => {
                        tokens.push(Token::new(
                            TokenType::At,
                            grapheme.to_string(),
                            self.line,
                            self.column,
                        ));
                    }
                    _ => {
                        errors.push(LexError::new(
                            self.lines[self.line - 1].as_str(),
                            self.line,
                            self.column,
                            format!("Unexpected character: {}", grapheme),
                        ));
                    }
                }
                self.column += 1;
            }

            self.line += 1;
        }

        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(errors)
        }
    }
}
