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

use crate::{
    ast::{Pragma, Program},
    token::{Token, TokenType},
};

pub struct ParseError {
    pub message: String,
    pub line_number: usize,
    pub line: String,
    pub column: usize,
}
impl ParseError {
    pub fn new(message: String, line: String, line_number: usize, column: usize) -> Self {
        ParseError {
            message,
            line,
            line_number,
            column,
        }
    }
}

impl std::fmt::Display for ParseError {
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

pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    current: usize,
    lines: Vec<Graphemes<'a>>,
    program: Program<'a>,
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, tokens: &'a Vec<Token>) -> Self {
        let lines = source
            .split('\n')
            .map(|line| line.graphemes(true))
            .collect();
        Parser {
            tokens,
            current: 0,
            lines,
            program: Program::new(),
            errors: Vec::new(),
        }
    }

    fn is_at_end(&self) -> bool {
        self.tokens[self.current].token_type == TokenType::EOF
    }

    pub fn parse(&mut self) -> Result<&Program, &Vec<ParseError>> {
        self.program();

        if self.tokens[self.current].token_type != crate::token::TokenType::EOF {
            self.errors.push(ParseError::new(
                "expeced end of file".to_string(),
                self.lines[self.tokens[self.current].line - 1]
                    .clone()
                    .collect(),
                self.tokens[self.current].line,
                self.tokens[self.current].column,
            ));
        }

        if self.errors.len() > 0 {
            return Err(&self.errors);
        }

        Ok(&self.program)
    }

    fn program(&mut self) {
        while !self.is_at_end() {
            match self.tokens[self.current] {
                Token {
                    token_type: TokenType::Operator,
                    value: ref literal,
                    ..
                } => {
                    if literal != "@@" {
                        self.errors.push(ParseError::new(
                            "expected pragma or declaration".to_string(),
                            self.lines[self.tokens[self.current].line - 1]
                                .clone()
                                .collect(),
                            self.tokens[self.current].line,
                            self.tokens[self.current].column,
                        ));
                    }
                    self.current += 1;
                    self.pragma();
                }
                _ => {
                    self.errors.push(ParseError::new(
                        "unexpected token".to_string(),
                        self.lines[self.tokens[self.current].line - 1]
                            .clone()
                            .collect(),
                        self.tokens[self.current].line,
                        self.tokens[self.current].column,
                    ));
                    self.current += 1;
                }
            }
        }
    }

    fn pragma(&mut self) {
        if self.is_at_end() {
            self.errors.push(ParseError::new(
                "expected pragma name".to_string(),
                self.tokens[self.current].value.clone(),
                self.tokens[self.current].line,
                self.tokens[self.current].column,
            ));
            return;
        }

        if self.tokens[self.current].token_type != TokenType::Name {
            self.errors.push(ParseError::new(
                "expected pragma name".to_string(),
                self.tokens[self.current].value.clone(),
                self.tokens[self.current].line,
                self.tokens[self.current].column,
            ));
            self.current += 1;
            return;
        }
        let name = &self.tokens[self.current];
        self.current += 1;

        let arguments = Vec::new();
        // TODO: Check for arguments
        self.program.pragmas.push(Pragma::new(name, arguments));
    }
}
