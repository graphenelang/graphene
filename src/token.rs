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

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    At,
    Const,
    Entity,
    Name,
    Operator,
    Lbrace,
    Rbrace,
    Lparen,
    Rparen,
    Comma,
    Equal,
    NoInit,
    Enum,
    System,
    Fn,
    Extern,
    Decorator,
    Macro,
    For,
    In,
    Break,
    Continue,
    While,
    Match,
    If,
    Else,
    ElseIf,
    Dot,
    String,
    Char,
    True,
    False,
    Null,
    Integer,
    Float,
    Colon,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
    pub line: usize,
    pub column: usize,
}

impl Token {
    #[must_use]
    pub fn new(token_type: TokenType, value: String, line: usize, column: usize) -> Self {
        Token {
            token_type,
            value,
            line,
            column,
        }
    }
}
