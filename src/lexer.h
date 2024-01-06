// Copyright (C) 2024 Devin Rockwell
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
// along with graphene.  If not, see <http://www.gnu.org/licenses/>.

#ifndef LEXER_H
#define LEXER_H

typedef enum
{
  TOKEN_EOF,
  TOKEN_AT,
  TOKEN_CONST,
  TOKEN_ENTITY,
  TOKEN_NAME,
  TOKEN_OPERATOR,
  TOKEN_LBRACE,
  TOKEN_RBRACE,
  TOKEN_LPAREN,
  TOKEN_RPAREN,
  TOKEN_COMMA,
  TOKEN_EQUAL,
  TOKEN_PRIVATE,
  TOKEN_ENUM,
  TOKEN_SYSTEM,
  TOKEN_FUNC,
  TOKEN_EXTERN,
  TOKEN_DECORATOR,
  TOKEN_MACRO,
  TOKEN_TRY,
  TOKEN_CATCH,
  TOKEN_FOR,
  TOKEN_IN,
  TOKEN_BREAK,
  TOKEN_CONTINUE,
  TOKEN_WHILE,
  TOKEN_SWITCH,
  TOKEN_CASE,
  TOKEN_FALL,
  TOKEN_DEFAULT,
  TOKEN_IF,
  TOKEN_ELSE,
  TOKEN_ELSEIF,
  TOKEN_DOT,
  TOKEN_STRING,
  TOKEN_CHAR,
  TOKEN_TRUE,
  TOKEN_FALSE,
  TOKEN_NULL,
  TOKEN_INTEGER,
  TOKEN_FLOAT,
  TOKEN_COLON
} TokenType;

typedef struct
{
  TokenType type;
  const char *literal;
  int line;
  int column;
} Token;

typedef struct
{
  Token *tokens;
  int length;
  int capacity;
} Tokens;

void tokensInit(Tokens *tokens);
void tokensFree(Tokens *tokens);
void tokensPush(Tokens *tokens, Token token);

Tokens tokenize(char *source);

#endif
