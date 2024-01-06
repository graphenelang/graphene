/**
 * Copyright (C) 2024 Devin Rockwell
 *
 * This file is part of graphene.
 *
 * graphene is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * graphene is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with graphene.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "lexer.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void
tokensInit(Tokens *tokens)
{
  tokens->capacity = 0;
  tokens->length   = 0;
  tokens->tokens   = NULL;
}

void
tokensFree(Tokens *tokens)
{
  free(tokens->tokens);
  tokensInit(tokens);
}

void
tokensPush(Tokens *tokens, Token token)
{
  if (tokens->length == tokens->capacity)
    {
      tokens->capacity = tokens->capacity == 0 ? 32 : tokens->capacity * 2;
      tokens->tokens
          = realloc(tokens->tokens, tokens->capacity * sizeof(Token));
    }

  tokens->tokens[tokens->length++] = token;
}

Tokens
tokenize(char *source)
{
  Tokens tokens;
  char *line = strtok(source, "\n");

  tokensInit(&tokens);

  for (; line != NULL; line = strtok(NULL, "\n"))
    {
      printf("line: %s\n", line);
    }

  return tokens;
}
