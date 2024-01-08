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
#include <ctype.h>
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

Token
newToken(TokenType type, char *start, int length, int line, int column)
{
  Token token;
  token.type   = type;
  token.column = column;
  token.line   = line;
  token.start  = start;
  token.length = length;
  return token;
}

void
printError(char *line_start, int line, int column, char *message)
{
  int i;

  fprintf(stderr, "%d | ", line);
  for (; *line_start != '\n' && *line_start != '\0'; line_start++)
    fprintf(stderr, "%c", *line_start);

  fprintf(stderr, "\n");

  for (i = -3; i < column; i++)
    fprintf(stderr, " ");

  fprintf(stderr, "^ %s\n", message);
}

int
number(char **pos, int *column, int line, Tokens *tokens)
{
  int length     = 0;
  int tok_column = *column;
  char *start    = *pos;

  if (**pos == '0')
    {
      (*pos)++;
      (*column)++;
      length++;

      switch (**pos)
        {
        case 'x':
          (*pos)++;
          (*column)++;
          length++;

          while (isxdigit(**pos))
            {
              (*pos)++;
              (*column)++;
              length++;
            }

          if (length == 2)
            {
              printError(start, line, tok_column + 2,
                         "Expected digits after '0x'");
              return 1;
            }

          tokensPush(tokens,
                     newToken(TOKEN_INTEGER, start, length, line, tok_column));
          break;
        case 'b':
          (*pos)++;
          (*column)++;
          length++;

          while (**pos == '0' || **pos == '1')
            {
              (*pos)++;
              (*column)++;
              length++;
            }

          if (length == 2)
            {
              printError(start, line, tok_column + 2,
                         "Expected digits after '0b'");
              return 1;
            }

          tokensPush(tokens,
                     newToken(TOKEN_INTEGER, start, length, line, tok_column));
          break;
        case 'o':
          (*pos)++;
          (*column)++;
          length++;

          while (**pos >= '0' && **pos <= '7')
            {
              (*pos)++;
              (*column)++;
              length++;
            }

          if (length == 2)
            {
              printError(start, line, tok_column + 2,
                         "Expected digits after '0o'");
              return 1;
            }

          tokensPush(tokens,
                     newToken(TOKEN_INTEGER, start, length, line, tok_column));
          break;

        default:
          while (isdigit(**pos))
            {
              (*pos)++;
              (*column)++;
              length++;
            }

          tokensPush(tokens,
                     newToken(TOKEN_INTEGER, start, length, line, tok_column));
        }
    }
  else
    {
      while (isdigit(**pos))
        {
          (*pos)++;
          (*column)++;
          length++;
        }

      if (**pos == '.')
        {
          int before_point = length;

          (*pos)++;
          (*column)++;
          length++;

          while (isdigit(**pos))
            {
              (*pos)++;
              (*column)++;
              length++;
            }

          if ((length - before_point) == 1)
            {
              printError(start, line, *column, "Expected digits after '.'");
              return 1;
            }

          if (**pos == 'e' || **pos == 'E')
            {
              (*pos)++;
              (*column)++;
              length++;

              if (**pos == '+' || **pos == '-')
                {
                  (*pos)++;
                  (*column)++;
                  length++;
                }

              while (isdigit(**pos))
                {
                  (*pos)++;
                  (*column)++;
                  length++;
                }

              if (before_point != 1)
                {
                  printError(start, line, tok_column + 1,
                             "Can only have one digit before the point in "
                             "scientific notation");
                  return 1;
                }
            }

          tokensPush(tokens,
                     newToken(TOKEN_FLOAT, start, length, 0, tok_column));
        }
      else
        {
          tokensPush(tokens,
                     newToken(TOKEN_INTEGER, start, length, 0, tok_column));
        }
    }

  (*pos)--;
  (*column)--;

  return 0;
}

int
tokenize(char *source, Tokens *tokens)
{
  char *pos;
  char *line_start = source;
  int column       = 1;
  int line         = 1;
  int error        = 0;

  tokensInit(tokens);

  for (pos = source; *pos != '\0'; pos++, column++)
    {
      switch (*pos)
        {
        case ' ':
        case '\t':
        case '\r':
          break;
        case '\n':
          line_start = pos + 1;
          column     = 0;
          line++;
          break;
        case '@':
          tokensPush(tokens, newToken(TOKEN_AT, pos, 1, line, column));
          break;
        case '{':
          tokensPush(tokens, newToken(TOKEN_LBRACE, pos, 1, line, column));
          break;
        case '}':
          tokensPush(tokens, newToken(TOKEN_RBRACE, pos, 1, line, column));
          break;
        case '(':
          tokensPush(tokens, newToken(TOKEN_LPAREN, pos, 1, line, column));
          break;
        case ')':
          tokensPush(tokens, newToken(TOKEN_RPAREN, pos, 1, line, column));
          break;
        case ',':
          tokensPush(tokens, newToken(TOKEN_COMMA, pos, 1, line, column));
          break;
        // TODO: equals
        case '.':
          tokensPush(tokens, newToken(TOKEN_DOT, pos, 1, line, column));
          break;
        case ':':
          tokensPush(tokens, newToken(TOKEN_COLON, pos, 1, line, column));
          break;
        default:
          {
            if (isdigit(*pos))
              {
                if (number(&pos, &column, line, tokens))
                  {
                    error++;
                  }
              }
            else
              {
                printError(line_start, line, column, "Unexpected character");
                error++;
                break;
              }
          }
        }
    }

  return error;
}
