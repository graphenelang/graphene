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
newToken(TokenType type, uint8_t *start, int length, int line, int column)
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
number(char *line_start, uint8_t **pos, int *column, int line, Tokens *tokens)
{
  int length     = 0;
  int tok_column = *column;
  uint8_t *start = *pos;

  utf8proc_ssize_t bytes_read;
  int32_t codepoint;
  if (bytes_read = utf8proc_iterate(*pos, -1, &codepoint), codepoint == '0')
    {
      (*pos) += bytes_read;
      (*column)++;
      length++;

      switch (bytes_read = utf8proc_iterate(*pos, -1, &codepoint), codepoint)
        {
        case 'x':
          (*pos) += bytes_read;
          (*column)++;
          length++;

          while (bytes_read = utf8proc_iterate(*pos, -1, &codepoint),
                 isxdigit(codepoint))
            {
              (*pos) += bytes_read;
              (*column)++;
              length++;
            }

          if (length == 2)
            {
              printError(line_start, line, *column,
                         "Expected hexadecimal digit");
              return 1;
            }

          tokensPush(tokens,
                     newToken(TOKEN_INTEGER, start, length, line, tok_column));
          break;
        case 'b':
          (*pos) += bytes_read;
          (*column)++;
          length++;

          while (bytes_read = utf8proc_iterate(*pos, -1, &codepoint),
                 codepoint == '0' || codepoint == '1')
            {
              (*pos) += bytes_read;
              (*column)++;
              length++;
            }

          if (length == 2)
            {
              printError(line_start, line, *column, "Expected binary digit");
              return 1;
            }

          tokensPush(tokens,
                     newToken(TOKEN_INTEGER, start, length, line, tok_column));
          break;

        case 'o':
          (*pos) += bytes_read;
          (*column)++;
          length++;

          while (bytes_read = utf8proc_iterate(*pos, -1, &codepoint),
                 codepoint >= '0' && codepoint <= '7')
            {
              (*pos) += bytes_read;
              (*column)++;
              length++;
            }

          if (length == 2)
            {
              printError(line_start, line, *column, "Expected octal digit");
              return 1;
            }

          tokensPush(tokens,
                     newToken(TOKEN_INTEGER, start, length, line, tok_column));
          break;
        default:
          while (bytes_read = utf8proc_iterate(*pos, -1, &codepoint),
                 isdigit(codepoint))
            {
              (*pos) += bytes_read;
              (*column)++;
              length++;
            }

          tokensPush(tokens,
                     newToken(TOKEN_INTEGER, start, length, line, tok_column));
        }
    }
  else
    {
      while (bytes_read = utf8proc_iterate(*pos, -1, &codepoint),
             isdigit(codepoint))
        {
          (*pos) += bytes_read;
          (*column)++;
          length++;
        }

      if (bytes_read = utf8proc_iterate(*pos, -1, &codepoint),
          codepoint == '.')
        {
          int before_point = length;

          (*pos) += bytes_read;
          (*column)++;
          length++;

          while (bytes_read = utf8proc_iterate(*pos, -1, &codepoint),
                 isdigit(codepoint))
            {
              (*pos) += bytes_read;
              (*column)++;
              length++;
            }

          if (length == before_point + 1)
            {
              printError(line_start, line, *column,
                         "Expected digit after decimal point");
              return 1;
            }

          if (bytes_read = utf8proc_iterate(*pos, -1, &codepoint),
              codepoint == 'e' || codepoint == 'E')
            {
              int e_length = length;

              (*pos) += bytes_read;
              (*column)++;
              length++;

              if (bytes_read = utf8proc_iterate(*pos, -1, &codepoint),
                  codepoint == '+' || codepoint == '-')
                {
                  (*pos) += bytes_read;
                  (*column)++;
                  length++;
                }

              while (bytes_read = utf8proc_iterate(*pos, -1, &codepoint),
                     isdigit(codepoint))
                {
                  (*pos) += bytes_read;
                  (*column)++;
                  length++;
                }

              if (before_point != 1)
                {
                  printError(line_start, line, tok_column + before_point - 1,
                             "Can only have one digit before the point in "
                             "scientific notation");
                  return 1;
                }

              if (length == e_length + 1)
                {
                  printError(line_start, line, *column,
                             "Expected digit after exponent");
                  return 1;
                }
            }

          tokensPush(tokens,
                     newToken(TOKEN_FLOAT, start, length, line, tok_column));
        }
      else
        {
          tokensPush(tokens,
                     newToken(TOKEN_INTEGER, start, length, line, tok_column));
        }
    }

  (*pos) -= bytes_read;
  (*column)--;

  return 0;
}

int
tokenize(uint8_t *source, Tokens *tokens, utf8proc_ssize_t source_len)
{
  uint8_t *pos     = source;
  char *line_start = (char *)source;
  int column       = 1;
  int line         = 1;
  int error        = 0;

  tokensInit(tokens);

  utf8proc_ssize_t bytes_read;
  int32_t codepoint;
  while (bytes_read = utf8proc_iterate(pos, source_len, &codepoint),
         codepoint != 0)
    {
      switch (codepoint)
        {
        case ' ':
        case '\t':
        case '\r':
          break;
        case '\n':
          line_start = (char *)(pos + bytes_read);
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
            if (isdigit(codepoint))
              {
                if (number(line_start, &pos, &column, line, tokens))
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

      pos += bytes_read;
      column++;
    }

  return error;
}
