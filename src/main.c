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
#include <utf8proc.h>

#define UTF8PROC_OPTIONS                                                      \
  (UTF8PROC_NULLTERM | UTF8PROC_COMPOSE | UTF8PROC_IGNORE)

utf8proc_ssize_t
readLine(uint8_t **dest)
{
  char *source   = NULL;
  size_t bufsize = 0;
  if (getline(&source, &bufsize, stdin) == EOF)
    {
      free(source);
      *dest = NULL;
      return 0;
    }

  utf8proc_ssize_t size
      = utf8proc_map((uint8_t *)source, bufsize, dest, UTF8PROC_OPTIONS);
  free(source);

  return size;
}

utf8proc_ssize_t
readFile(char *path, uint8_t **dest)
{
  FILE *file = fopen(path, "r");
  size_t size;
  char *buffer;
  size_t bytes_read;

  if (file == NULL)
    {
      fprintf(stderr, "Could not open file '%s'\n", path);
      exit(1);
    }

  fseek(file, 0L, SEEK_END);
  size = ftell(file);
  rewind(file);

  buffer = malloc(size + 1);
  if (buffer == NULL)
    {
      fprintf(stderr, "Could not allocate memory for file '%s'\n", path);
      exit(1);
    }

  bytes_read = fread(buffer, sizeof(char), size, file);
  if (bytes_read < size)
    {
      fprintf(stderr, "Could not read file '%s'\n", path);
      exit(1);
    }

  buffer[size] = '\0';

  fclose(file);
  utf8proc_ssize_t utf8_size = utf8proc_map(
      (uint8_t *)buffer, size, (uint8_t **)dest, UTF8PROC_OPTIONS);
  free(buffer);
  return utf8_size;
}

void
compile(uint8_t *source, utf8proc_ssize_t source_len)
{
  Tokens tokens;
  int i;

  if (tokenize(source, &tokens, source_len))
    {
      exit(1);
    }

  for (i = 0; i < tokens.length; i++)
    {
      Token token = tokens.tokens[i];
      printf("%d:%d \"%.*s\"\n", token.line, token.column, token.length,
             token.start);
    }

  tokensFree(&tokens);
  free(source);
}

int
main(int argc, char **argv)
{
  uint8_t *source;
  utf8proc_ssize_t source_length;

  if (argc == 2)
    {
      source_length = readFile(argv[1], &source);
      compile(source, source_length);
    }
  else
    {
      while (1)
        {
          printf("> ");

          source_length = readLine(&source);
          if (source == NULL)
            {
              break;
            }
          compile(source, source_length);
        }
    }

  return 0;
}
