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

char *
readLine(void)
{
  char *source   = NULL;
  size_t bufsize = 0;
  if (getline(&source, &bufsize, stdin) == EOF)
    {
      free(source);
      return NULL;
    }
  return source;
}

char *
readFile(char *path)
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
  return buffer;
}

void
compile(char *source)
{
  Tokens tokens = tokenize(source);
  tokensFree(&tokens);
  free(source);
}

int
main(int argc, char **argv)
{
  char *source;

  if (argc == 2)
    {
      source = readFile(argv[1]);
      compile(source);
    }
  else
    {
      while (1)
        {
          printf("> ");

          source = readLine();
          if (source == NULL)
            {
              break;
            }
          compile(source);
        }
    }

  return 0;
}
