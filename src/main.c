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

#include <stdio.h>
#include <stdlib.h>

char *
readLine(void)
{
  char *line     = NULL;
  size_t bufsize = 0;
  if (getline(&line, &bufsize, stdin) == EOF)
    {
      free(line);
      return NULL;
    }
  return line;
}

int
main(void)
{
  char *line;

  while (1L)
    {
      printf("> ");

      line = readLine();
      if (line == NULL)
        {
          printf("\n");
          break;
        }

      printf("%s", line);
      free(line);
    }

  return 0;
}
