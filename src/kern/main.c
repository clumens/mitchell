/* The main file of the mitchell kernel, which controls the entire
 * compilation process.
 *
 * $Id: main.c,v 1.7 2004/10/15 14:36:50 chris Exp $
 */

/* mitchell - the bootstrapping compiler
 * Copyright (C) 2004 Chris Lumens
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>

extern void parse (const char *filename);

int main (int argc, char **argv)
{
   /* Grab locale information from the environment. */
   setlocale (LC_ALL, "");

   if (argc != 2)
   {
      fprintf (stderr, "usage: %s <file>\n", argv[0]);
      exit(1);
   }

   parse (argv[1]);

   return 0;
}

/* vim: set tags=../tags: */
