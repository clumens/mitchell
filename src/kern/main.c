/* The main file of the mitchell kernel, which controls the entire
 * compilation process.
 *
 * $Id: main.c,v 1.4 2004/10/13 02:47:24 chris Exp $
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

#include "tokens.h"

int main (int argc, char **argv)
{
   token_t *t;
   FILE *in;

   /* Grab locale information from the environment. */
   setlocale (LC_ALL, "");

   if (argc != 2)
   {
      fprintf (stderr, "usage: %s <file>\n", argv[0]);
      exit(1);
   }

   if ((in = fopen (argv[1], "r")) == NULL)
   {
      fprintf (stderr, "could not open for reading: %s\n", argv[1]);
      exit(1);
   }

   /* Read in each token of the input file and print it out. */
   while ((t = next_token (in)) != NULL)
   {
      switch (t->type) {
         case LPAREN:
            printf ("LPAREN\n");
            break;

         case RPAREN:
            printf ("RPAREN\n");
            break;

         case LBRACK:
            printf ("LBRACK\n");
            break;

         case RBRACK:
            printf ("RBRACK\n");
            break;

         case COMMA:
            printf ("COMMA\n");
            break;

         case DBLQUOTE:
            printf ("DBLQUOTE\n");
            break;

         case IDENTIFIER:
            printf ("IDENTIFIER: |%ls|\n", t->string);
            free (t->string);
            break;

         case INTEGER:
            printf ("INTEGER: |%li|\n", t->integer);
            break;

         case STRING:
            printf ("STRING: |%ls|\n", t->string);
            free (t->string);
            break;

         case BOOLEAN:
            printf ("BOOLEAN: |%s|\n", t->boolean == 1 ? "true" : "false");
            break;

         case FUNCTION:
            printf ("FUNCTION\n");
            break;
      }

      free (t);
   }

   fclose (in);
   return 0;
}
