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
      }

      free (t);
   }

   fclose (in);
   return 0;
}
