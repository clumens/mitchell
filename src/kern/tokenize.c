/* This file is the tokenizer for mitchell, which breaks up the input file
 * into a stram of tokens for use by the parser.  I am generally a big fan
 * of using automated tools to generate lexers, but I've decided to make
 * this one by hand since I don't believe flex supports wide characters
 * and also because it needs to be as simple as possible for future
 * reimplementation in the language itself.
 *
 * $Id: tokenize.c,v 1.4 2004/09/02 15:29:58 chris Exp $
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
#include <errno.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>
#include <wctype.h>

#include "tokens.h"

static wchar_t *reserved = L"#()[].\"ƒ";

#define MALLOC(ptr, size) \
   if (((ptr) = malloc(size)) == NULL) \
   { \
      fprintf (stderr, "could not malloc at %s: %d\n", __FILE__, __LINE__); \
      exit (1); \
   }

#define REALLOC(ptr, size) \
   if (((ptr) = realloc(ptr, size)) == NULL) \
   { \
      fprintf (stderr, "could not realloc at %s: %d\n", __FILE__, __LINE__); \
      exit (1); \
   }

/* Is this one of our reserved characters? */
static __inline__ unsigned int is_reserved (wchar_t ch)
{
   return wcschr (reserved, ch) != NULL;
}

/* Read one wide char from the input file, checking for errors in the
 * conversion.  Returns the character read.
 */
static __inline__ wint_t read_char (FILE *f)
{
   wint_t ch = fgetwc (f);

   if (errno == EILSEQ)
   {
      fprintf (stderr, "fgetwc returned EILSEQ\n");
      fclose (f);
      exit (1);
   }

   return ch;
}

/* Put a character back into the file stream. */
static __inline__ void unget_char (wint_t ch, FILE *f)
{
   if (ungetwc (ch, f) == WEOF && errno == EILSEQ)
   {
      fprintf (stderr, "ungetwc returned EILSEQ\n");
      fclose (f);
      exit (1);
   }
}

/* A comment extends from the comment marker to the end of the line. */
static __inline__ void comment_state (FILE *f)
{
   wint_t ch;

   while ((ch = read_char (f)) != WEOF && ch != L'\n')
      ;
}

/* Skip all the whitespace, putting the first non-whitespace back into the
 * input stream so it can be read as part of the next token.
 */
static void whitespace_state (FILE *f)
{
   wint_t ch;

   while ((ch = read_char (f)) != WEOF)
   {
      if (!iswspace (ch))
      {
         unget_char (ch, f);
         return;
      }
   }
}

/* We've seen a character that puts us into one of the special word-reading
 * states.  Gather up all the characters until we see one that is not a
 * member of the current word state's set.
 */
static wchar_t *word_state (FILE *f, unsigned int (*is_member)(wint_t ch))
{
   wchar_t *retval = NULL;
   wint_t   ch;
   int      new_len = 2;

   MALLOC (retval, sizeof(wchar_t))

   while ((ch = read_char (f)) != WEOF)
   {
      if (!is_member(ch))
      {
         unget_char (ch, f);
         break;
      }

      REALLOC (retval, sizeof(wchar_t)*new_len)

      retval[new_len-2] = ch;
      retval[new_len-1] = L'\0';

      new_len++;
   }

   return retval;
}

/* Function to test if a character is a valid number character. */
static unsigned int number_member (wint_t ch)
{
   return !iswspace (ch) && !is_reserved (ch) &&
          (iswdigit(ch) || iswlower(ch) == L'x');
}

/* Function to test if a character is a valid string character. */
static unsigned int string_member (wint_t ch)
{
   return ch != L'"';
}

/* Function to test if a character is a valid word character. */
static unsigned int word_member (wint_t ch)
{
   return !iswspace (ch) && !is_reserved (ch);
}

/* Returns the next token in the previously opened file f or NULL if there
 * are no more tokens available.
 */
token_t *next_token (FILE *f)
{
   token_t *retval;
   wint_t   ch;

   MALLOC (retval, sizeof (token_t))

   while (1)
   {
      ch = read_char (f);

      /* If the next character is whitespace, skip all that to set up for the
       * next token and return the one we just finished reading.
       */
      if (ch == WEOF)
         return NULL;
      else if (iswspace (ch))
      {
         whitespace_state (f);
         continue;
      }

      switch (ch) {
         case L'#':
            comment_state (f);
            continue;

         case L'(':
            retval->type = LPAREN;
            return retval;

         case L')':
            retval->type = RPAREN;
            return retval;

         case L'[':
            retval->type = LBRACK;
            return retval;

         case L']':
            retval->type = RBRACK;
            return retval;

         case L',':
            retval->type = COMMA;
            return retval;

         case L'"':
            retval->type = STRING;
            retval->string = word_state (f, string_member);

            /* word_state put the double quote character back, so read it
             * one more time and throw it away.
             */
            read_char (f);
            return retval;

         case L'ƒ':
            retval->type = FUNCTION;
            return retval;

         case L'0' ... L'9':
         {
            long n;

            unget_char (ch, f);
            n = wcstol (word_state (f, number_member), NULL, 0);

            if (n == 0 && (errno == ERANGE || errno == EINVAL))
            {
               fprintf (stderr, "could not perform number conversion\n");
               fclose (f);
               exit (1);
            }

            retval->type = INTEGER;
            retval->integer = n;
            return retval;
         }

         default:
            unget_char (ch, f);

            retval->type = IDENTIFIER;
            retval->string = word_state (f, word_member);
            return retval;
      }
   }

   return NULL;
}
