/* This file is the tokenizer for mitchell, which breaks up the input file
 * into a stram of tokens for use by the parser.  I am generally a big fan
 * of using automated tools to generate lexers, but I've decided to make
 * this one by hand since I don't believe flex supports wide characters
 * and also because it needs to be as simple as possible for future
 * reimplementation in the language itself.
 *
 * $Id: tokenize.c,v 1.21 2005/01/10 04:53:33 chris Exp $
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
#include <gc.h>
#include <errno.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>
#include <wctype.h>

#include "basic_types.h"
#include "config.h"
#include "error.h"
#include "memory.h"
#include "tokens.h"

/* Maps a token value to an identifying string, for error reporting
 * purposes.  Note that the order of these strings must match up with the
 * order of the token_val_t structure in tokens.h.
 */
const char *token_map[] = {
   "BOOLEAN", "IDENTIFIER", "INTEGER", "LIST", "STRING",

   "ASSIGN", "BOTTOM", "CASE", "COLON", "COMMA", "DECL", "DOT", "ELSE", "END",
   "FUNCTION", "IF", "IN", "LBRACE", "LBRACK", "LPAREN", "MAPSTO", "MODULE",
   "PIPE", "RBRACE", "RBRACK", "RPAREN", "THEN", "TYPE", "VAL",
   
   "COMMENT", "DBLQUOTE", "ENDOFFILE"};

static unsigned int lineno = 1;
static unsigned int column = 0;
static wchar_t *reserved = L"←⊥:,.ƒ{[(→ℳ|}])τʋ#\"";

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
      USAGE_ERROR (compiler_config.filename,
                   "fgetwc returned EILSEQ.  Please check that $LANG is set "
                   "to a UTF-8 aware locale\n\tand that mitchell was compiled "
                   "with gcc-3.4 or more recent.  Exiting.");
      fclose (f);
      exit (1);
   }

   if (ch == L'\n')
   {
      lineno++;
      column = 0;
   }
   else
      column++;

   return ch;
}

/* Put a character back into the file stream. */
static __inline__ void unget_char (wint_t ch, FILE *f)
{
   if (ungetwc (ch, f) == WEOF && errno == EILSEQ)
   {
      USAGE_ERROR (compiler_config.filename,
                   "ungetwc returned EILSEQ.  Please check that $LANG is set "
                   "to a UTF-8 aware locale\n\tand that mitchell was compiled "
                   "with gcc-3.4 or more recent.  Exiting.");
      fclose (f);
      exit (1);
   }

   if (ch == L'\n')
      lineno--;
   else
      column--;
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
static mstring_t *word_state (FILE *f, unsigned int (*is_member)(wint_t ch))
{
   mstring_t  *retval = NULL;
   wint_t      ch;
   int         new_len = 2;

   MALLOC (retval, sizeof(mstring_t));

   while ((ch = read_char (f)) != WEOF)
   {
      if (!is_member(ch))
      {
         unget_char (ch, f);
         break;
      }

      REALLOC (retval, sizeof(wchar_t)*new_len);

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

   MALLOC (retval, sizeof (token_t));

   while (1)
   {
      ch = read_char (f);

      /* If the next character is whitespace, skip all that to set up for the
       * next token and return the one we just finished reading.
       */
      if (ch == WEOF)
      {
         retval->lineno = lineno;
         retval->column = column;
         retval->type = ENDOFFILE;
         return retval;
      }
      else if (iswspace (ch))
      {
         whitespace_state (f);
         continue;
      }

      switch (ch) {
         case L'←':
            retval->lineno = lineno;
            retval->column = column;
            retval->type = ASSIGN;
            return retval;

         case L'⊥':
            retval->lineno = lineno;
            retval->column = column;
            retval->type = BOTTOM;
            return retval;

         case L':':
            retval->lineno = lineno;
            retval->column = column;
            retval->type = COLON;
            return retval;

         case L',':
            retval->lineno = lineno;
            retval->column = column;
            retval->type = COMMA;
            return retval;

         case L'.':
            retval->lineno = lineno;
            retval->column = column;
            retval->type = DOT;
            return retval;

         case L'ƒ':
            retval->lineno = lineno;
            retval->column = column;
            retval->type = FUNCTION;
            return retval;

         case L'{':
            retval->lineno = lineno;
            retval->column = column;
            retval->type = LBRACE;
            return retval;

         case L'[':
            retval->lineno = lineno;
            retval->column = column;
            retval->type = LBRACK;
            return retval;

         case L'(':
            retval->lineno = lineno;
            retval->column = column;
            retval->type = LPAREN;
            return retval;

         case L'→':
            retval->lineno = lineno;
            retval->column = column;
            retval->type = MAPSTO;
            return retval;

         case L'ℳ':
            retval->lineno = lineno;
            retval->column = column;
            retval->type = MODULE;
            return retval;

         case L'|':
            retval->lineno = lineno;
            retval->column = column;
            retval->type = PIPE;
            return retval;

         case L'}':
            retval->lineno = lineno;
            retval->column = column;
            retval->type = RBRACE;
            return retval;

         case L']':
            retval->lineno = lineno;
            retval->column = column;
            retval->type = RBRACK;
            return retval;

         case L')':
            retval->lineno = lineno;
            retval->column = column;
            retval->type = RPAREN;
            return retval;

         case L'τ':
            retval->lineno = lineno;
            retval->column = column;
            retval->type = TYPE;
            return retval;

         case L'ʋ':
            retval->lineno = lineno;
            retval->column = column;
            retval->type = VAL;
            return retval;

         case L'#':
            comment_state (f);
            continue;

         case L'"':
            retval->lineno = lineno;
            retval->column = column;
            retval->type = STRING;
            retval->string = word_state (f, string_member);

            /* word_state put the double quote character back, so read it
             * one more time and throw it away.
             */
            read_char (f);
            return retval;

         case L'0' ... L'9':
         {
            mint_t n;

            retval->lineno = lineno;
            retval->column = column;
            retval->type = INTEGER;

            unget_char (ch, f);
            n = wcstol (word_state (f, number_member), NULL, 0);

            if (n == 0 && (errno == ERANGE || errno == EINVAL))
            {
               MITCHELL_INTERNAL_ERROR(compiler_config.filename,
                                       "Could not perform number conversion");
               fclose (f);
               exit (1);
            }

            retval->integer = n;
            return retval;
         }

         /* Deal with words, which can either be reserved words or something
          * new that the programmer has made up.
          */
         default:
         {
            wchar_t *str;

            retval->lineno = lineno;
            retval->column = column;

            unget_char (ch, f);
            str = word_state (f, word_member);

            if (wcscmp (str, L"f") == 0)
            {
               retval->type = BOOLEAN;
               retval->boolean = 0;
            }
            else if (wcscmp (str, L"t") == 0)
            {
               retval->type = BOOLEAN;
               retval->boolean = 1;
            }
            else if (wcscmp (str, L"case") == 0)
               retval->type = CASE;
            else if (wcscmp (str, L"decl") == 0)
               retval->type = DECL;
            else if (wcscmp (str, L"else") == 0)
               retval->type = ELSE;
            else if (wcscmp (str, L"end") == 0)
               retval->type = END;
            else if (wcscmp (str, L"if") == 0)
               retval->type = IF;
            else if (wcscmp (str, L"in") == 0)
               retval->type = IN;
            else if (wcscmp (str, L"list") == 0)
               retval->type = LIST;
            else if (wcscmp (str, L"then") == 0)
               retval->type = THEN;
            else
            {
               retval->type = IDENTIFIER;
               retval->string = str;
            }

            return retval;
         }
      }
   }

   return NULL;
}

/* vim: set tags=../tags: */
