/* This file is the tokenizer for mitchell, which breaks up the input file
 * into a stram of tokens for use by the parser.  I am generally a big fan
 * of using automated tools to generate lexers, but I've decided to make
 * this one by hand since I don't believe flex supports wide characters
 * and also because it needs to be as simple as possible for future
 * reimplementation in the language itself.
 *
 * $Id: tokenize.c,v 1.29 2005/07/14 03:02:53 chris Exp $
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
#include "translate.h"

/* Maps a token value to an identifying string, for error reporting
 * purposes.  Note that the order of these strings must match up with the
 * order of the token_val_t structure in tokens.h.
 */
const wchar_t *token_map[] = {
   L"BOOLEAN", L"EXN", L"IDENTIFIER", L"INTEGER", L"LIST", L"STRING",

   L"ASSIGN", L"BOTTOM", L"CASE", L"COLON", L"COMMA", L"DECL", L"DOT", L"ELSE",
   L"END", L"FUNCTION", L"HANDLE", L"IF", L"IN", L"LBRACE", L"LBRACK",
   L"LPAREN", L"MAPSTO", L"MODULE", L"PIPE", L"RAISE", L"RBRACE", L"RBRACK",
   L"RPAREN", L"THEN", L"TYPE", L"VAL",
   
   L"COMMENT", L"DBLQUOTE", L"ENDOFFILE"};

static unsigned int lineno = 1;
static unsigned int column = 0;
static wchar_t *reserved = L"←⊥:,.ℰƒ{[(→ℳ|}])τʋ#\"";

/* Is this one of our reserved characters? */
static __inline__ unsigned int is_reserved (wchar_t ch)
{
   return wcschr (reserved, ch) != NULL;
}

/* Function to test if a character is a valid number character. */
static unsigned int number_member (wint_t ch)
{
   return !iswspace (ch) && !is_reserved (ch) &&
          (iswdigit(ch) || iswlower(ch) == L'x');
}

/* Function to test if a character is a valid word character. */
static unsigned int word_member (wint_t ch)
{
   return !iswspace (ch) && !is_reserved (ch);
}

/* Read one wide char from the input file, checking for errors in the
 * conversion.  Returns the character read.
 */
static __inline__ wint_t read_char (FILE *f)
{
   wint_t ch = fgetwc (f);

   if (errno == EILSEQ)
   {
      ERROR (_("Unable to read character.  Please check that $LANG is set to a "
               "UTF-8 aware\n\tlocale and that mitchell was compiled with "
               "gcc-3.4 or more recent.\n\tExiting."));
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
      ERROR (_("Unable to read character.  Please check that $LANG is set to a "
               "UTF-8 aware\n\tlocale and that mitchell was compiled with "
               "gcc-3.4 or more recent.\n\tExiting."));
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

/* Strings just got more complicated and now need their own function what with
 * all the escaping and action.
 */
static mstring_t *string_state (FILE *f)
{
   mstring_t  *retval = NULL;
   wint_t      ch;
   int         new_len = 2;

   MALLOC (retval, sizeof(mstring_t));

   while ((ch = read_char (f)) != WEOF)
   {
      if (ch == L'"')
         break;

      REALLOC (retval, sizeof(wchar_t)*new_len);

      if (ch == L'\\')
      {
         if ((ch = read_char (f)) == WEOF)
         {
            fclose(f);
            PARSE_ERROR (cconfig.filename, lineno, column, N_("Premature end "
                         "of file while reading string.\n"));
         }

         switch (ch) {
            case L'n':
               retval[new_len-2] = '\n';
               break;

            case L't':
               retval[new_len-2] = '\t';
               break;

            case L'u':
            {
               mint_t n;
               wchar_t str[4];

               if ((str[0] = read_char(f)) == WEOF ||
                   (str[1] = read_char(f)) == WEOF ||
                   (str[2] = read_char(f)) == WEOF ||
                   (str[3] = read_char(f)) == WEOF)
               {
                  fclose(f);
                  PARSE_ERROR (cconfig.filename, lineno, column, N_("Premature "
                               "end of file while reading unicode character "
                               "escape sequence.\n"));
               }

               if (!iswxdigit (str[0]) || !iswxdigit(str[1]) ||
                   !iswxdigit (str[2]) || !iswxdigit(str[3]))
               {
                  fclose(f);
                  PARSE_ERROR (cconfig.filename, lineno, column, N_("Invalid "
                               "unicode character escape sequence.\n"));
               }

               n = wcstol (str, NULL, 16);

               if (errno == ERANGE || errno == EINVAL)
               {
                  fclose(f);
                  PARSE_ERROR (cconfig.filename, lineno, column, N_("Invalid "
                               "unicode character escape sequence.\n"));
               }

               retval[new_len-2] = n;
               break;
            }

            case L'\n':
            case L'\r':
               whitespace_state (f);
               if ((ch = read_char (f)) == WEOF)
               {
                  fclose(f);
                  PARSE_ERROR (cconfig.filename, lineno, column, N_("Premature "
                               "end of file while reading string whitespace "
                               "escape sequence.\n"));
               }

               if (ch != L'\\')
               {
                  fclose(f);
                  PARSE_ERROR (cconfig.filename, lineno, column, N_("String "
                               "whitespace escape sequences must end with "
                               "'\\'.\n"));
               }

               new_len--;
               break;
	       
            case L'\\':
            case L'"':
            default:
               retval[new_len-2] = ch;
               break;
         }
      }
      else
         retval[new_len-2] = ch;

      retval[new_len-1] = L'\0';

      new_len++;
   }

   return retval;
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

         case L'ℰ':
            retval->lineno = lineno;
            retval->column = column;
            retval->type = EXN;
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
            retval->string = string_state(f);
            return retval;

         case L'0' ... L'9':
         {
            mint_t n;
            mstring_t *str;

            retval->lineno = lineno;
            retval->column = column;
            retval->type = INTEGER;

            unget_char (ch, f);
            str = word_state (f, number_member);
            n = wcstol (str, NULL, 0);

            if (n == 0 && (errno == ERANGE || errno == EINVAL))
            {
               fclose(f);
               MITCHELL_INTERNAL_ERROR (cconfig.filename, __FILE__, __LINE__,
                                        N_("Could not perform numeric "
                                           "conversion on string: %ls\n"),
                                        str);
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
            else if (wcscmp (str, L"handle") == 0)
               retval->type = HANDLE;
            else if (wcscmp (str, L"if") == 0)
               retval->type = IF;
            else if (wcscmp (str, L"in") == 0)
               retval->type = IN;
            else if (wcscmp (str, L"list") == 0)
               retval->type = LIST;
            else if (wcscmp (str, L"raise") == 0)
               retval->type = RAISE;
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
