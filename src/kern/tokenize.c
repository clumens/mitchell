#include <errno.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>
#include <wctype.h>

#include "tokens.h"

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

static __inline__ unsigned int is_reserved (wchar_t ch)
{
   return ch == L'#' || ch == L'(' || ch == L')' || ch == L'[' ||
          ch == L']' || ch == L',' || ch == L'"';
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

/* We've seen a single non-reserved character, which puts us into the word
 * reading state.  Gather up all the characters until the next reserved one
 * and return those as a string.
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
