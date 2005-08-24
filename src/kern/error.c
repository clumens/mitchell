/* Error message formatting macros so all the messages at least look a little
 * bit like each other.  Some consistency is good.
 *
 * $Id: error.c,v 1.6 2005/08/23 23:28:39 chris Exp $
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
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "config.h"
#include "error.h"
#include "translate.h"

static void _error (char *file, unsigned int line, unsigned int column)
{
   fprintf (stderr, _("%s:%d.%d Error: "), file, line, column);
}

static void _warning (char *file, unsigned int line, unsigned int column)
{
   fprintf (stderr, _("%s:%d.%d Warning: "), file, line, column);
}

static void WARNINGS_AS_ERRORS()
{
   if (cconfig.warnings_are_errors)
   {
      fflush(stdout);
      fprintf (stderr, _("Handling warnings as errors.  Exiting.\n"));
      exit(1);
   }
}

/* Errors, which should be followed by whatever other text needs to be printed
 * and then an exit(1).
 */
void BAD_SYMBOL_ERROR (char *file, unsigned int line, unsigned int column, wchar_t *sym, char *msg)
{
   fflush(stdout);
   _error (file, line, column);
   fprintf (stderr, "\n\t%ls: %s\n", sym, msg);
}

void COULD_NOT_READ_ERROR (char *file)
{
   fflush(stdout);
   fprintf (stderr, _("%s Error: Could not open file for reading.\n"), file);
}

void COULD_NOT_WRITE_ERROR (char *file)
{
   fflush(stdout);
   fprintf (stderr, _("%s Error: Could not open file for writing.\n"), file);
}

void ERROR (const char *format, ...)
{
   va_list ap;
   
   fflush(stdout);
   fprintf (stderr, _("Error: "));

   va_start (ap, format);
   vfprintf (stderr, _(format), ap);
   va_end (ap);

   exit(1);
}

void ERROR_IN_FILE (char *file, unsigned int line, unsigned int column, char *msg)
{
   fflush(stdout);
   _error (file, line, column);
   fprintf (stderr, "%s\n", msg);
}

void FCLOSE_ERROR (char *file)
{
   fflush(stdout);
   fprintf (stderr, _("%s Error: Could not close file.\n"), file);
}

void MITCHELL_INTERNAL_ERROR (char *file, char *srcfile, unsigned int line, const char *format, ...)
{
   va_list ap;
   
   fflush(stdout);
   fprintf (stderr, _("%s Mitchell internal compiler error: %s:%d: "), file, srcfile, line);

   va_start (ap, format);
   vfprintf (stderr, _(format), ap);
   va_end (ap);

   exit(1);
}

void NONEXHAUSTIVE_MATCH_ERROR (char *file, unsigned int line, unsigned int column)
{
   fflush(stdout);
   _error (file, line, column);
   fprintf (stderr, _("Non-exhaustive match in case expression.  Adding an "
                      "else branch is recommended to avoid runtime errors.\n"));
}

void PARSE_ERROR (char *file, unsigned int line, unsigned int column, const char *format, ...)
{
   va_list ap;
   
   fflush(stdout);
   _error (file, line, column);
   fprintf (stderr, _("Parse error on input file.\n\t"));

   va_start (ap, format);
   vfprintf (stderr, _(format), ap);
   va_end (ap);

   exit(1);   
}

void TYPE_LOOP_ERROR (char *file, unsigned int line, unsigned int column, wchar_t *ty)
{
   fflush(stdout);
   _error (file, line, column);
   fprintf (stderr, _("Type check error: The following symbol is in an "
                      "infinite loop of type definitions: %ls\n"), ty);
   exit(1);
}

void TYPE_ERROR (char *file, unsigned int line, unsigned int column, char *msg, char *ty1_msg, wchar_t *ty1,
                 char *ty2_msg, wchar_t *ty2)
{
   fflush(stdout);
   _error (file, line, column);
   fprintf (stderr, _("Type check error: %s\n\t%s: %ls\n\t%s: %ls\n"), msg, ty1_msg, ty1, ty2_msg, ty2);
   exit(1);
}

/* Warnings, which do not stop compilation (unless you want them to). */
void NONEXHAUSTIVE_MATCH_WARNING (char *file, unsigned int line, unsigned int column)
{
   fflush(stdout);
   _warning (file, line, column);
   fprintf (stderr, _("Non-exhaustive match in case expression.  Adding an "
                      "else branch is recommended to avoid runtime errors.\n"));
   WARNINGS_AS_ERRORS();
}

/* vim: set tags=../tags: */
