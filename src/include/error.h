/* Error message formatting macros so all the messages at least look a little
 * bit like each other.  Some consistency is good.
 *
 * $Id: error.h,v 1.11 2005/03/29 05:52:52 chris Exp $
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
#ifndef _ERROR_H
#define _ERROR_H 1

/* Errors, which should be followed by whatever other text needs to be printed
 * and then an exit(1).
 */
#define BAD_SYMBOL_ERROR(file, line, column, sym, msg) \
   fprintf (stderr, "%s:%d.%d Error:\n\t%ls: %s\n", (file), (line), (column), \
                    (sym), (msg))

#define COULD_NOT_OPEN_ERROR(file, mode) \
   fprintf (stderr, "%s Error:  could not open file for %s\n", (file), (mode))

#define ERROR(msg) \
   fprintf (stderr, "Error:  %s\n", (msg))

#define ERROR_IN_FILE(file, line, column, msg) \
   fprintf (stderr, "%s:%d.%d Error:  %s\n", (file), (line), (column), (msg))

#define FCLOSE_ERROR(file) \
   fprintf (stderr, "%s Error:  could not close file\n", (file))

#define MITCHELL_INTERNAL_ERROR(in_file, msg) \
   fprintf (stderr, "%s Mitchell Compiler Internal Error:\n" \
                    "%s:%d: %s\n", (in_file), __FILE__, __LINE__, (msg));

#define NONEXHAUSTIVE_MATCH_ERROR(file, line, column) \
   fprintf (stderr, "%s:%d.%d Error:  non-exhaustive match in case-expr - " \
                    "recommend adding an else branch\n", (file), (line), \
                    (column))

#define PARSE_ERROR(file, line, column) \
   fprintf (stderr, "%s:%d.%d Error:  parse error\n", (file), (line), (column))

#define TYPE_LOOP_ERROR(file, line, column, ty) \
   fprintf (stderr, "%s:%d.%d Error:  type check error: symbol is in an " \
                    "infinite type loop:  %ls\n", (file), (line), (column), \
                    (ty))

#define TYPE_ERROR(file, line, column, msg, ty1_msg, ty1, ty2_msg, ty2) \
   fprintf (stderr, "%s:%d.%d Error:  type check error: %s\n" \
                    "\t%s type: %ls\n\t%s type: %ls\n", (file), (line), \
                    (column), msg, ty1_msg, (ty1), ty2_msg, (ty2))

/* Warnings, which do not stop compilation. */
#define NONEXHAUSTIVE_MATCH_WARNING(file, line, column) \
   fprintf (stderr, "%s:%d.%d Warning:  non-exhaustive match in case-expr " \
                    "- recommend adding an else branch to prevent runtime " \
                    "errors\n", (file), (line), (column))

#define WARNINGS_AS_ERRORS() \
   do { \
      if (cconfig.warnings_are_errors) \
      { \
         fprintf (stderr, "Handling warnings as errors.  Exiting.\n"); \
         exit(1); \
      } \
   } while (0)

#endif

/* vim: set tags=../tags: */
