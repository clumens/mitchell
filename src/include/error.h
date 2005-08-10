/* Error message formatting macros so all the messages at least look a little
 * bit like each other.  Some consistency is good.
 *
 * $Id: error.h,v 1.15 2005/08/10 01:40:09 chris Exp $
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

#include <stdarg.h>

/* Errors, which should be followed by whatever other text needs to be printed
 * and then an exit(1).
 */
void BAD_SYMBOL_ERROR (char *file, unsigned int line, unsigned int column, wchar_t *sym, char *msg);
void COULD_NOT_READ_ERROR (char *file);
void COULD_NOT_WRITE_ERROR (char *file);
void ERROR (const char *format, ...) __attribute__ ((format (printf, 1, 2)));
void ERROR_IN_FILE (char *file, unsigned int line, unsigned int column, char *msg);
void FCLOSE_ERROR (char *file);
void MITCHELL_INTERNAL_ERROR (char *file, char *srcfile, unsigned int line, const char *format, ...)
                             __attribute__ ((format (printf, 4, 5)));
void NONEXHAUSTIVE_MATCH_ERROR (char *file, unsigned int line, unsigned int column);
void PARSE_ERROR (char *file, unsigned int line, unsigned int column, const char *format, ...)
                 __attribute__ ((format (printf, 4, 5)));
void TYPE_LOOP_ERROR (char *file, unsigned int line, unsigned int column, wchar_t *ty);
void TYPE_ERROR (char *file, unsigned int line, unsigned int column, char *msg, char *ty1_msg, wchar_t *ty1,
                 char *ty2_msg, wchar_t *ty2);

/* Warnings, which do not stop compilation. */
void NONEXHAUSTIVE_MATCH_WARNING (char *file, unsigned int line, unsigned int column);

#endif

/* vim: set tags=../tags: */
