/* Error message formatting macros so all the messages at least look a little
 * bit like each other.  Some consistency is good.
 *
 * $Id: error.h,v 1.1 2004/11/24 20:45:38 chris Exp $
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

#define BAD_SYMBOL_ERROR(file, line, msg, sym) \
   fprintf (stderr, "%s:%d Error:\n\t%ls: %s\n", (file), (line), (msg), (sym))

#define COULD_NOT_OPEN_ERROR(file, mode) \
   fprintf (stderr, "%s Error:  could not open file for %s\n", (file), (mode))

#define FCLOSE_ERROR(file) \
   fprintf (stderr, "%s Error:  could not close file\n", (file))

#define MITCHELL_INTERNAL_ERROR(in_file, msg) \
   fprintf (stderr, "%s Mitchell Compiler Internal Error:\n" \
                    "%s:%d: %s\n", (in_file), __FILE__, __LINE__, (msg));

#define PARSE_ERROR(file, line) \
   fprintf (stderr, "%s:%d Error:  parse error\n", (file), (line))

#define USAGE_ERROR(file, msg) \
   fprintf (stderr, "%s Error:\n\t%s\n", (file), (msg))

#endif

/* vim: set tags=../tags: */
