/* Generic string handling functions.
 *
 * $Id: str.c,v 1.2 2005/02/08 05:24:59 chris Exp $
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
#define _GNU_SOURCE
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

#include "basic_types.h"
#include "memory.h"
#include "str.h"

#define WCSLEN(str)  (wcslen(str)*sizeof(wchar_t))

wchar_t *build_wcsstr (int nargs, ...)
{
   va_list ap;
   unsigned int i;
   wchar_t *retval = NULL;
   wchar_t *init = NULL;

   if (nargs == 0)
      return NULL;

   va_start (ap, nargs);
   init = va_arg (ap, wchar_t *);

   MALLOC(retval, WCSLEN(init));
   retval = wcscpy (retval, init);

   for (i = 1; i < nargs; i++)
   {
      wchar_t *next = va_arg(ap, wchar_t *);

      REALLOC (retval, WCSLEN(retval)+WCSLEN(next));
      retval = wcscat (retval, next);
   }

   va_end (ap);
   return retval;
}
