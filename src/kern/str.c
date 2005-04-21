/* Generic string handling functions.
 *
 * $Id: str.c,v 1.6 2005/04/21 02:49:52 chris Exp $
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
#include <wctype.h>

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

static unsigned long int unique_id = 0;

/* Construct a new string that is guaranteed to not overlap with anything the
 * source file contains.  These strings all start with "_.".  Since there's a
 * period in the symbol name, we're guaranteed it won't conflict.
 */
mstring_t *make_unique_str (mstring_t *base)
{
   mstring_t *retval;
   char *buf;

   MALLOC (buf, sizeof(char)*10);
   snprintf (buf, 9, "%ld", unique_id);

   MALLOC (retval, sizeof(mstring_t));
   retval = build_wcsstr (4, L"_.", base, L"_", buf);

   unique_id++;

   return retval;
}

/* Convert a unicode string representing a mitchell identifier into an ascii
 * string representing an assembler identifier.  We do this by converting all
 * characters not recognized by an assembler into the sequence .Uxxxx, where
 * xxxx is the hex representation of the unicode character.  We still represent
 * the resulting string as an mstring_t, just for simplicity.
 */
mstring_t *unicode_to_ascii (mstring_t *sym)
{
   mstring_t *retval = NULL;
   wint_t tmp;

   for (tmp = *sym; tmp != L'\0'; tmp = *++sym)
   {
      if (wcschr (L"abcdefghijklmnopqrstuvwxyz0123456789_",
                  towlower(tmp)) != NULL)
      {
         wchar_t buf[2] = { tmp, L'\0' };

         if (retval == NULL)
         {
            MALLOC (retval, sizeof(wchar_t)*2);
            retval = wcscpy (retval, buf);
         }
         else
         {
            REALLOC (retval, WCSLEN(retval)+sizeof(wchar_t)*2);
            retval = wcscat (retval, buf);
         }
      }
      else
      {
         wchar_t buf[7];

         swprintf (buf, 7, L".U%.4lX", tmp);
         if (retval == NULL)
         {
            MALLOC (retval, sizeof(wchar_t)*7);
            retval = wcscpy (retval, buf);
         }
         else
         {
            REALLOC (retval, WCSLEN(retval)+sizeof(wchar_t)*7);
            retval = wcscat (retval, buf);
         }
      }
   }

   return retval;
}
