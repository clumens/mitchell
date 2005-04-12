/* Generic string handling functions.
 *
 * $Id: str.c,v 1.4 2005/04/12 01:13:01 chris Exp $
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

/* Convert a unicode string representing a mitchell identifier into an ascii
 * string representing an assembler identifier.  We do this by converting all
 * characters not recognized by an assembler into the sequence .Uxxxx, where
 * xxxx is the hex representation of the unicode character.
 */
char *unicode_to_ascii (mstring_t *sym)
{
   char *retval = NULL;
   wint_t tmp;

   for (tmp = *sym; tmp != L'\0'; tmp = *++sym)
   {
      if (wcschr (L"abcdefghijklmnopqrstuvwxyz0123456789_",
                  towlower(tmp)) != NULL)
      {
         char buf[2] = { tmp, '\0' };

         if (retval == NULL)
         {
            MALLOC (retval, sizeof(char)*2);
            retval = strcpy (retval, buf);
         }
         else
         {
            REALLOC (retval, strlen(retval)+sizeof(char)*2);
            retval = strcat (retval, buf);
         }
      }
      else
      {
         char buf[7];

         sprintf (buf, ".U%.4X", tmp);
         if (retval == NULL)
         {
            MALLOC (retval, sizeof(char)*7);
            retval = strcpy (retval, buf);
         }
         else
         {
            REALLOC (retval, strlen(retval)+sizeof(char)*7);
            retval = strcat (retval, buf);
         }
      }
   }

   return retval;
}
