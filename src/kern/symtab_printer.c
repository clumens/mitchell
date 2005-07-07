/* Pretty printer for the symbol tables.
 *
 * $Id: symtab_printer.c,v 1.12 2005/07/07 05:04:20 chris Exp $
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "config.h"
#include "basic_types.h"
#include "semant.h"
#include "symtab.h"
#include "translate.h"

#define STR(s)  (s) == NULL ? L"" : (s)

/* Print a single symbol. */
void symbol_dump (FILE *f, symbol_t *sym)
{
   if (sym == NULL)
      return;

   switch (sym->kind) {
      case SYM_EXN:
         fprintf (f, "EXN{%ls,%ls}:%ls", STR(sym->name), STR(sym->label),
                     sym->info.ty == NULL ? L"" : ty_to_str(sym->info.ty));
         break;

      case SYM_FUNCTION:
         fprintf (f, "FUNCTION{%ls,%ls}:%ls", STR(sym->name), STR(sym->label),
                     ty_to_str(sym->info.function->retval));
         break;

      case SYM_MODULE:
         fprintf (f, "MODULE{%ls,%ls}", STR(sym->name), STR(sym->label));
         break;
      
      case SYM_TYPE:
         fprintf (f, "TYPE{%ls,%ls}:%ls", STR(sym->name), STR(sym->label),
                     sym->info.ty == NULL ? L"" : ty_to_str(sym->info.ty));
         break;
         
      case SYM_VALUE:
         fprintf (f, "VALUE{%ls,%ls}:%ls", STR(sym->name), STR(sym->label),
                     sym->info.ty == NULL ? L"" : ty_to_str(sym->info.ty));
         break;

      default:
         break;
   }
}

/* Print the single symbol table. */
void table_dump (FILE *f, symtab_t *symtab, mstring_t *scope_name)
{
   symtab_entry_t *tmp;
   unsigned int i;

   fprintf (f, "----------------------------------------\n");
   fprintf (f, _("Leaving scope for symbol: %ls\n"), scope_name);

   for (i = 0; i < SYMTAB_ROWS; i++)
   {
      tmp = (*symtab)[i];
      
      if (tmp != NULL)
      {
         fprintf (f, "[%2d] => ", i);

         while (tmp != NULL)
         {
            symbol_dump (f, tmp->symbol);
            tmp = tmp->next;

            if (tmp != NULL)
               fprintf (f, ", ");
         }

         fprintf (f, "\n");
      }
   }
}

/* Print a stack of symbol tables, starting from the innermost scope and
 * proceeding to the outermost one.  f should be an already open filehandle.
 */
void symtab_dump (FILE *f, tabstack_t *tabstack, mstring_t *scope_name)
{
   tabstack_t *tmp = tabstack;

   fprintf (f, "========================================\n");
   fprintf (f, _("Leaving module: %ls\n"), scope_name);

   while (tmp != NULL)
   {
      table_dump (f, tmp->symtab, scope_name);
      tmp = tmp->upper;
   }
}

/* vim: set tags=../tags: */
