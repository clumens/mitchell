/* Pretty printer for the symbol tables.
 *
 * $Id: symtab_printer.c,v 1.6 2004/12/22 00:33:51 chris Exp $
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

/* Print a single symbol. */
void symbol_dump (symbol_t *sym)
{
   if (sym == NULL)
      return;

   switch (sym->kind) {
      case SYM_FUNCTION:
         printf ("FUNCTION(%ls)", sym->name);
         break;

      case SYM_MODULE:
         printf ("MODULE(%ls)", sym->name);
         break;
      
      case SYM_TYPE:
         printf ("TYPE(%ls):%ls", sym->name,
                 sym->info.ty == NULL ? L"" : ty_to_str(sym->info.ty));
         break;
         
      case SYM_VALUE:
         printf ("VALUE(%ls):%ls", sym->name,
                 sym->info.ty == NULL ? L"" : ty_to_str(sym->info.ty));
         break;
   }
}

/* Print the single symbol table. */
void table_dump (symtab_t *symtab, mstring_t *scope_name)
{
   symtab_entry_t *tmp;
   unsigned int i;

   printf ("----------------------------------------\n");
   printf ("Leaving scope for symbol: %ls\n", scope_name);

   for (i = 0; i < SYMTAB_ROWS; i++)
   {
      tmp = (*symtab)[i];
      
      if (tmp != NULL)
      {
         printf ("[%2d] => ", i);

         while (tmp != NULL)
         {
            symbol_dump (tmp->symbol);
            tmp = tmp->next;

            if (tmp != NULL)
               printf (", ");
         }

         printf ("\n");
      }
   }
}

/* Print a stack of symbol tables, starting from the innermost scope and
 * proceeding to the outermost one.
 */
void symtab_dump (tabstack_t *tabstack, mstring_t *scope_name)
{
   tabstack_t *tmp = tabstack;

   printf ("========================================\n");
   printf ("Leaving module: %ls\n", scope_name);

   while (tmp != NULL)
   {
      table_dump (tmp->symtab, scope_name);
      tmp = tmp->upper;
   }
}
