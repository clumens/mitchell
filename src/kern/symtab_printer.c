/* Pretty printer for the symbol tables.
 *
 * $Id: symtab_printer.c,v 1.2 2004/11/24 03:56:04 chris Exp $
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
#include "symtab.h"

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
         printf ("row %2d:\t", i);

         while (tmp != NULL)
         {
            switch (tmp->symbol->kind) {
               case SYM_FUNVAL: printf ("FUNVAL("); break;
               case SYM_MODULE: printf ("MODULE("); break;
               case SYM_TYPE: printf ("TYPE("); break;
            }
            
            printf ("%ls), ", tmp->symbol->name);
            tmp = tmp->next;
         }

         printf ("NULL\n");
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
