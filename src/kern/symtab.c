/* Symbol table manipulation.
 *
 * $Id: symtab.c,v 1.4 2004/11/18 04:14:33 chris Exp $
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

#include "basic_types.h"
#include "memory.h"
#include "symtab.h"

static unsigned int hash (const mstring_t *str, const subtable_t kind)
{
   unsigned int len = strlen ((char *) str);
   unsigned int i, h = kind;

   for (i = 0; i < len; i++)
      h += (int) str[i];

   return h % SYMTAB_ROWS;
}

/* +================================================================+
 * | FUNCTIONS FOR MANIPULATING A SINGLE SYMBOL TABLE               |
 * +================================================================+
 */

/* Create a new symbol table, initializing all its buckets to NULL.  Return
 * a pointer to the new table.
 */
symtab_t *symtab_new ()
{
   symtab_t *retval;
   unsigned int i;

   MALLOC (retval, sizeof (symtab_t))

   for (i = 0; i < SYMTAB_ROWS; i++)
      (*retval)[i] = NULL;

   return retval;
}

/* Add a new entry into the symbol table, returning success or not. */
int table_add_entry (symtab_t *symtab, symbol_t *sym)
{
   symtab_entry_t *tmp;
   unsigned int row = hash(sym->name, sym->kind);

   if (symtab == NULL)
      return 0;

   if (table_entry_exists (symtab, sym))
      return -1;
   
   if ((*symtab)[row] == NULL)
   {
      MALLOC ((*symtab)[row], sizeof (symtab_entry_t))
      MALLOC ((*symtab)[row]->symbol, sizeof (symbol_t))
      (*symtab)[row]->symbol->kind = sym->kind;
      (*symtab)[row]->symbol->name =
         (mstring_t *) wcsdup ((wchar_t *) sym->name);
      (*symtab)[row]->next = NULL;
   }
   else
   {
      tmp = (*symtab)[row];
      while (tmp->next != NULL)
         tmp = tmp->next;

      MALLOC (tmp->next, sizeof (symtab_entry_t))
      MALLOC (tmp->next->symbol, sizeof (symbol_t))
      tmp->next->symbol->kind = sym->kind;
      tmp->next->symbol->name = (mstring_t *) wcsdup ((wchar_t *) sym->name);
      tmp->next->next = NULL;
   }

   return 1;
}

void table_dump (symtab_t *symtab)
{
   symtab_entry_t *tmp;
   unsigned int i;

   for (i = 0; i < SYMTAB_ROWS; i++)
   {
      tmp = (*symtab)[i];
      
      if (tmp != NULL)
      {
         printf ("row %2d:\t", i);

         while (tmp != NULL)
         {
            printf ("%ls, ", (wchar_t *) tmp->symbol->name);
            tmp = tmp->next;
         }

         printf ("NULL\n");
      }
   }
}

/* Does the given string exist in the symbol table?  Return success or not. */
unsigned int table_entry_exists (symtab_t *symtab, symbol_t *sym)
{
   unsigned int row = hash(sym->name, sym->kind);
   symtab_entry_t *tmp;
   
   if (symtab == NULL || (*symtab)[row] == NULL)
      return 0;

   for (tmp = (*symtab)[row]; tmp != NULL; tmp = tmp->next)
      if (wcscmp ((wchar_t*) sym->name, (wchar_t *) tmp->symbol->name) == 0 &&
          sym->kind == tmp->symbol->kind)
         return 1;

   return 0;
}

/* +================================================================+
 * | FUNCTIONS FOR MANIPULATING A STACK OF SYMBOL TABLES            |
 * +================================================================+
 */

/* Enter a new level of scope by creating a new symbol table and putting it
 * on the top of the symbol table stack, ensuring it will be the first to be
 * referenced.  Returns a pointer to the new stack.
 */
tabstack_t *enter_scope (tabstack_t *tabstack)
{
   tabstack_t *tmp;

   if (tabstack == NULL)
   {
      MALLOC (tabstack, sizeof (tabstack_t))
      tabstack->symtab = symtab_new();
      tabstack->upper = NULL;
   }
   else
   {
      MALLOC (tmp, sizeof (tabstack_t))
      tmp->symtab = symtab_new();
      tmp->upper = tabstack;
      tabstack = tmp;
   }

   return tabstack;
}

/* Leave the innermost level of scope by popping the table off the top of the
 * stack and destroying it.  Returns a pointer to the new stack.
 */
tabstack_t *leave_scope (tabstack_t *tabstack)
{
   tabstack = tabstack->upper;
   return tabstack;
}

int symtab_add_entry (tabstack_t *tabstack, symbol_t *sym)
{
   return table_add_entry (tabstack->symtab, sym);
}

void symtab_dump (tabstack_t *tabstack)
{
   tabstack_t *tmp = tabstack;

   while (tmp != NULL)
   {
      table_dump (tmp->symtab);
      printf ("==================================================\n");
      tmp = tmp->upper;
   }
}

unsigned int symtab_entry_exists (tabstack_t *tabstack, symbol_t *sym)
{
   tabstack_t *tmp = tabstack;

   while (tmp != NULL)
   {
      if (table_entry_exists (tmp->symtab, sym) == 1)
         return 1;

      tmp = tmp->upper;
   }

   return 0;
}

unsigned int symtab_entry_exists_local (tabstack_t *tabstack, symbol_t *sym)
{
   return table_entry_exists (tabstack->symtab, sym);
}

/* vim: set tags=../tags: */
