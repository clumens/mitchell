/* Symbol table manipulation.
 *
 * $Id: symtab.c,v 1.12 2004/12/16 23:41:51 chris Exp $
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
#include "config.h"
#include "memory.h"
#include "symtab.h"

/* In general, I'm against big disgusting macros - and cpp in general.  However,
 * I'm making an exception here because the code is just so similar and I
 * don't feel like making it a function.
 */
#define COPY_ENTRY(dest, src) \
   do { \
      (dest)->symbol->kind = (src)->kind; \
      (dest)->symbol->name = wcsdup ((wchar_t*) (src)->name); \
      (dest)->next = NULL; \
      switch ((src)->kind) { \
         case SYM_MODULE: \
            (dest)->symbol->info.stack = (src)->info.stack; \
            break; \
         case SYM_FUNCTION: \
            break; \
         case SYM_TYPE: \
         case SYM_VALUE: \
            (dest)->symbol->info.ty = (src)->info.ty; \
            break; \
      } \
   } while (0)

static unsigned int hash (const mstring_t *str, const subtable_t kind)
{
   unsigned int len = strlen ((char *) str);
   unsigned int i;
   unsigned int h = (kind == SYM_VALUE ? SYM_FUNCTION : kind);

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

   MALLOC (retval, sizeof (symtab_t));

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

   if (table_entry_exists (symtab, sym->name, sym->kind) == 1)
      return -1;
   
   if ((*symtab)[row] == NULL)
   {
      MALLOC ((*symtab)[row], sizeof(symtab_entry_t));
      MALLOC ((*symtab)[row]->symbol, sizeof(symbol_t));

      (*symtab)[row]->symbol->kind = sym->kind;
      (*symtab)[row]->symbol->name = wcsdup (sym->name);
      (*symtab)[row]->next = NULL;

      switch (sym->kind) {
         case SYM_MODULE:
            (*symtab)[row]->symbol->info.stack = sym->info.stack;
            break;

         case SYM_FUNCTION:
            break;

         case SYM_TYPE:
         case SYM_VALUE:
            (*symtab)[row]->symbol->info.ty = sym->info.ty;
            break;
      }
   }
   else
   {
      /* Scan to the end of the row, since we're not being clever and just
       * appending new entries.
       */
      tmp = (*symtab)[row];
      while (tmp->next != NULL)
         tmp = tmp->next;

      MALLOC (tmp->next, sizeof(symtab_entry_t));
      MALLOC (tmp->next->symbol, sizeof(symbol_t));

      tmp->next->symbol->kind = sym->kind;
      tmp->next->symbol->name = wcsdup (sym->name);
      tmp->next->next = NULL;

      switch (sym->kind) {
         case SYM_MODULE:
            tmp->next->symbol->info.stack = sym->info.stack;
            break;

         case SYM_FUNCTION:
            break;

         case SYM_TYPE:
         case SYM_VALUE:
            tmp->next->symbol->info.ty = sym->info.ty;
            break;
      }
   }

   return 1;
}

/* Lookup an entry in the given symbol table. */
symbol_t *table_lookup_entry (symtab_t *symtab, mstring_t *name,
                              subtable_t kind)
{
   unsigned int row = hash (name, kind);
   symtab_entry_t *tmp;

   if (symtab == NULL || (*symtab)[row] == NULL)
      return NULL;

   for (tmp = (*symtab)[row] ; tmp != NULL ; tmp = tmp->next)
      if (wcscmp ((wchar_t *) name, (wchar_t *) tmp->symbol->name) == 0 &&
          kind == tmp->symbol->kind)
         return tmp->symbol;

   return NULL;
}

/* Does the given string exist in the symbol table?  Return success or not. */
unsigned int table_entry_exists (symtab_t *symtab, mstring_t *name,
                                 subtable_t kind)
{
   return table_lookup_entry (symtab, name, kind) == NULL ? 0 : 1;
}

/* Search for a symbol with the given name and kind.  If it exists, remove it
 * and replace with the symbol new.  If it does not exist, simply add new into
 * the symbol table.
 */
int table_update_entry (symtab_t *symtab, mstring_t *name, subtable_t kind,
                        symbol_t *newsym)
{
   if (table_entry_exists (symtab, name, kind) == 0)
      return table_add_entry (symtab, newsym);
   else
   {
      unsigned int row    = hash (name, kind);
      symtab_entry_t *cur = (*symtab)[row];

      /* Scan until we get to the right entry - we know the entry exists
       * somewhere since table_entry_exists told us so and there's no
       * threading-type problems in this program.
       */
      while (1)
      {
         if (wcscmp (name, cur->symbol->name) == 0 && kind == cur->symbol->kind)
            break;
         else
            cur = cur->next;
      }

      /* cur now points at the entry we need to revise.  Overwite its
       * contents with that of the new data.
       */
      switch (newsym->kind) {
         case SYM_MODULE:
            cur->symbol->info.stack = newsym->info.stack;
            break;

         case SYM_FUNCTION:
            break;

         case SYM_TYPE:
         case SYM_VALUE:
            cur->symbol->info.ty = newsym->info.ty;
            break;
      }
   }

   return 1;
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
      MALLOC (tabstack, sizeof (tabstack_t));
      tabstack->symtab = symtab_new();
      tabstack->upper = NULL;
   }
   else
   {
      MALLOC (tmp, sizeof (tabstack_t));
      tmp->symtab = symtab_new();
      tmp->upper = tabstack;
      tabstack = tmp;
   }

   return tabstack;
}

/* Leave the innermost level of scope by popping the table off the top of the
 * stack and destroying it.  Returns a pointer to the new stack.
 */
tabstack_t *leave_scope (tabstack_t *tabstack, mstring_t *scope_name)
{
   if (compiler_config.debug.dump_symtabs)
      table_dump (tabstack->symtab, scope_name);

   tabstack = tabstack->upper;
   return tabstack;
}

int symtab_add_entry (tabstack_t *tabstack, symbol_t *sym)
{
   return table_add_entry (tabstack->symtab, sym);
}

symbol_t *symtab_lookup_entry (tabstack_t *tabstack, mstring_t *name,
                               subtable_t kind)
{
   tabstack_t *tmp = tabstack;
   symbol_t *retval;

   while (tmp != NULL)
   {
      retval = table_lookup_entry  (tmp->symtab, name, kind);
      if (retval != NULL)
         return retval;

      tmp = tmp->upper;
   }

   return NULL;
}

unsigned int symtab_entry_exists (tabstack_t *tabstack, mstring_t *name,
                                  subtable_t kind)
{
   return (symtab_lookup_entry (tabstack, name, kind) == NULL) ?
      0 : 1;
}

unsigned int symtab_entry_exists_local (tabstack_t *tabstack, mstring_t *name,
                                        subtable_t kind)
{
   return table_entry_exists (tabstack->symtab, name, kind);
}

/* vim: set tags=../tags: */
